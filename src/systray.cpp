#include "systray.h"
#include "application.h"
#include "config.h"
#include "icons.h"
#include "main.h"
#include "simple_dbus.h"
#include "taskbar.h"
#include <xcb/xcb.h>

#ifdef TRACY_ENABLE

#include "../tracy/public/tracy/Tracy.hpp"

#endif

#include <iostream>
#include <array>
#include <filesystem>
#include <memory>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility.h>
#include <xcb/xcb_event.h>
#include "dbus_helper.h"

#define SYSTEM_TRAY_REQUEST_DOCK 0

static AppClient *systray = nullptr;
static AppClient *display = nullptr;
static uint32_t icon_size = 22;
static uint32_t container_size = 40;
static bool layout_invalid = true;

static void
layout_systray();

static void
display_close();

class Systray_Icon {
public:
    xcb_window_t window;
    bool reparented = false;
    int times = 0;
};

std::vector<Systray_Icon *> systray_icons;

struct CairoSurfaceDeleter {
    void operator()(cairo_surface_t *surface) const {
        if (surface) {
            cairo_surface_destroy(surface);
        }
    }
};

using CairoSurfacePtr = std::unique_ptr<cairo_surface_t, CairoSurfaceDeleter>;

class StatusNotifierIcon {
public:
    std::string service;
    std::string path = "/StatusNotifierItem";
    std::string_view iface = "org.kde.StatusNotifierItem";
    std::string icon_name;
    std::string icon_path;
    CairoSurfacePtr icon_pixmap_surface;
    std::unique_ptr<gl_surface> icon_pixmap_gsurf;
    Bounds cell_bounds = Bounds(0, 0, 0, 0);
    long last_property_update = 0;

    StatusNotifierIcon() = default;
    StatusNotifierIcon(StatusNotifierIcon &&) noexcept = default;
    StatusNotifierIcon &operator=(StatusNotifierIcon &&) noexcept = default;
    StatusNotifierIcon(const StatusNotifierIcon &) = delete;
    StatusNotifierIcon &operator=(const StatusNotifierIcon &) = delete;

    [[nodiscard]] std::string key() const {
        return service + "|" + path;
    }
};

static std::vector<StatusNotifierIcon> status_notifier_icons;

static constexpr std::array<std::string_view, 2> status_notifier_item_ifaces = {
        "org.kde.StatusNotifierItem",
        "org.freedesktop.StatusNotifierItem",
};

static constexpr std::array<std::pair<std::string_view, std::string_view>, 4> status_notifier_watcher_ifaces = {{
        {"org.kde.StatusNotifierWatcher", "org.kde.StatusNotifierWatcher"},
        {"org.kde.StatusNotifierWatcher", "org.freedesktop.StatusNotifierWatcher"},
        {"org.freedesktop.StatusNotifierWatcher", "org.freedesktop.StatusNotifierWatcher"},
        {"org.freedesktop.StatusNotifierWatcher", "org.kde.StatusNotifierWatcher"},
}};

static bool
session_get_property_any(std::string_view bus_name,
                         std::string_view path,
                         std::string_view iface,
                         std::string_view property_name,
                         std::any &out_value) {
    if (!dbus_connection_session) {
        return false;
    }

    DBusMessage *get_msg = dbus_message_new_method_call(bus_name.data(),
                                                        path.data(),
                                                        "org.freedesktop.DBus.Properties",
                                                        "Get");
    if (!get_msg) {
        return false;
    }

    DBusMessageIter iter;
    dbus_message_iter_init_append(get_msg, &iter);
    const char *iface_name = iface.data();
    const char *property = property_name.data();
    dbus_message_iter_append_basic(&iter, DBUS_TYPE_STRING, &iface_name);
    dbus_message_iter_append_basic(&iter, DBUS_TYPE_STRING, &property);

    DBusError error;
    dbus_error_init(&error);
    DBusMessage *reply = dbus_connection_send_with_reply_and_block(dbus_connection_session, get_msg, 60, &error);
    dbus_message_unref(get_msg);
    if (dbus_error_is_set(&error)) {
        dbus_error_free(&error);
        if (reply) {
            dbus_message_unref(reply);
        }
        return false;
    }

    Msg msg{reply};
    out_value = parse_message(msg.msg);
    return out_value.has_value();
}

static bool
session_get_property_string(std::string_view bus_name,
                            std::string_view path,
                            std::string_view iface,
                            std::string_view property_name,
                            std::string &out_value) {
    std::any any;
    if (!session_get_property_any(bus_name, path, iface, property_name, any)) {
        return false;
    }
    if (auto str = std::any_cast<std::string>(&any)) {
        out_value = *str;
    } else {
        out_value = "";
    }
    return true;
}

static bool
any_to_int(const std::any &value, int &out) {
    if (auto v = std::any_cast<int32_t>(&value)) {
        out = *v;
        return true;
    }
    if (auto v = std::any_cast<uint32_t>(&value)) {
        out = (int) *v;
        return true;
    }
    if (auto v = std::any_cast<int64_t>(&value)) {
        out = (int) *v;
        return true;
    }
    if (auto v = std::any_cast<uint64_t>(&value)) {
        out = (int) *v;
        return true;
    }
    return false;
}

static bool
any_to_byte_array(const std::any &value, std::vector<uint8_t> &out) {
    auto arr = std::any_cast<DbusArray>(&value);
    if (!arr) {
        return false;
    }
    out.clear();
    out.reserve(arr->elements.size());
    for (const auto &element: arr->elements) {
        if (auto b = std::any_cast<uint8_t>(&element)) {
            out.push_back(*b);
        } else if (auto n = std::any_cast<int32_t>(&element)) {
            out.push_back((uint8_t) (*n & 0xFF));
        } else if (auto n = std::any_cast<uint32_t>(&element)) {
            out.push_back((uint8_t) (*n & 0xFF));
        } else {
            return false;
        }
    }
    return true;
}

struct PixmapCandidate {
    int w = 0;
    int h = 0;
    std::vector<uint8_t> bytes;
};

static CairoSurfacePtr
surface_from_icon_pixmap(const PixmapCandidate &pixmap) {
    if (pixmap.w <= 0 || pixmap.h <= 0) {
        return nullptr;
    }
    if (pixmap.bytes.size() < (size_t) (pixmap.w * pixmap.h * 4)) {
        return nullptr;
    }

    cairo_surface_t *surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, pixmap.w, pixmap.h);
    if (!surface || cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
        if (surface) {
            cairo_surface_destroy(surface);
        }
        return nullptr;
    }
    CairoSurfacePtr result(surface);

    unsigned char *dst = cairo_image_surface_get_data(surface);
    int stride = cairo_image_surface_get_stride(surface);
    for (int y = 0; y < pixmap.h; y++) {
        unsigned char *row = dst + (y * stride);
        for (int x = 0; x < pixmap.w; x++) {
            size_t i = (size_t) (y * pixmap.w + x) * 4;
            uint8_t a = pixmap.bytes[i + 0];
            uint8_t r = pixmap.bytes[i + 1];
            uint8_t g = pixmap.bytes[i + 2];
            uint8_t b = pixmap.bytes[i + 3];

            row[x * 4 + 0] = (uint8_t) ((b * a + 127) / 255);
            row[x * 4 + 1] = (uint8_t) ((g * a + 127) / 255);
            row[x * 4 + 2] = (uint8_t) ((r * a + 127) / 255);
            row[x * 4 + 3] = a;
        }
    }
    cairo_surface_mark_dirty(surface);
    return result;
}

static CairoSurfacePtr
best_icon_pixmap_surface(const std::any &property_any) {
    auto pixmaps = std::any_cast<DbusArray>(&property_any);
    if (!pixmaps) {
        return nullptr;
    }

    std::vector<PixmapCandidate> candidates;
    for (const auto &element: pixmaps->elements) {
        auto s = std::any_cast<DbusStruct>(&element);
        if (!s || s->members.size() < 3) {
            continue;
        }
        PixmapCandidate candidate;
        if (!any_to_int(s->members[0], candidate.w) ||
            !any_to_int(s->members[1], candidate.h) ||
            !any_to_byte_array(s->members[2], candidate.bytes)) {
            continue;
        }
        if (candidate.w <= 0 || candidate.h <= 0) {
            continue;
        }
        if (candidate.bytes.size() < (size_t) (candidate.w * candidate.h * 4)) {
            continue;
        }
        candidates.push_back(std::move(candidate));
    }
    if (candidates.empty()) {
        return nullptr;
    }

    int best_above = -1;
    int best_below = -1;
    for (int i = 0; i < candidates.size(); i++) {
        int side = std::max(candidates[i].w, candidates[i].h);
        if (side >= (int) icon_size) {
            if (best_above == -1 ||
                std::max(candidates[i].w, candidates[i].h) <
                        std::max(candidates[best_above].w, candidates[best_above].h)) {
                best_above = i;
            }
        } else {
            if (best_below == -1 ||
                std::max(candidates[i].w, candidates[i].h) >
                        std::max(candidates[best_below].w, candidates[best_below].h)) {
                best_below = i;
            }
        }
    }
    int best_index = best_above != -1 ? best_above : best_below;
    if (best_index == -1) {
        return nullptr;
    }
    return surface_from_icon_pixmap(candidates[best_index]);
}

static bool
session_get_property_array_strings(std::string_view bus_name,
                                   std::string_view path,
                                   std::string_view iface,
                                   std::string_view property_name,
                                   std::vector<std::string> &out_values) {
    std::any any;
    if (!session_get_property_any(bus_name, path, iface, property_name, any)) {
        return false;
    }
    if (auto arr = std::any_cast<DbusArray>(&any)) {
        for (const auto &element: arr->elements) {
            if (auto str = std::any_cast<std::string>(&element)) {
                out_values.push_back(*str);
            }
        }
        return true;
    }
    return false;
}

static bool
parse_status_notifier_item_name(std::string_view name, StatusNotifierIcon &icon) {
    if (name.empty()) {
        return false;
    }
    if (name[0] == '/') {
        return false;
    }

    auto slash = name.find('/');
    if (slash == std::string::npos) {
        icon.service = std::string(name);
        icon.path = "/StatusNotifierItem";
    } else {
        icon.service = std::string(name.substr(0, slash));
        icon.path = std::string(name.substr(slash));
        if (icon.path.empty()) {
            icon.path = "/StatusNotifierItem";
        }
    }
    return !icon.service.empty();
}

static std::string
resolve_icon_path(std::string_view icon_name_view) {
    std::string icon_name(icon_name_view);
    if (icon_name.empty()) {
        return "";
    }
    if (icon_name[0] == '/' && std::filesystem::exists(icon_name)) {
        return icon_name;
    }

    if (!has_options(icon_name)) {
        auto pos = icon_name.find("-symbolic");
        if (pos != std::string::npos) {
            icon_name = icon_name.substr(0, pos);
        }
    }
    if (!has_options(icon_name)) {
        return "";
    }

    std::vector<IconTarget> targets;
    targets.emplace_back(icon_name);
    search_icons(targets);
    pick_best(targets, icon_size);
    if (targets.empty()) {
        return "";
    }
    return targets[0].best_full_path;
}

static bool
populate_status_notifier_item(StatusNotifierIcon &icon) {
    for (const auto iface: status_notifier_item_ifaces) {
        std::string item_id;
        bool got_id = session_get_property_string(icon.service, icon.path, iface, "Id", item_id);
        bool got_icon_name = session_get_property_string(icon.service, icon.path, iface, "IconName", icon.icon_name);

        icon.iface = iface;
        icon.icon_path.clear();
        icon.icon_pixmap_surface = nullptr;
        icon.icon_pixmap_gsurf.reset();

        if (got_icon_name && !icon.icon_name.empty()) {
            icon.icon_path = resolve_icon_path(icon.icon_name);
            if (icon.icon_path.empty()) {
                std::string without_symbolic = icon.icon_name;
                auto pos = without_symbolic.find("-symbolic");
                if (pos != std::string::npos) {
                    without_symbolic = without_symbolic.substr(0, pos);
                    icon.icon_path = resolve_icon_path(without_symbolic);
                }
            }
        }

        bool has_pixmap_prop = false;
        if (icon.icon_path.empty()) {
            std::any pixmap_any;
            has_pixmap_prop = session_get_property_any(icon.service, icon.path, iface, "IconPixmap", pixmap_any);
            if (has_pixmap_prop) {
                icon.icon_pixmap_surface = best_icon_pixmap_surface(pixmap_any);
                if (icon.icon_pixmap_surface) {
                    icon.icon_pixmap_gsurf = std::make_unique<gl_surface>();
                }
            }
        }

        if (icon.icon_path.empty() && !icon.icon_pixmap_surface) {
            std::string fallback_name = got_id && !item_id.empty() ? item_id : icon.service;
            auto slash = fallback_name.rfind('/');
            if (slash != std::string::npos && slash + 1 < fallback_name.size()) {
                fallback_name = fallback_name.substr(slash + 1);
            }
            auto dot = fallback_name.rfind('.');
            if (dot != std::string::npos && dot + 1 < fallback_name.size()) {
                fallback_name = fallback_name.substr(dot + 1);
            }
            icon.icon_path = resolve_icon_path(fallback_name);
        }

        if (got_icon_name || has_pixmap_prop || !icon.icon_path.empty() || icon.icon_pixmap_surface) {
            icon.last_property_update = app->current;
            return true;
        }
    }
    return false;
}

static void
refresh_status_notifier_items() {
    std::vector<std::string> registered_items;

    bool queried = false;
    for (const auto watcher: status_notifier_watcher_ifaces) {
        std::vector<std::string> items;
        if (session_get_property_array_strings(watcher.first,
                                               "/StatusNotifierWatcher",
                                               watcher.second,
                                               "RegisteredStatusNotifierItems",
                                               items)) {
            registered_items = std::move(items);
            queried = true;
            break;
        }
    }

    if (!queried) {
        status_notifier_icons.clear();
        return;
    }

    std::unordered_map<std::string, size_t> previous_indices;
    previous_indices.reserve(status_notifier_icons.size());
    for (size_t i = 0; i < status_notifier_icons.size(); i++) {
        previous_indices[status_notifier_icons[i].key()] = i;
    }
    auto previous = std::move(status_notifier_icons);

    std::unordered_set<std::string> seen;
    std::vector<StatusNotifierIcon> refreshed;
    refreshed.reserve(registered_items.size());
    for (const auto &item_name: registered_items) {
        StatusNotifierIcon icon;
        if (!parse_status_notifier_item_name(item_name, icon)) {
            continue;
        }
        auto key = icon.key();
        if (seen.find(key) != seen.end()) {
            continue;
        }
        seen.insert(key);

        auto previous_it = previous_indices.find(key);
        if (previous_it != previous_indices.end() &&
            app->current - previous[previous_it->second].last_property_update < 5000) {
            refreshed.push_back(std::move(previous[previous_it->second]));
            continue;
        }

        if (populate_status_notifier_item(icon)) {
            refreshed.push_back(std::move(icon));
        }
    }
    status_notifier_icons = std::move(refreshed);
}

static bool
call_status_notifier_method(const StatusNotifierIcon &icon, const char *method, int32_t x, int32_t y) {
    if (!dbus_connection_session) {
        return false;
    }

    std::string iface(icon.iface);
    DBusMessage *msg = dbus_message_new_method_call(icon.service.c_str(),
                                                    icon.path.c_str(),
                                                    iface.c_str(),
                                                    method);
    if (!msg) {
        return false;
    }

    DBusMessageIter iter;
    dbus_message_iter_init_append(msg, &iter);
    dbus_message_iter_append_basic(&iter, DBUS_TYPE_INT32, &x);
    dbus_message_iter_append_basic(&iter, DBUS_TYPE_INT32, &y);

    DBusError error;
    dbus_error_init(&error);
    DBusMessage *reply = dbus_connection_send_with_reply_and_block(dbus_connection_session, msg, 120, &error);
    dbus_message_unref(msg);
    if (reply) {
        dbus_message_unref(reply);
    }
    if (dbus_error_is_set(&error)) {
        dbus_error_free(&error);
        return false;
    }
    return true;
}

static void
clicked_display(AppClient *client, cairo_t *, Container *container) {
    int x = client->mouse_current_x;
    int y = client->mouse_current_y;
    for (const auto &icon: status_notifier_icons) {
        if (!bounds_contains(icon.cell_bounds, x, y)) {
            continue;
        }

        int32_t center_x = client->bounds->x + icon.cell_bounds.x + icon.cell_bounds.w / 2;
        int32_t center_y = client->bounds->y + icon.cell_bounds.y + icon.cell_bounds.h / 2;
        if (container->state.mouse_button_pressed == XCB_BUTTON_INDEX_3) {
            // some SNI clients place the menu top left at this point and dont clamp well
            // use pointer position and bias upward if its too close to the bottom edge
            int32_t menu_x = client->bounds->x + x;
            int32_t menu_y = client->bounds->y + y;

            int32_t min_x = 0;
            int32_t min_y = 0;
            int32_t max_x = app->bounds.w - 1;
            int32_t max_y = app->bounds.h - 1;
            if (client->screen_information) {
                min_x = client->screen_information->x;
                min_y = client->screen_information->y;
                max_x = min_x + client->screen_information->width_in_pixels - 1;
                max_y = min_y + client->screen_information->height_in_pixels - 1;
            }

            if (menu_x < min_x) 
                menu_x = min_x;
            if (menu_x > max_x) 
                menu_x = max_x;
            if (menu_y < min_y)
                menu_y = min_y;
            if (menu_y > max_y) 
                menu_y = max_y;

            int desired_room_below = (int) (320 * client->dpi());
            int room_below = max_y - menu_y;
            if (room_below < desired_room_below) {
                menu_y -= (desired_room_below - room_below);
                if (menu_y < min_y)
                    menu_y = min_y;
            }

            call_status_notifier_method(icon, "ContextMenu", menu_x, menu_y);
        } else if (container->state.mouse_button_pressed == XCB_BUTTON_INDEX_2) {
            call_status_notifier_method(icon, "SecondaryActivate", center_x, center_y);
        } else {
            call_status_notifier_method(icon, "Activate", center_x, center_y);
        }
        break;
    }
}

unsigned long
create_rgba(int r, int g, int b, int a) {
    return ((a & 0xff) << 24) + ((r & 0xff) << 16) + ((g & 0xff) << 8) + (b & 0xff);
}

static void
paint_display(AppClient *client, cairo_t *cr, Container *container) {
#ifdef TRACY_ENABLE
    ZoneScoped;
#endif
    ArgbColor bg_color = correct_opaqueness(client, config->color_systray_background);
    
    for (int i = 0; i < systray_icons.size(); i++) {
        Systray_Icon *icon = systray_icons[i];
        int r = (int) (bg_color.r * 255);
        int g = (int) (bg_color.g * 255);
        int b = (int) (bg_color.b * 255);
        int a = (int) (bg_color.a * 255);
        uint32_t rgb = create_rgba(r, g, b, a);
        uint32_t value_list[] = {rgb};
        xcb_change_window_attributes(app->connection, icon->window, XCB_CW_BACK_PIXEL, value_list);
    }
    
    if (layout_invalid) {
        layout_systray();
    }
    
    for (auto icon: systray_icons) {
        xcb_map_window(app->connection, icon->window);
    }
    
    xcb_map_window(app->connection, client->window);
    xcb_map_subwindows(app->connection, client->window);
    
    xcb_flush(app->connection);
    
    // paint the background
    draw_colored_rect(client, bg_color, container->real_bounds);

    for (const auto &icon: status_notifier_icons) {
        if (!icon.icon_path.empty()) {
            load_and_paint(app, client, icon.icon_path, icon_size,
                           icon.cell_bounds.x + icon.cell_bounds.w / 2 - icon_size / 2,
                           icon.cell_bounds.y + icon.cell_bounds.h / 2 - icon_size / 2);
        } else if (icon.icon_pixmap_surface && icon.icon_pixmap_gsurf) {
            draw_gl_texture(client, icon.icon_pixmap_gsurf.get(), icon.icon_pixmap_surface.get(),
                            icon.cell_bounds.x + icon.cell_bounds.w / 2 - icon_size / 2,
                            icon.cell_bounds.y + icon.cell_bounds.h / 2 - icon_size / 2,
                            icon_size, icon_size);
        } else {
            draw_text(client, 10 * config->dpi, config->icons, EXPAND(config->color_taskbar_button_icons),
                      "\uE946", icon.cell_bounds);
        }
    }
}

static bool
systray_event_handler(App *app, xcb_generic_event_t *event, xcb_window_t) {
#ifdef TRACY_ENABLE
    ZoneScoped;
#endif
    switch (XCB_EVENT_RESPONSE_TYPE(event)) {
        case XCB_CLIENT_MESSAGE: {
            auto *client_message = (xcb_client_message_event_t *) event;
            
            if (client_message->type == get_cached_atom(app, "_NET_SYSTEM_TRAY_OPCODE")) {
                if (client_message->data.data32[1] == SYSTEM_TRAY_REQUEST_DOCK) {
                    auto window_to_be_docked = client_message->data.data32[2];
                    
                    for (auto icon: systray_icons)
                        if (icon->window == window_to_be_docked)
                            break;
                    
                    const uint32_t cw_values[] = {
                            XCB_EVENT_MASK_ENTER_WINDOW | XCB_EVENT_MASK_STRUCTURE_NOTIFY |
                            XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT |
                            XCB_EVENT_MASK_PROPERTY_CHANGE};
                    xcb_change_window_attributes(app->connection, window_to_be_docked,
                                                 XCB_CW_EVENT_MASK, cw_values);
                    xcb_change_save_set(app->connection, XCB_SET_MODE_INSERT, window_to_be_docked);
                    
                    auto icon = new Systray_Icon;
                    icon->window = window_to_be_docked;
//                    printf("ADD: %d\n", icon->window);
                    systray_icons.push_back(icon);
                    
                    // if (display) {
                    //     xcb_reparent_window(app->connection, icon->window, display->window, -512, -512);
                    //     icon->reparented = true;
                    // } else if (systray) {
                    //     xcb_reparent_window(app->connection, icon->window, systray->window, 0, 0);
                    //     icon->reparented = true;
                    // }
                    xcb_unmap_window(app->connection, icon->window);
                    
                    layout_invalid = true;
                }
            }
        }
            break;
    }
    
    return false;
}

// The XEmbed Protocol says that when you re-parent a window into your window
// You should basically act in the way that a windows manager acts a.k.a
// Selecting the SubstrucreRedirectMask and intercepting events on it like mapping/unmapping
// configuring and so on. So that's what we do in this icon_event_handler
static bool
icon_event_handler(App *app, xcb_generic_event_t *generic_event, xcb_window_t) {
#ifdef TRACY_ENABLE
    ZoneScoped;
#endif
    // Since this function looks at every single xcb event generated
    // we first need to filter out windows that are not clients (icons) we are handling
    xcb_window_t event_window = get_window(generic_event);
    
    bool window_is_systray_icon = false;
    for (auto icon: systray_icons)
        if (icon->window == event_window)
            window_is_systray_icon = true;
    if (!window_is_systray_icon)
        return false;// Let someone else handle the event
    
//    printf("%d - %d\n", event_window, generic_event->response_type);
    
    switch (XCB_EVENT_RESPONSE_TYPE(generic_event)) {
        case XCB_DESTROY_NOTIFY: {
//            printf("DESTROY: %d\n", event_window);
            
            for (int i = 0; i < systray_icons.size(); i++) {
                auto icon = systray_icons[i];
                if (icon->window == event_window) {
                    systray_icons.erase(systray_icons.begin() + i);
                    delete icon;
                }
            }
            layout_invalid = true;
            
            if (systray_icons.empty() && status_notifier_icons.empty()) {
                display_close();
            }
            break;
        }
        case XCB_CONFIGURE_NOTIFY: {
            
            break;
        }
        case XCB_PROPERTY_NOTIFY: {
            
            break;
        }
        case XCB_MAP_NOTIFY: {
            
            break;
        }
    }
    layout_invalid = true;
    
    return true;
}

static int
closest_square_root_above(int target) {
    int i = 0;
    while (true && i < 100) {
        if (i * i >= target)
            return i;
        i++;
    }
    return 0;
}

static void
layout_systray() {
#ifdef TRACY_ENABLE
    ZoneScoped;
#endif
    // If this looks funky, it's because systray icons are laid out wierdly
    int total_icons = systray_icons.size() + status_notifier_icons.size();
    int x = 0;
    int y = 0;
    int w = closest_square_root_above(total_icons);
    if (w == 0) {// even if the systray has no icons we want to show a 1x1
        w = 1;
    } else if (w > 4) {// after we reach a width of 4 icons we just want to grow upwards
        w = 4;
    }
    
    // This part puts the icon in the correct location at the correct size
    for (int i = 0; i < total_icons; i++) {
        if (x == w) {
            x = 0;
            y++;
        }

        int cell_x = x * container_size;
        int cell_y = y * container_size;
        if (i < systray_icons.size()) {
            Systray_Icon *icon = systray_icons[i];

            uint32_t value_mask = XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y | XCB_CONFIG_WINDOW_WIDTH |
                                  XCB_CONFIG_WINDOW_HEIGHT;
            uint32_t value_list_resize[] = {
                    (uint32_t) (cell_x + container_size / 2 - icon_size / 2),
                    (uint32_t) (cell_y + container_size / 2 - icon_size / 2),
                    icon_size,
                    icon_size};
            xcb_configure_window(app->connection, icon->window, value_mask, value_list_resize);
            if (display && !systray_icons[i]->reparented && systray_icons[i]->times++) {
                xcb_reparent_window(app->connection, systray_icons[i]->window, display->window,
                                    value_list_resize[0], value_list_resize[1]);
                if (systray_icons[i]->times > 10) {
                    systray_icons[i]->reparented = true;
                }
            }
        } else {
            auto &icon = status_notifier_icons[i - systray_icons.size()];
            icon.cell_bounds = Bounds(cell_x, cell_y, container_size, container_size);
        }
        x++;
    }
    
    // If the display window (which holds our icon windows) is open, we move and resize it to the
    // correct spot
    if (display) {
        layout_invalid = false;
        
        auto window_width = (uint32_t) container_size * w;
        auto window_height = (uint32_t) container_size * ++y;
        
        int window_x = 0;
        auto window_y = (uint32_t) (app->bounds.h - config->taskbar_height - window_height);
        
        if (auto taskbar = client_by_name(app, "taskbar")) {
            window_x = taskbar->bounds->x;
            window_y = taskbar->bounds->y - window_height;
            if (auto container = container_by_name("systray", taskbar->root)) {
                window_x += container->real_bounds.x + container->real_bounds.w / 2 - window_width / 2;
                if (!container->exists)
                    window_x = 0;
                if (window_x + window_width >= taskbar->screen_information->width_in_pixels)
                    window_x = taskbar->screen_information->width_in_pixels - window_width;
            }
        }
        if (window_x <= 0)
            window_x = 0;
        
        uint32_t value_mask = XCB_CONFIG_WINDOW_X | XCB_CONFIG_WINDOW_Y |
                              XCB_CONFIG_WINDOW_WIDTH | XCB_CONFIG_WINDOW_HEIGHT;
        uint32_t value_list_resize[] = {(uint32_t) window_x, window_y, window_width, window_height};
        xcb_configure_window(app->connection, display->window, value_mask, value_list_resize);
    }
}

static void
on_display_closed(AppClient *) {
    if (systray)
        for (auto icon: systray_icons) {
            xcb_reparent_window(app->connection, icon->window, systray->window, 0, 0);
            icon->reparented = false;
            icon->times = 0;
        }
    status_notifier_icons.clear();
    display = nullptr;
}

static void
on_systray_closed(AppClient *) {
    if (systray)
        for (auto icon: systray_icons) {
            xcb_reparent_window(app->connection, icon->window, app->screen->root, 0, 0);
            icon->reparented = false;
            icon->times = 0;
        }
}

void register_as_systray() {
#ifdef TRACY_ENABLE
    ZoneScoped;
#endif
    Settings settings;
    settings.window_transparent = false;
    settings.background = argb_to_color(config->color_systray_background);
    systray = client_new(app, settings, "systray");
    systray->keeps_app_running = false;
    systray->when_closed = on_systray_closed;
    
    std::string selection = "_NET_SYSTEM_TRAY_S";
    selection.append(std::to_string(app->screen_number));
    auto tray_atom = get_cached_atom(app, selection.c_str());
    xcb_set_selection_owner(app->connection, systray->window, tray_atom, XCB_CURRENT_TIME);
    auto selection_owner_cookie = xcb_get_selection_owner(app->connection, tray_atom);
    auto *selection_owner_reply =
            xcb_get_selection_owner_reply(app->connection, selection_owner_cookie, NULL);
    if (selection_owner_reply->owner != systray->window) {
        client_close_threaded(app, systray);
        systray = nullptr;
        free(selection_owner_reply);
        return;
    }
    free(selection_owner_reply);
    
    layout_invalid = true;
    for (auto a: systray_icons)
        delete a;
    systray_icons.clear();
    systray_icons.shrink_to_fit();
    
    app_create_custom_event_handler(app, INT_MAX, icon_event_handler);
    app_create_custom_event_handler(app, systray->window, systray_event_handler);
    
    xcb_client_message_event_t ev;
    ev.response_type = XCB_CLIENT_MESSAGE;
    ev.window = app->screen->root;
    ev.format = 32;
    ev.type = get_cached_atom(app, "MANAGER");
    ev.data.data32[0] = 0;
    ev.data.data32[1] = tray_atom;
    ev.data.data32[2] = systray->window;
    ev.data.data32[3] = ev.data.data32[4] = 0;
    
    xcb_send_event_checked(app->connection, false, app->screen->root, 0xFFFFFF, (char *) &ev);
    xcb_flush(app->connection);
}

void open_systray() {
#ifdef TRACY_ENABLE
    ZoneScoped;
#endif
    icon_size = 22 * config->dpi;
    container_size = 40 * config->dpi;
    refresh_status_notifier_items();
    
    Settings settings;
    // Very important that the window is not 32 bit depth because you can't embed non transparent windows into transparent ones
    settings.window_transparent = false;
    settings.skip_taskbar = true;
    settings.decorations = false;
    settings.force_position = true;
    settings.w = 1;
    settings.h = 1;
    settings.x = -settings.w * 2;
    settings.y = -settings.h * 2;
    settings.no_input_focus = false;
    settings.override_redirect = true;
    if (app->wayland)
        settings.override_redirect = false;
    settings.background = argb_to_color(config->color_systray_background);
    
    if (auto taskbar = client_by_name(app, "taskbar")) {
        PopupSettings popup_settings;
        popup_settings.name = "display";
        display = taskbar->create_popup(popup_settings, settings);
        
        display->root->when_clicked = clicked_display;
        display->root->when_paint = paint_display;
        display->when_closed = on_display_closed;
        client_show(app, display);
        static int times = 0;
        times = 0;
        app_timeout_create(taskbar->app, display, 50, [](App *, AppClient *client, Timeout *t, void *) {
            t->keep_running = times++ < 10;
            layout_systray();
            request_refresh(app, client);
        }, nullptr, "systray relayout");

        app_timeout_create(taskbar->app, display, 1000, [](App *app, AppClient *client, Timeout *t, void *) {
            if (!display || !valid_client(app, display)) {
                t->keep_running = false;
                return;
            }
            t->keep_running = true;
            refresh_status_notifier_items();
            layout_systray();
            request_refresh(app, client);
        }, nullptr, "systray sni refresh");
        
        layout_systray();
        
        layout_invalid = true;
    }
}

void display_close_timeout(App *app, AppClient *, Timeout *, void *) {
    if (display) {
        client_close_threaded(app, display);
        display = nullptr;
    }
}

static void
display_close() {
#ifdef TRACY_ENABLE
    ZoneScoped;
#endif
    app->grab_window = 0;
    xcb_ungrab_button(app->connection, XCB_BUTTON_INDEX_ANY, app->screen->root, XCB_MOD_MASK_ANY);
    app_timeout_create(app, nullptr, 100, display_close_timeout, nullptr, const_cast<char *>(__PRETTY_FUNCTION__));
}
