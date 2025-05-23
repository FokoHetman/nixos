import { NotificationPopups } from "./notificationPopups.js"
import { applauncher } from "./applauncher.js"

Utils.timeout(100, () => Utils.notify({
    summary: "Notification Popup Example",
    iconName: "info-symbolic",
    body: "Lorem ipsum dolor sit amet, qui minim labore adipisicing "
        + "minim sint cillum sint consectetur cupidatat.",
    actions: {
        "Cool": () => print("pressed Cool"),
    },
}))


const global_spacing = 8;




const hyprland = await Service.import("hyprland")
const notifications = await Service.import("notifications")
const mpris = await Service.import("mpris")
const audio = await Service.import("audio")
const battery = await Service.import("battery")
const systemtray = await Service.import("systemtray")

const date = Variable("", {
    poll: [1000, 'date "+%H:%M:%S %b %e."'],
})

// widgets can be only assigned as a child in one container
// so to make a reuseable widget, make it a function
// then you can simply instantiate one by calling it

function Workspaces() {
    const activeId = hyprland.active.workspace.bind("id")
    const workspaces = hyprland.bind("workspaces")
        .as(ws => ws.map(({ id }) => Widget.Button({
            on_clicked: () => hyprland.messageAsync(`dispatch workspace ${id}`),
            child: Widget.Label(`${id}`),
            class_name: activeId.as(i => `${i === id ? "focused" : ""}`),
        })))

    return Widget.Box({
        class_name: "workspaces",
        children: workspaces,
    })
}


function ClientTitle() {
    return Widget.Label({
        class_name: "client-title",
        label: hyprland.active.client.bind("title"),
    })
}


function Clock() {
    return Widget.Label({
        class_name: "clock",
        label: date.bind(),
    })
}


// we don't need dunst or any other notification daemon
// because the Notifications module is a notification daemon itself
function Notification() {
    const popups = notifications.bind("popups")
    return Widget.Box({
        class_name: "notification",
        visible: popups.as(p => p.length > 0),
        children: [
            Widget.Icon({
                icon: "preferences-system-notifications-symbolic",
            }),
            Widget.Label({
                label: popups.as(p => p[0]?.summary || ""),
            }),
        ],
    })
}


function Media() {
    const label = Utils.watch("", mpris, "player-changed", () => {
        if (mpris.players[0]) {
            const { track_artists, track_title } = mpris.players[0]
            return `${track_artists.join(", ")} - ${track_title}`
        } else {
            return "Nothing is playing"
        }
    })

    return Widget.Button({
        class_name: "media",
        on_primary_click: () => mpris.getPlayer("")?.playPause(),
        on_scroll_up: () => mpris.getPlayer("")?.next(),
        on_scroll_down: () => mpris.getPlayer("")?.previous(),
        child: Widget.Label({ label }),
    })
}


function Volume() {
    const icons = {
        101: "overamplified",
        67: "high",
        34: "medium",
        1: "low",
        0: "muted",
    }

    function getIcon() {
        const icon = audio.speaker.is_muted ? 0 : [101, 67, 34, 1, 0].find(
            threshold => threshold <= audio.speaker.volume * 100)

        return `audio-volume-${icons[icon]}-symbolic`
    }

    const icon = Widget.Icon({
        icon: Utils.watch(getIcon(), audio.speaker, getIcon),
    })

    const slider = Widget.Slider({
        hexpand: true,
        draw_value: false,
        on_change: ({ value }) => audio.speaker.volume = value,
        setup: self => self.hook(audio.speaker, () => {
            self.value = audio.speaker.volume || 0
        }),
    })

    return Widget.Box({
        class_name: "volume",
        css: "min-width: 180px",
        children: [icon, slider],
    })
}


function BatteryLabel() {
    const value = battery.bind("percent").as(p => p > 0 ? p / 100 : 0)
    const icon = battery.bind("percent").as(p =>
        `battery-level-${Math.floor(p / 10) * 10}-symbolic`)

    return Widget.Box({
        class_name: "battery",
        visible: battery.bind("available"),
        children: [
            Widget.Icon({ icon }),
            Widget.LevelBar({
                widthRequest: 140,
                vpack: "center",
                value,
            }),
        ],
    })
}


function SysTray() {
    const items = systemtray.bind("items")
        .as(items => items.map(item => Widget.Button({
            child: Widget.Icon({ icon: item.bind("icon") }),
            on_primary_click: (_, event) => item.activate(event),
            on_secondary_click: (_, event) => item.openMenu(event),
            tooltip_markup: item.bind("tooltip_markup"),
        })))

    return Widget.Box({
        children: items,
    })
}


function Left(){
  return Widget.Box({
    spacing: global_spacing,
    children: [
      Workspaces(),
      ClientTitle(),
    ],
  })
}

function Center(){
  return Widget.Box({
    spacing: global_spacing,
    children: [
      Media(),
      Notification(),
    ],
  })
}

function Right(){
  return Widget.Box({
    hpack: "end",
    spacing: global_spacing,
    children: [
      Volume(),
//      BatteryLabel(),
      Clock(),
      SysTray(),
    ],
  })
}


const Bar = (/** @type {number} */ monitor) => Widget.Window({
    monitor,
    name: `bar${monitor}`,
    anchor: ['top', 'left', 'right'],
    exclusivity: 'exclusive',
    layer: 'top',
    child: Widget.CenterBox({
        start_widget: Left(),/*Widget.Label({
            hpack: 'center',
            label: 'Welcome to AGS!',
        }),*/
        center_widget: Center(),
        end_widget: Right(), /*Widget.Label({
            hpack: 'center',
            label: time.bind(),
        }),*/
    }),
})


function AppBar_Up(){
  return Widget.Box({
    vertical: true,
    vpack: "start",
    spacing: global_spacing,
    children: [
    ],
  })
}


function AppBar_Center(){
  return Widget.Box({
    vertical: true,
    vpack: "center",
    spacing: global_spacing,
    children: [

    ],
  })
}


function AppBar_Down(){
  return Widget.Box({
    vertical: true,
    vpack: "end",
    spacing: global_spacing,
    children: [
      Widget.Icon({icon: '/home/foko/.config/ags/icons/sovietnobg.png', size: 30})
    ],
  })
}


const AppBar = (monitor) => Widget.Window({
    monitor,
    name: `app_bar${monitor}`,
    anchor: ['left', 'top', 'bottom'],
    exclusivity: 'exclusive',
    layer: 'top',
    child: Widget.CenterBox({
        vertical: true,
        start_widget: AppBar_Up(),
        center_widget: AppBar_Center(),
        end_widget: AppBar_Down(),
    })
})
/*DESKTOP

function Fokquote() {
    const label = Widget.Label();
    const fokquote = Widget.Button({
            on_clicked: () => label.label = Utils.exec("fok-quote"),
            child: Widget.Label(`refresh`),
        });

    return Widget.Box({
        class_name: "fokquote",
        children: [fokquote, label],
    })
}
const Fokquote_win = (monitor) => Widget.Window({
  monitor,
  name: `fok_quote${monitor}`,
  anchor: ['right', 'bottom'],
  layer: 'bottom',
  child: Fokquote(),
})
*/

// END

App.config({
    windows: [NotificationPopups(), Bar(0), AppBar(0)],
})
