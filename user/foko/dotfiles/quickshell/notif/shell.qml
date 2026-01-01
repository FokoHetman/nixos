import Quickshell
import Quickshell.Services.Notifications
import QtQuick
/*
ShellRoot {
    NotificationServer { id: notify }

    PanelWindow {
        anchors { top: true }
        margins.top: 200
        width: 500
        exclusiveZone: 0
        aboveWindows: true
        color: "transparent"

        Column {
            id: notifColumn
            anchors.horizontalCenter: parent.horizontalCenter
            spacing: 2
        }

        Connections {
            target: notify
            function onNotification(n) {
                console.log("Notification arrived:", n.summary)

                n.tracked = true

                var component = Qt.createComponent("NotificationItem.qml")
                var obj = component.createObject(notifColumn, {
                    notification: n
                })
            }
        }
    }
  }*/
  import Quickshell
import Quickshell.Services.Notifications
import QtQuick 2.15
import QtQuick.Layouts 1.15

ShellRoot {
  NotificationServer { id: notify }

  property var notificationModel: []

  PanelWindow {
    anchors.top: true
    margins.top: 50               // distance from screen top
    width: 420
    height: 500
    exclusiveZone: 0
    aboveWindows: true
    color: "transparent"

    ListView {
      id: notificationList
      anchors.horizontalCenter: parent.horizontalCenter
      width: parent.width * 0.9
      height: childrenRect.height
      spacing: 10                    // gap between notifications
      verticalLayoutDirection: ListView.TopToBottom

      // This makes items animate when added/removed/moved
      displaced: Transition {
        NumberAnimation { properties: "y"; duration: 300; easing.type: Easing.OutQuint }
      }
      add: Transition {
        ParallelAnimation {
          NumberAnimation { property: "opacity"; from: 0; to: 1; duration: 250 }
          NumberAnimation { property: "scale";   from: 0.9; to: 1; duration: 300; easing.type: Easing.OutBack }
        }
      }
      remove: Transition {
        ParallelAnimation {
          NumberAnimation { property: "opacity"; to: 0; duration: 200 }
          NumberAnimation { property: "scale";   to: 0.8; duration: 200 }
        }
      }

      model: notificationModel


      delegate: NotificationItem {
        width: notificationList.width
        notif: modelData

        // Auto-remove from model after animation
        Connections {
          target: notif
          function onClosed() {
            // Find index and remove it → triggers the nice "fall down" animation
            const index = notificationModel.indexOf(notif)
            if (index !== -1) {
              notificationModel.splice(index, 1)
            }
            notificationList.decrementCurrentIndex()
            console.log("destroying")
          }
        }
      }
    }

    Timer {
      id: dismissTimer
      property var n
      interval: 5000
      repeat: false
      onTriggered: function() {
        if (n && n.dismiss) n.dismiss()
      }
    }
    Connections {
      target: notify
      function onNotification(n) {
        n.tracked = true
        // Insert at the top (index 0) so newest is on top
        notificationModel.unshift(n);
        notificationList.model = notificationModel
        // Optional: auto-dismiss after 6 seconds
        dismissTimer.n = n
        dismissTimer.start()
      }
    }
  }
}
