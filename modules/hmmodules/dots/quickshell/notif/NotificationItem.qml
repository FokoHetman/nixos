import QtQuick 2.15
import Quickshell.Services.Notifications

Rectangle {
  id: root
  property Notification notif

  width: parent ? parent.width : 400
  height: 80
  color: "#282828"
  radius: 12
  border.color: "#98971a"
  border.width: 2

  Row {
    anchors.fill: parent
    anchors.margins: 12
    spacing: 12

    Image {
      source: notif.appIcon || ""
      width: 40; height: 40
      fillMode: Image.PreserveAspectFit
      anchors.verticalCenter: parent.verticalCenter
    }

    Column {
      width: parent.width - 64
      anchors.verticalCenter: parent.verticalCenter
      spacing: 4

      Text {
        text: notif.appName || "App"
        color: "#ebdbb2"
        font.bold: true
        font.pixelSize: 14
      }
      Text {
        text: notif.summary || ""
        color: "white"
        font.pixelSize: 15
        width: parent.width
        elide: Text.ElideRight
      }
      Text {
        visible: notif.body.length > 0
        text: notif.body
        color: "#bdae93"
        font.pixelSize: 13
        width: parent.width
        wrapMode: Text.Wrap
        maximumLineCount: 2
        elide: Text.ElideRight
      }
    }
  }

  // Optional hover effect
  MouseArea {
    anchors.fill: parent
    hoverEnabled: true
    onEntered: root.scale = 1.02
    onExited:  root.scale = 1.0
    onClicked: {
      root.opacity = 0
      notif.dismiss()
    }
  }
}
