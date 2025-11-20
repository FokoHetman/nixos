import Quickshell
import QtQuick.Layouts
import Quickshell.Io
import QtQuick


Variants {
	id: root
	property color backgroundColor: "#282828"
	property color buttonColor: "#404040"
	property color buttonHoverColor: "#3700b3"
  property list<FComponent> wideComponents
  property list<FComponent> components

  

  model: Quickshell.screens
	PanelWindow {
    id: w
    aboveWindows: false

    anchors.right: true
    margins.right: 20

    implicitWidth: 750
    implicitHeight: 1000

		property var modelData
		screen: modelData
    color: "transparent"
		exclusionMode: ExclusionMode.Ignore
    Rectangle {
      radius: 5
      implicitWidth: 750
      implicitHeight: 1000
      color: "#282828"
      border.color: "black"
      border.width: 1
      GridLayout {
        anchors.top: parent.top
        anchors.topMargin: 20
        anchors.horizontalCenter: parent.horizontalCenter

        columns: 1
        rows: 2

        width: 0.95 * parent.width
        height: parent.height / 2 * 0.95
        Repeater {
          model: wideComponents
          delegate: Rectangle {
            required property FComponent modelData;
            Layout.fillWidth: true
            Layout.fillHeight: true
            radius: modelData.radius
            color: modelData.background
            border.color: modelData.border
            Text {
              /*anchors {
                 top: icon.bottom
                 topMargin: 20
                 horizontalCenter: parent.horizontalCenter
               }*/
               
              anchors.centerIn: parent
              horizontalAlignment: Text.AlignHCenter
              verticalAlignment: Text.AlignVCenter
              font.family: "Fira Code Mono"
              text: modelData.text
              font.pointSize: 20
              color: modelData.foreground
            }
          }
        }
      }
      GridLayout {
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 20
        anchors.horizontalCenter: parent.horizontalCenter
        
        width: parent.width * 0.95
        height: parent.height / 2 * 0.95

        columns: 2
        rows: 3

        Repeater {
          model: components
          delegate: Rectangle {
            required property FComponent modelData;
            Layout.fillWidth: true
            Layout.fillHeight: true
            radius: modelData.radius
            color: modelData.background
            //color: ma.containsMouse ? buttonHoverColor : buttonColor
            border.color: modelData.border
            //border.width: ma.containsMouse ? 0 : 1

            /*MouseArea {
               id: ma
               anchors.fill: parent
               hoverEnabled: true
               onClicked: modelData.exec()
             }

             Image {
               id: icon
               anchors.centerIn: parent
               source: `icons/${modelData.icon}.png`
               width: parent.width * 0.25
               height: parent.width * 0.25
             }*/
             Text {
               /*anchors {
                  top: icon.bottom
                  topMargin: 20
                  horizontalCenter: parent.horizontalCenter
                }*/
                anchors.centerIn: parent
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
                font.family: "Fira Code Mono"
                text: modelData.text
                font.pointSize: 20
                color: modelData.foreground
              }
            }
          }
        }
    }
	}
}

