import QtQuick
import Quickshell.Io
import Quickshell

Rectangle {
  required property string command
	required property string text
  property color foreground: "white"
  required property color background
  property color border: "black"
  property int radius: 10
}
