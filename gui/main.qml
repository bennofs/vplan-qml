import QtQuick 2.0
import QtQuick.Controls 1.0
// import VPlan 1.0

ApplicationWindow {
  
  Repeater {
    model: schedule
    Repeater {
      model: modelData
      Text {
        text: modelData
      }
    }
  }
  
}