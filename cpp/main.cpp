#include <QGuiApplication>
#include <QQuickView>
#include <QQmlEngine>
#include <QQmlContext>
#include "ScheduleModel.hpp"

int main(int argc, char *argv[])
{
  QGuiApplication app(argc, argv);
  
  QQuickView view;
  view.setSource(QUrl("qrc:///gui/Prototype.qml"));
  ScheduleModel schedule("test.json");
  view.rootContext()->setContextProperty("schedule", &schedule);
  view.show();

  return app.exec();
}
