#include <iostream>
#include <QGuiApplication>
#include <QQuickView>
#include <QQmlApplicationEngine>
#include <QQmlContext>
#include <QResource>
#include <QList>
#include <QtQml>
#include "ScheduleDay.hpp"
#include "Schedule.hpp"

int main(int argc, char *argv[])
{
  QGuiApplication app(argc, argv);
  
  Schedule schedule("test.json");
  std::cout << schedule.index(Index(0,0,0)) << std::endl;

  qmlRegisterType<ScheduleDay>();

  QQmlApplicationEngine engine;
  QVariantList days;
  days << QVariant::fromValue(new ScheduleDay(schedule, WeekDate(0,0)))
       << QVariant::fromValue(new ScheduleDay(schedule, WeekDate(0,1)))
       << QVariant::fromValue(new ScheduleDay(schedule, WeekDate(0,2)))
       << QVariant::fromValue(new ScheduleDay(schedule, WeekDate(0,3)))
       << QVariant::fromValue(new ScheduleDay(schedule, WeekDate(0,4)));
  std::cout << days[0].value<ScheduleDay*>()->rowCount(QModelIndex()) << std::endl;
  engine.rootContext()->setContextProperty("schedule", days);    
  engine.load(QUrl("gui/main.qml"));
  
  QObject* toplevel = engine.rootObjects().value(0);
  QQuickWindow* window = qobject_cast<QQuickWindow*>(toplevel);
  if(!window) {
    qWarning("Error: No root window in QML application");
    return -1;
  }
  window->show();
  return app.exec();
}
