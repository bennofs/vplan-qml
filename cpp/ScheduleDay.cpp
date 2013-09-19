#include "ScheduleDay.hpp"
#include "hs/Schedule_stub.h"

QVariant ScheduleDay::data(QModelIndex const& index, int role) const {
  return QVariant();
}

QVariant ScheduleDay::headerData(int section, Qt::Orientation orientation, int role) const {
  return QVariant();
}

int ScheduleDay::rowCount(QModelIndex const& parent) const {
  return 0;
}
