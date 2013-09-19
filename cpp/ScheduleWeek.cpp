#include "ScheduleWeek.hpp"
#include "ScheduleDay.hpp"
#include "hs/Schedule_stub.h"

ScheduleWeek::ScheduleWeek(std::string filename) {
  // Todo
}

ScheduleWeek::~ScheduleWeek() {
  hs_free_stable_ptr(m_schedule);
}

QVariant ScheduleWeek::data(QModelIndex const& index, int role) const {
  return QVariant();
}

QVariant ScheduleWeek::headerData(int section, Qt::Orientation orientation, int role) const {
  return QVariant();
}
