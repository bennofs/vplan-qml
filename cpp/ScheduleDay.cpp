#include "ScheduleDay.hpp"
#include "hs/Schedule_stub.h"

QVariant ScheduleDay::data(QModelIndex const& index, int role) const {
  if(!index.isValid() || index.row() >= m_schedule.maxLessonsOn(m_day)) return QVariant();
  if(role != Qt::DisplayRole) return QVariant();
  return QVariant(m_schedule.index(Index(m_day, index.row())));
}

QVariant ScheduleDay::headerData(int section, Qt::Orientation orientation, int role) const {
  if(role != Qt::DisplayRole) return QVariant();
  return QVariant(section);
}

int ScheduleDay::rowCount(QModelIndex const& /* parent */) const {
  return m_schedule.maxLessonsOn(m_day);
}
