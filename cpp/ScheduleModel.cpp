#include "ScheduleModel.hpp"
#include "hs/Schedule_stub.h"

ScheduleModel::ScheduleModel(std::string filename) {
  // Todo
}

ScheduleModel::~ScheduleModel() {
  hs_free_stable_ptr(m_schedule);
}

QVariant ScheduleModel::data(QModelIndex const& index, int role) const {
  return QVariant();
}

QVariant ScheduleModel::headerData(int section, Qt::Orientation orientation, int role) const {
  return QVariant();
}

int ScheduleModel::rowCount(QModelIndex const& parent) const {
  return 0;
}

int ScheduleModel::maxLessons() const {
  return 0;
}
