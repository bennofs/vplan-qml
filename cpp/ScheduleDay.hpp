#pragma once

#include <QObject>
#include <QAbstractListModel>
#include <HsFFI.h>

#include "Index.hpp"
#include "Schedule.hpp"

////////////////////////////////////////////////////////////////////////////////////////////////////
/// \brief Model for the lessons of a day in a schedule.
///
/// A model that views all the lessons of a given day in a schedule. It's just a list model, where 
/// the values are lessons.
////////////////////////////////////////////////////////////////////////////////////////////////////
class ScheduleDay : public QAbstractListModel {

    Q_OBJECT

  public:
    
    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// \brief Constructor
    ///
    /// \param[in] schedule The schedule that contains the data for this model.
    /// \param[in] day      The day which is viewed by this model.
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ScheduleDay(Schedule schedule, WeekDate day) : m_schedule(schedule), m_day(day) {}

    // Model methods
    virtual QVariant data(QModelIndex const& index, int role) const override;
    virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const override;
    virtual int rowCount(QModelIndex const& parent) const override;

  private:

    Schedule m_schedule;
    WeekDate m_day;
};
Q_DECLARE_METATYPE(ScheduleDay*)
