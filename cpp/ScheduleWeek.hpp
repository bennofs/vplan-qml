#pragma once

#include <QObject>
#include <QAbstractListModel>
#include <string>
#include <HsFFI.h>

////////////////////////////////////////////////////////////////////////////////////////////////////
/// \brief A model viewing a single week of a schedule.
/// 
/// This model represents a single week of a schedule. It's structured as simple ListModel of
/// ScheduleDay models. 
////////////////////////////////////////////////////////////////////////////////////////////////////
class ScheduleWeek : public QAbstractListModel {

    Q_OBJECT
    Q_PROPERTY(int week READ week WRITE setWeek NOTIFY weekChanged)
  
  public:

    // Constructors and destructors
    ScheduleWeek(std::string filename);
    ScheduleWeek(ScheduleWeek&& other) : m_schedule(0) { swap(*this, other); }
    virtual ~ScheduleWeek();

    // Operators
    ScheduleWeek& operator=(ScheduleWeek other) {
      swap(*this, other);
      return *this;
    }
  
    // Model methods
    virtual QVariant data(QModelIndex const& index, int role) const override;
    virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const override;
    virtual int rowCount(QModelIndex const& parent) const override { return 5; }
  
    // Week property
    int week() const       { return m_week; }
    void setWeek(int week) { m_week = week; }
    
    // Non-member friends
    friend void swap(ScheduleWeek& first, ScheduleWeek& second) {
      using std::swap;
      swap(first.m_schedule, second.m_schedule);
      swap(first.m_week, second.m_week);
    }
  
  private:

    HsStablePtr m_schedule;
    int m_week;

  signals:
    
    void weekChanged(int week);
  
};
