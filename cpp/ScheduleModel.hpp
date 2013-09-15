#pragma once

#include <QObject>
#include <QAbstractListModel>
#include <string>
#include <HsFFI.h>

class ScheduleModel : public QAbstractListModel {

    Q_OBJECT
    Q_PROPERTY(int week READ week WRITE setWeek NOTIFY weekChanged)
  
  public:

    // Constructors and destructors
    ScheduleModel(std::string filename);
    ScheduleModel(ScheduleModel&& other) : m_schedule(0) { swap(*this, other); }
    virtual ~ScheduleModel();

    // Operators
    ScheduleModel& operator=(ScheduleModel other) {
      swap(*this, other);
      return *this;
    }
  
    // Model methods
    virtual QVariant data(QModelIndex const& index, int role) const override;
    virtual QVariant headerData(int section, Qt::Orientation orientation, int role) const override;
    virtual int rowCount(QModelIndex const& parent) const override;
  
    // Week property
    int week() const       { return m_week; }
    void setWeek(int week) { m_week = week; }

    // Other member functions
    int maxLessons() const;
    
    // Non-member friends
    friend void swap(ScheduleModel& first, ScheduleModel& second) {
      using std::swap;
      swap(first.m_schedule, second.m_schedule);
    }
  
  private:

    HsStablePtr m_schedule;
    int m_week;

  signals:
    
    void weekChanged(int week);
  
};
