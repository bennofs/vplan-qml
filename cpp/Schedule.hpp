#pragma once

#include "Index.hpp"
#include "hs/Schedule_stub.h"

#include <string>

////////////////////////////////////////////////////////////////////////////////////////////////////
/// \brief A wrapper for a schedule which is managed by haskell.
///
/// This class provides an interface to the haskell code for schedules. It can load schedules from
/// files and index into them, and perform updates on schedules.
////////////////////////////////////////////////////////////////////////////////////////////////////
class Schedule {

  public:

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// \brief Load a schedule from a file.
    ///
    /// This function constructs a new schedule by loading it from a file. The file is expected to
    /// be in JSON format. 
    ///
    /// \param[in] file      The file to load.
    /// \exception FileError This exception is thrown when loading the file fails. This could happen 
    ///                      because the file does not exist, cannot be parsed, or some other reason.
    ////////////////////////////////////////////////////////////////////////////////////////////////
    explicit Schedule(std::string file);

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// \brief Create a new schedule by copying a existing one. 
    ///        
    /// This constructor does a deep-copy of an existing schedule. This might not be very efficient,
    /// because it has to perform a FFI call to copy the schedule. Use the move constructor when
    /// possible.
    ///
    /// \param[in] other The schedule to copy from.
    ////////////////////////////////////////////////////////////////////////////////////////////////
    Schedule(Schedule const& other);
    
    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// \brief Move construct from another schedule.
    ///
    /// This constructor steels the schedule from another schedule object. It can be implemented
    /// without calling to the Haskell side.
    ///
    /// \param[in,out] other Schedule to move from.
    ////////////////////////////////////////////////////////////////////////////////////////////////
    Schedule(Schedule&& other) : m_allocated(false) { swap(*this, other); }

    ////////////////////////////////////////////////////////////////////////////////////////////////
    /// \brief Destructor. Takes care of releasing the handle to the schedule.
    ////////////////////////////////////////////////////////////////////////////////////////////////
    ~Schedule() { if(m_allocated) hs_free_stable_ptr(m_schedule); }    

    Schedule& operator=(Schedule other) { swap(*this, other); return *this;}

    int index(Index index) const { return hs_scheduleIndex(m_schedule, index.date.week, index.date.day, index.lesson); }
    int maxLessonsOn(WeekDate day) const { return hs_dayMaxLessons(m_schedule, day.week, day.day); }

    friend void swap(Schedule& first, Schedule& second) {
      using std::swap;
      swap(first.m_schedule, second.m_schedule);
      swap(first.m_allocated, second.m_allocated);
    }
    
  private:

    HsStablePtr m_schedule;           ///< Handle to the schedule
    bool        m_allocated = true;   ///< True if m_schedule is a valid handle.
    
};
