#include "Schedule.hpp"
#include "Exceptions.hpp"
#include "hs/Schedule_stub.h"

#include <QString>

Schedule::Schedule(std::string file) {
  char* err_msg = 0;
  // Const_cast is safe, the haskell ffi just doesn't generate const. We don't actually modify the value from Haskell.
  m_schedule = hs_scheduleLoadFile(const_cast<char*>(file.data()), file.length(), &err_msg);
  if(err_msg) throw FileError("Loading", file, std::string(err_msg));
}

Schedule::Schedule(Schedule const& other) : m_schedule(hs_cloneSchedule(other.m_schedule)) {}
