#pragma once

struct WeekDate {

    int day;
    int week;    

    WeekDate(int week, int day): day(day), week(week) {}
};

struct Index {

    WeekDate date;
    int lesson;

    Index(int week, int day, int lesson): date(week, day), lesson(lesson) {}
    Index(WeekDate date, int lesson): date(date), lesson(lesson) {}
};
