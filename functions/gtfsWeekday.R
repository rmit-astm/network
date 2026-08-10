# GTFS weekday name for a date (or vector of dates)
#
# The 'calendar' table in a GTFS feed always uses the English weekday names as its
# column names ('monday' ... 'sunday'). base::weekdays() returns day names in the
# current locale, so it can't be used to look up those columns reliably. This
# derives the name from the ISO weekday number instead (1 = Monday ... 7 = Sunday),
# which is locale-independent.

gtfsWeekday <- function(date) {
  if (!inherits(date, "Date")) date <- as.Date(date)
  c("monday", "tuesday", "wednesday", "thursday",
    "friday", "saturday", "sunday")[as.integer(format(date, "%u"))]
}
