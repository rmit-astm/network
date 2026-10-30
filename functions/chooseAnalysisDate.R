# choose a representative analysis date automatically from a GTFS feed's calendar
#
# Used when 'analysis_date' is not set (left as null) in the config file, so the
# date does not have to be hard-coded and kept up to date with the feed. Picks the
# weekday (preferring midweek) within the feed's service window that has the most
# active weekly services, so the chosen date is valid for the feed and reflects a
# typical service day.
#
# Note: like the downstream filtering in gtfs2PtNetwork.R, this uses the weekly
# 'calendar' table, with the 'calendar_dates' exceptions for each date (so a date
# with services removed, such as a holiday, is not chosen); feeds that rely only
# on 'calendar_dates' are not supported and will need 'analysis_date' set
# explicitly.

chooseAnalysisDate <- function(gtfs,
                               preferredWeekdays = c("wednesday", "tuesday",
                                                     "thursday", "monday",
                                                     "friday", "saturday",
                                                     "sunday")) {

  cal <- gtfs$calendar
  if (is.null(cal) || nrow(cal) == 0) {
    stop("Unable to choose an analysis date: the GTFS feed has no 'calendar' table. Set 'analysis_date' explicitly in the config file.")
  }

  # normalise start/end dates to Date (feeds store them as yyyymmdd integers,
  # equivalent strings, or Date, depending on how they were read)
  asDate <- function(x) {
    if (inherits(x, "Date")) return(x)
    as.Date(as.character(x), format = "%Y%m%d")
  }
  start <- asDate(cal$start_date)
  end   <- asDate(cal$end_date)

  # services added (exception_type 1) or removed (2) on particular dates
  exceptions <- gtfs$calendar_dates
  if (is.null(exceptions)) exceptions <- data.frame(service_id = character(0),
                                                    date = character(0),
                                                    exception_type = integer(0))
  exceptionDates <- asDate(exceptions$date)

  # candidate dates across the whole service window
  candidateDates <- seq(min(start, na.rm = TRUE), max(end, na.rm = TRUE), by = "day")
  # weekday of each candidate, matching the calendar's English column names
  weekdayName <- gtfsWeekday(candidateDates)

  # for each preferred weekday in turn, find the date with the most active services
  for (day in preferredWeekdays) {
    if (!day %in% names(cal)) next
    theseDates <- candidateDates[weekdayName == day]
    if (length(theseDates) == 0) next
    counts <- vapply(theseDates, function(d) {
      weekly <- cal$service_id[which(start <= d & end >= d & cal[[day]] == 1)]
      onDate <- which(exceptionDates == d)
      removed <- exceptions$service_id[onDate][exceptions$exception_type[onDate] == 2]
      added <- exceptions$service_id[onDate][exceptions$exception_type[onDate] == 1]
      length(union(setdiff(weekly, removed), added))
    }, numeric(1))
    if (max(counts) > 0) {
      # most-served date of this weekday; if tied, the middle one (avoids edges)
      best <- theseDates[counts == max(counts)]
      return(best[ceiling(length(best) / 2)])
    }
  }

  stop("Unable to choose an analysis date from the GTFS calendar; set 'analysis_date' explicitly in the config file.")
}
