source("../../functions/chooseAnalysisDate.R")
source("../../functions/gtfsWeekday.R")

test_that("The analysis date avoids dates with services removed", {
  # two services every Wednesday in a three-week window; one is removed on the
  # middle Wednesday (eg a holiday), which would otherwise be chosen
  calendar <- data.frame(service_id = c("a", "b"), monday = 0, tuesday = 0,
                         wednesday = 1, thursday = 0, friday = 0, saturday = 0,
                         sunday = 0, start_date = "20261104", end_date = "20261118")
  calendar_dates <- data.frame(service_id = "b", date = as.Date("2026-11-11"),
                               exception_type = 2)
  gtfs <- list(calendar = calendar, calendar_dates = calendar_dates)
  expect_equal(chooseAnalysisDate(gtfs), as.Date("2026-11-04"))
  # without exceptions, the middle of the equally busy dates
  expect_equal(chooseAnalysisDate(list(calendar = calendar)), as.Date("2026-11-11"))
})
