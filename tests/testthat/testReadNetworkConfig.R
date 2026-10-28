source("../../functions/readNetworkConfig.R")

# config files are read relative to the repository root
readConfig <- function(...) {
  wd <- getwd()
  on.exit(setwd(wd))
  setwd("../..")
  readNetworkConfig(...)
}

test_that("The test config is merged over the defaults", {
  params <- readConfig("./tests/test_config.yml")$params
  expect_equal(params$city, "Test")
  expect_false(params$extractOsm)
  expect_equal(params$outputCrs, 7899)        # from the defaults
  expect_equal(params$gtfsFolders, 1:6)       # from the defaults
  expect_true(is.na(params$analysis_date))    # 'null' becomes NA
})

test_that("Overrides are applied last", {
  params <- readConfig("./tests/test_config.yml",
                       list(maxcores = 4, analysis_date = "2026-11-11"))$params
  expect_equal(params$maxcores, 4)
  expect_equal(params$analysis_date, as.Date("2026-11-11"))
})

test_that("Unknown parameters are rejected", {
  expect_error(readConfig("./tests/test_config.yml", list(maxcore = 4)),
               "Unknown parameters in overrides: maxcore")
  bad <- tempfile(fileext = ".yml")
  writeLines(c("gtfs:", "  addElevation: false"), bad)
  expect_error(readConfig(bad), "should be elevation/addElevation")
})

test_that("Missing input files are reported before building", {
  expect_error(readConfig("./tests/test_config.yml",
                          list(addElevation = TRUE, demFile = "./data/no_such_dem.tif")),
               "demFile not found")
  expect_error(readConfig("./tests/test_config.yml", list(addNDVI = TRUE)),
               "ndviFile is not set")
})

test_that("The saved config reproduces the parameters", {
  config <- readConfig("./tests/test_config.yml", list(maxcores = 4))
  saved <- tempfile(fileext = ".yml")
  yaml::write_yaml(config$resolved, saved)
  expect_equal(readConfig(saved)$params, config$params)
})
