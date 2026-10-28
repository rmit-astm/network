source("../../NetworkGenerator.R")

test_that("Network generation from Sqlite input works", {
  set.seed(12345)
  wd<-getwd()

  setwd("../..")
  makeNetwork("./tests/test_config.yml")
  setwd(wd)

  expect_true(file.exists('../../output/test/network.sqlite'))
  expect_true(file.exists('../../output/test/config.yml'))
  links <- sf::st_read('../../output/test/network.sqlite', layer = "links", quiet = TRUE)
  expect_gt(nrow(links), 0)
})



test_that("A failed build stops logging and reports the error", {
  sinks <- sink.number()
  wd <- getwd()
  setwd("../..")
  on.exit({unlink("output/test_error", recursive = TRUE); setwd(wd)})

  # an invalid output CRS fails just after logging has started
  expect_error(suppressWarnings(
    makeNetwork("./tests/test_config.yml",
                list(outputSubdirectory = "test_error", outputCrs = "not-a-crs"))))
  expect_equal(sink.number(), sinks)
})
