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


