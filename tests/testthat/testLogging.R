source("../../functions/etc/logging.R")

test_that("timeStep runs the step where it is called and logs its time", {
  addOne <- function(x) x + 1
  expect_output(timeStep(y <- addOne(1)), "addOne took 0.0 s")
  expect_equal(y, 2)
})
