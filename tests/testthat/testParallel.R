source("../../functions/etc/parallel.R")
library(parallel)
library(foreach)

test_that("startCluster uses at most maxcores and registers the cluster", {
  cluster <- startCluster(2)
  on.exit(stopCluster(cluster))
  expect_equal(length(cluster), min(2, detectCores()))
  expect_equal(foreach(i = 1:4, .combine = c) %dopar% (i * 2), c(2, 4, 6, 8))
})
