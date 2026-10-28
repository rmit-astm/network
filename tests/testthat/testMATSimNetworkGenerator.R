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

test_that("A build resumed from the checkpoint gives the same network", {
  wd <- getwd()
  setwd("../..")
  on.exit({
    unlink(c("output/test_checkpoint", "output/test_resumed",
             "output/test_checkpoint.sqlite"), recursive = TRUE)
    setwd(wd)
  })

  makeNetwork("./tests/test_config.yml",
              list(outputSubdirectory = "test_checkpoint", saveCheckpoint = TRUE,
                   checkpointSqlite = "./output/test_checkpoint.sqlite"))
  makeNetwork("./tests/test_config.yml",
              list(outputSubdirectory = "test_resumed", startFromCheckpoint = TRUE,
                   checkpointSqlite = "./output/test_checkpoint.sqlite"))

  for (layer in c("nodes", "links")) {
    full <- sf::st_read("output/test_checkpoint/network.sqlite", layer, quiet = TRUE)
    resumed <- sf::st_read("output/test_resumed/network.sqlite", layer, quiet = TRUE)
    expect_equal(sf::st_drop_geometry(resumed), sf::st_drop_geometry(full))
    expect_equal(sf::st_geometry(resumed), sf::st_geometry(full))
  }
  expect_match(paste(readLines("output/test_resumed/makeMatsimNetwork.log"), collapse = "\n"),
               "Reading in checkpoint")
})
