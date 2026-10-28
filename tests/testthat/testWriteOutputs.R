library(dplyr)
source("../../functions/etc/logging.R")
source("../../functions/writeOutputs.R")

test_that("The XML output carries the link slopes", {
  nodes <- data.frame(id = c(1, 2), x = c(0, 100), y = c(0, 0), z = c(10, 12))
  links <- data.frame(link_id = c(1, 2), from_id = c(1, 2), to_id = c(2, 1),
                      length = 100, capacity = 600, freespeed = 13.9, permlanes = 1,
                      modes = "car,bike,walk", highway = "residential",
                      cycleway = NA, surface = NA, slope_pct = c(2, -2),
                      lvl_traf_stress = 1)
  outputDir <- tempfile()
  dir.create(outputDir)
  invisible(capture.output(exportXML(list(nodes, links), outputDir)))

  xml <- readLines(file.path(outputDir, "network.xml"))
  slopes <- sub(".*>(.*)</attribute>", "\\1", grep("slopePct", xml, value = TRUE))
  expect_equal(slopes, c("2", "-2"))
})
