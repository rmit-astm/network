library(sf)
library(dplyr)
library(parallel)
library(foreach)
source("../../functions/etc/logging.R")
source("../../functions/etc/parallel.R")
source("../../functions/addDestinations.R")

test_that("Boundary points are found on every part and hole of a polygon", {
  square <- function(x0, y0, size) {
    rbind(c(x0, y0), c(x0 + size, y0), c(x0 + size, y0 + size), c(x0, y0 + size), c(x0, y0))
  }
  # a park in two parts, 200m squares 1km apart; a school with a 100m hole
  polygons <- st_sf(dest_id = c(1, 2),
                    geom = st_sfc(st_multipolygon(list(list(square(0, 0, 200)),
                                                       list(square(1000, 0, 200)))),
                                  st_polygon(list(square(0, 1000, 400), square(150, 1150, 100))),
                                  crs = 7899))
  points <- boundaryPoints(polygons, 7899)
  xy <- st_coordinates(points)
  
  # a point every 20m: 40 on each part of the park, 80 and 20 on the school
  # boundary and hole
  expect_equal(as.vector(table(points$id)), c(80, 100))
  expect_equal(sum(points$id == 1 & xy[, "X"] > 500), 40)
  expect_equal(sum(points$id == 2 & xy[, "X"] > 100 & xy[, "X"] < 300 &
                     xy[, "Y"] > 1100 & xy[, "Y"] < 1300), 20)
})
