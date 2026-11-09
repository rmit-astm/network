library(sf)
library(dplyr)
library(stringr)
library(lwgeom)
library(parallel)
library(foreach)
source("../../functions/etc/logging.R")
source("../../functions/etc/parallel.R")
source("../../functions/processOsm.R")
source("../../functions/splitPathsAtPoints.R")

test_that("Paths are split where they cross on the same level, but not under a bridge", {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # two roads crossing at (100, 0) with no shared vertex, and a bridge over
  # the first road at (150, 0); a traffic signal near the crossing
  lines <- st_sf(osm_id = c("1", "2", "3"),
                 highway = c("residential", "residential", "primary"),
                 other_tags = c(NA, NA, '"bridge"=>"yes"'),
                 geom = st_sfc(st_linestring(rbind(c(0, 0), c(200, 0))),
                               st_linestring(rbind(c(100, -100), c(100, 100))),
                               st_linestring(rbind(c(150, -100), c(150, 100))), crs = 7899))
  points <- st_sf(osm_id = "4", highway = "traffic_signals", other_tags = NA_character_,
                  geom = st_sfc(st_point(c(100, 5)), crs = 7899))
  st_write(lines, file.path(dir, "osm.gpkg"), "lines", quiet = TRUE)
  st_write(points, file.path(dir, "osm.gpkg"), "points", quiet = TRUE)
  region <- st_sf(geom = st_as_sfc(st_bbox(c(xmin = -500, ymin = -500, xmax = 500, ymax = 500),
                                           crs = st_crs(7899))))
  st_write(region, file.path(dir, "region.sqlite"), quiet = TRUE)

  network <- suppressWarnings(processOsm(file.path(dir, "osm.gpkg"), file.path(dir, "region.sqlite"),
                                         10000, 7899, maxcores = 1))
  nodes <- network[[1]]
  edges <- network[[2]]
  xy <- st_coordinates(nodes)
  nodeAt <- function(x, y) nodes$id[xy[, "X"] == x & xy[, "Y"] == y]

  # each crossing road is split in two; the bridge is not split
  expect_equal(sort(edges$osm_id), c("1", "1", "2", "2", "3"))
  expect_equal(nrow(nodes), 7)
  expect_equal(sort(edges$length), c(100, 100, 100, 100, 200))
  # the parts of the crossing roads all meet at the crossing node
  crossing <- nodeAt(100, 0)
  expect_equal(sum(edges$from_id == crossing | edges$to_id == crossing), 4)
  # from and to ids are the nodes at the start and end of each edge
  starts <- st_coordinates(st_startpoint(edges))
  ends <- st_coordinates(st_endpoint(edges))
  expect_equal(edges$from_id, mapply(nodeAt, starts[, "X"], starts[, "Y"]))
  expect_equal(edges$to_id, mapply(nodeAt, ends[, "X"], ends[, "Y"]))
  # the crossing is a signal, being within 20m of the traffic signal
  expect_equal(nodes$id[nodes$is_signal == 1], crossing)
})
