library(sf)
library(dplyr)
library(igraph)
library(parallel)
library(foreach)
source("../../functions/etc/logging.R")
source("../../functions/etc/parallel.R")
source("../../functions/gtfs2PtNetwork.R")

test_that("Bus stops snap to the nearer eligible end node of their nearest link", {
  nodes <- st_as_sf(data.frame(id = c(1, 2, 3, 4), x = c(0, 100, 0, 100), y = c(0, 0, 50, 50)),
                    coords = c("x", "y"), remove = FALSE, crs = 7899)
  links <- st_sf(from_id = c(1, 3), to_id = c(2, 5),
                 geometry = st_sfc(st_linestring(rbind(c(0, 0), c(100, 0))),
                                   st_linestring(rbind(c(0, 50), c(100, 50))), crs = 7899))
  stops <- st_as_sf(data.frame(x = c(80, 10, 90, 50), y = c(5, -5, 45, 1)),
                    coords = c("x", "y"), crs = 7899)

  snapped <- nearestNodeOnNearestLink(stops, nodes, links)
  # node 5 is not in 'nodes', so the stop near it goes to node 3; the stop
  # halfway along the first link goes to the node st_nearest_feature picks
  tied <- nodes$id[1:2][st_nearest_feature(stops[4, ], nodes[1:2, ])]
  expect_equal(snapped$id, c(2, 1, 3, tied))
  expect_equal(nrow(nearestNodeOnNearestLink(stops[0, ], nodes, links)), 0)
})

test_that("Routes between bus stops are the shortest paths, in the order of the stop pairs", {
  # links 1-2-3 (length 20) and 1-4-3 (length 30) both join nodes 1 and 3;
  # node 1 can't be reached from node 5
  links <- st_sf(from_id = c(1, 2, 1, 4, 3), to_id = c(2, 3, 4, 3, 5),
                 length = c(10, 10, 15, 15, 10), link_id = c(11, 12, 13, 14, 15),
                 geometry = st_sfc(lapply(1:5, function(i) st_point(c(i, 0))), crs = 7899))
  stopTimes <- data.frame(trip_id = c("a", "a", "a", "b", "b", "c", "c"),
                          stop_id = c(1, 3, 5, 1, 2, 5, 1), region_stop = 1)
  trips <- data.frame(trip_id = c("a", "b", "c"), route_id = "r")
  routes <- data.frame(route_id = "r", service_type = "bus")

  routed <- findNodePairRoutes(stopTimes, trips, routes, links, maxcores = 1)
  expect_equal(routed$stop_id, c(1, 3, 1, 5))
  expect_equal(routed$next_stop_id, c(3, 5, 2, 1))
  expect_equal(routed$link_ids, c("11, 12", "15", "11", NA))
})

test_that("Bus routes much longer than the car route, or not found, use the car route", {
  # the shape links go from node 1 to node 3 by a 2.1km detour through node 4,
  # not from node 3 back to node 1, and from node 1 to node 5 a little longer
  # than the car link; the car links go directly
  shape.links <- st_sf(from_id = c(1, 4, 1), to_id = c(4, 3, 5),
                       length = c(1050, 1050, 500), link_id = c(31, 32, 33),
                       geometry = st_sfc(lapply(1:3, function(i) st_point(c(i, 0))), crs = 7899))
  car.links <- st_sf(from_id = c(1, 2, 3, 1), to_id = c(2, 3, 1, 5),
                     length = c(300, 300, 600, 400), link_id = c(21, 22, 23, 24),
                     geometry = st_sfc(lapply(1:4, function(i) st_point(c(i, 0))), crs = 7899))
  stopTimes <- data.frame(trip_id = c("a", "a", "a", "b", "b"),
                          stop_id = c(1, 3, 1, 1, 5), x = c(0, 600, 0, 0, 0),
                          y = c(0, 0, 0, 0, 400), region_stop = 1)
  trips <- data.frame(trip_id = c("a", "b"), route_id = "r")
  routes <- data.frame(route_id = "r", service_type = "bus")

  routed <- findNodePairRoutes(stopTimes, trips, routes, shape.links, maxcores = 1)
  expect_equal(routed$link_ids, c("31, 32", NA, "33"))
  routed <- findNodePairRoutes(stopTimes, trips, routes, shape.links, maxcores = 1,
                               car.links = car.links)
  expect_equal(routed$stop_id, c(1, 3, 1))
  expect_equal(routed$next_stop_id, c(3, 1, 5))
  expect_equal(routed$link_ids, c("21, 22", "23", "33"))
  expect_equal(names(routed), c("stop_id", "next_stop_id", "link_ids"))
})
