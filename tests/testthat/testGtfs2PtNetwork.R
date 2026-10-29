library(sf)
library(dplyr)
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
