# Makes the small GTFS feed used by the tests ('test_gtfs.zip'): real stops in
# the test area (from a PTV feed at ./data/gtfs.zip), with one made-up tram,
# train and bus trip so each stop has a service type. The OSM extract for the
# test area ('test_osm.gpkg') was clipped from a Melbourne extract with ogr2ogr,
# keeping only the points and polygons that can be destinations.
# Run from the repository root.

library(sf)
library(dplyr)

area <- st_read("./tests/data/test_region.sqlite", quiet = TRUE) %>%
  st_transform(4326)
stops <- read.csv(unz("./data/gtfs.zip", "stops.txt"), colClasses = "character") %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326, remove = FALSE) %>%
  st_filter(area) %>%
  st_drop_geometry() %>%
  arrange(stop_id) %>%
  dplyr::select(stop_id, stop_name, stop_lat, stop_lon) %>%
  head(12)

routes <- data.frame(route_id = c("tram", "train", "bus"), agency_id = "test",
                     route_short_name = c("1", "2", "3"), route_type = c(0, 2, 3))
trips <- data.frame(route_id = routes$route_id, service_id = "weekday",
                    trip_id = paste0(routes$route_id, "_1"))
stop_times <- data.frame(trip_id = rep(trips$trip_id, each = 4),
                         stop_id = stops$stop_id,
                         stop_sequence = rep(1:4, 3)) %>%
  mutate(arrival_time = sprintf("08:%02d:00", stop_sequence * 2),
         departure_time = arrival_time)

feed <- list(
  agency = data.frame(agency_id = "test", agency_name = "Test",
                      agency_url = "https://example.org", agency_timezone = "Australia/Melbourne"),
  stops = stops, routes = routes, trips = trips, stop_times = stop_times,
  calendar = data.frame(service_id = "weekday", monday = 1, tuesday = 1,
                        wednesday = 1, thursday = 1, friday = 1, saturday = 0,
                        sunday = 0, start_date = "20260101", end_date = "20261231"))
dir <- tempfile(); dir.create(dir)
for (name in names(feed)) write.csv(feed[[name]], file.path(dir, paste0(name, ".txt")),
                                    row.names = FALSE)
unlink("./tests/data/test_gtfs.zip")
zip::zipr("./tests/data/test_gtfs.zip", file.path(dir, paste0(names(feed), ".txt")))
