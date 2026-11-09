# script to check a built network, and report the checks in 'qa_report.md' in
# the network's output folder: size, ids and lengths, connectivity of each
# mode, destinations and their nodes, public transport stops and routes, and
# (if given) the differences from an earlier build

# usage, from the repository root:
#   Rscript calibration-validation/CheckNetwork.R <output folder> [earlier output folder] [gtfs zip]
# eg Rscript calibration-validation/CheckNetwork.R output/v2.2/victoria output/v2.1/victoria data/gtfs.zip
# the gtfs zip (the feed the network was built from) is only needed to check
# how far each stop is from the node it was placed on

checkNetwork <- function(outputDir, previousDir = NA, gtfsFeed = NA) {
  
  # outputDir <- "./output/v2.1/bendigo"
  # previousDir <- NA
  # gtfsFeed <- NA
  
  # 1 packages ----
  suppressPackageStartupMessages({
    library(tidyverse)
    library(sf)
    library(igraph)
  })
  
  
  # 2 checks ----
  checks <- networkChecks(outputDir, gtfsFeed)
  
  
  # 3 report ----
  report <- c(paste("# Network checks:", outputDir), "",
              paste("Checked", format(Sys.time(), "%d %B %Y %H:%M")), "")
  
  report <- c(report, "## Size", "", mdTable(checks$size), "")
  if (!is.na(previousDir) && previousDir != "") {
    previous <- networkChecks(previousDir, NA)
    comparison <- checks$size %>%
      left_join(previous$size %>% dplyr::select(measure, earlier = value), by = "measure") %>%
      mutate(change = value - earlier,
             change_pct = round(100 * change / earlier, 2))
    report <- c(report, paste("## Compared with", previousDir), "", mdTable(comparison), "")
  }
  
  report <- c(report, "## Ids and lengths", "",
              "Each of these should be 0.", "", mdTable(checks$integrity), "")
  report <- c(report, "## Connectivity", "",
              paste("Links of each mode as a directed network: the number of strongly",
                    "connected parts, and the share of the mode's nodes in the largest part,",
                    "which should be 100%."), "",
              mdTable(checks$connectivity), "")
  report <- c(report, "## Destinations", "",
              paste("Entry nodes missing from the network, or not on a link of their mode,",
                    "should be 0. Distance is from each destination's centroid to its nearest",
                    "entry node for the mode."), "",
              mdTable(checks$destinations), "")
  
  if (!is.null(checks$stops)) {
    report <- c(report, "## Public transport stops", "",
                paste("Distance from each GTFS stop to the network node it was placed on,",
                      "for stops in and outside the network's region (outside it, the",
                      "network may have only main roads)."), "",
                mdTable(checks$stops), "")
  }
  if (!is.null(checks$routes)) {
    report <- c(report, "## Public transport routes", "",
                paste("For each transit route, its length along the network compared with",
                      "the straight-line distance between its stops in turn, for the routes",
                      "with at least 90% of their length on roads (rather than straight public",
                      "transport links). Routes over 2 times the straight-line distance may",
                      "have been routed the wrong way. Links missing from the network should",
                      "be 0."), "",
                mdTable(checks$routes), "",
                "Routes with the largest ratios:", "",
                mdTable(checks$longest.routes), "")
  }
  
  writeLines(report, file.path(outputDir, "qa_report.md"))
  message("Written ", file.path(outputDir, "qa_report.md"))
  
  return(invisible(checks))
}


# the checks for one network, as a list of tables
networkChecks <- function(outputDir, gtfsFeed) {
  
  networkFile <- file.path(outputDir, "network.sqlite")
  
  # read in the network, without link geometries (not needed, and large)
  nodes <- st_read(networkFile, query = "SELECT id, x, y FROM nodes", quiet = TRUE)
  networkCrs <- st_crs(st_read(networkFile, query = "SELECT * FROM nodes LIMIT 1", quiet = TRUE))
  links <- st_read(networkFile, quiet = TRUE,
                   query = paste("SELECT from_id, to_id, length, freespeed, capacity, modes,",
                                 "is_car, id, link_id FROM links"))
  
  # modes of each link, as columns
  modes <- c("car", "bike", "walk", "bus", "tram", "train")
  for (mode in modes) {
    links[[mode]] <- grepl(paste0("(^|,)", mode, "(,|$)"), links$modes)
  }
  # road (and path) links, rather than public transport only links
  links$road <- links$car | links$bike | links$walk
  
  # size ----
  size <- bind_rows(
    tibble(measure = c("nodes", "links"), value = c(nrow(nodes), nrow(links))),
    tibble(measure = paste(modes, "links"),
           value = sapply(modes, function(mode) sum(links[[mode]]))),
    tibble(measure = paste(modes, "link km"),
           value = sapply(modes, function(mode) round(sum(links$length[links[[mode]]]) / 1000)))
  )
  
  # ids and lengths ----
  integrity <- tibble(
    check = c("duplicate node ids",
              "links with a from or to node missing from the nodes",
              "road links that start and end at the same node",
              "links with no length, or 0 or less",
              "car links with no freespeed or capacity",
              "links with no mode"),
    links = c(sum(duplicated(nodes$id)),
              sum(!(links$from_id %in% nodes$id) | !(links$to_id %in% nodes$id)),
              sum(links$road & links$from_id == links$to_id, na.rm = TRUE),
              sum(is.na(links$length) | links$length <= 0),
              sum(links$car & (is.na(links$freespeed) | is.na(links$capacity))),
              sum(rowSums(links[, modes]) == 0))
  )
  
  # connectivity ----
  connectivity <- lapply(c("car", "bike", "walk"), function(mode) {
    mode.links <- links[links[[mode]], ]
    g <- graph_from_data_frame(mode.links %>% dplyr::select(from_id, to_id), directed = TRUE)
    strong <- components(g, mode = "strong")
    tibble(mode = mode, nodes = vcount(g), parts = strong$no,
           largest_part_pct = round(100 * max(strong$csize) / vcount(g), 2))
  }) %>%
    bind_rows()
  
  # destinations ----
  destinations <- st_read(networkFile, quiet = TRUE,
                          query = paste("SELECT dest_id, dest_type, node_cycle, node_walk,",
                                        "node_car, centroid_x, centroid_y FROM destinations"))
  destinationColumns <- c(bike = "node_cycle", walk = "node_walk", car = "node_car")
  destinationChecks <- lapply(names(destinationColumns), function(mode) {
    column <- destinationColumns[[mode]]
    entry <- tibble(dest_id = destinations$dest_id,
                    node = strsplit(destinations[[column]], ", ")) %>%
      unnest(node) %>%
      mutate(node = as.numeric(node))
    mode.nodes <- unique(c(links$from_id[links[[mode]]], links$to_id[links[[mode]]]))
    distance <- entry %>%
      left_join(destinations %>% dplyr::select(dest_id, centroid_x, centroid_y), by = "dest_id") %>%
      left_join(nodes %>% dplyr::select(node = id, x, y), by = "node") %>%
      mutate(distance = sqrt((x - centroid_x)^2 + (y - centroid_y)^2)) %>%
      group_by(dest_id) %>%
      summarise(distance = min(distance))
    tibble(mode = mode,
           destinations = nrow(destinations),
           entry_nodes_missing = sum(!(entry$node %in% nodes$id)),
           entry_nodes_not_on_mode = sum(!(entry$node %in% mode.nodes)),
           median_distance_m = round(median(distance$distance, na.rm = TRUE)),
           p95_distance_m = round(quantile(distance$distance, 0.95, na.rm = TRUE)),
           over_500m = sum(distance$distance > 500, na.rm = TRUE))
  }) %>%
    bind_rows()
  
  # public transport stops ----
  stops <- NULL
  stopTableFile <- file.path(outputDir, "pt", "stopTable.csv")
  if (!is.na(gtfsFeed) && file.exists(stopTableFile)) {
    stopTable <- read.csv(stopTableFile, row.names = 1)
    gtfsStops <- read.csv(unz(gtfsFeed, "stops.txt"), colClasses = "character") %>%
      st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
      st_transform(networkCrs)
    gtfsStops <- data.frame(gtfs_stop_id = gtfsStops$stop_id, st_coordinates(gtfsStops))
    stopDistances <- stopTable %>%
      left_join(gtfsStops, by = "gtfs_stop_id") %>%
      left_join(nodes %>% dplyr::select(node_id = id, x, y), by = "node_id") %>%
      mutate(distance = sqrt((x - X)^2 + (y - Y)^2))
    # whether stops are in the network's region (the city, outside which the
    # network may only have main roads), if the region file is found
    stopDistances$in_region <- NA
    configFile <- file.path(outputDir, "config.yml")
    if (file.exists(configFile)) {
      regionFile <- yaml::read_yaml(configFile)$region$region
      if (!is.null(regionFile) && file.exists(regionFile)) {
        region <- st_read(regionFile, quiet = TRUE) %>% st_transform(networkCrs)
        located <- !is.na(stopDistances$X)
        stopDistances$in_region[located] <- lengths(st_intersects(
          st_as_sf(stopDistances[located, ], coords = c("X", "Y"), crs = networkCrs), region)) > 0
      }
    }
    stops <- stopDistances %>%
      group_by(service_type, in_region) %>%
      summarise(stops = n(),
                not_in_gtfs_feed = sum(is.na(X)),
                not_placed = sum(is.na(x)),
                median_distance_m = round(median(distance, na.rm = TRUE)),
                p95_distance_m = round(quantile(distance, 0.95, na.rm = TRUE)),
                over_100m = sum(distance > 100, na.rm = TRUE), .groups = "drop")
  }
  
  # public transport routes ----
  routes <- NULL
  longest.routes <- NULL
  scheduleFile <- file.path(outputDir, "pt", "transitSchedule.xml")
  if (file.exists(scheduleFile)) {
    schedule <- readLines(scheduleFile)
  
    # stop locations
    facilities <- grep("<stopFacility ", schedule, value = TRUE)
    facilities <- tibble(stop = sub('.* id="([^"]*)".*', "\\1", facilities),
                         x = as.numeric(sub('.* x="([^"]*)".*', "\\1", facilities)),
                         y = as.numeric(sub('.* y="([^"]*)".*', "\\1", facilities)))
  
    # the stops and links of each route, in order
    lines <- tibble(text = schedule) %>%
      mutate(route = cumsum(grepl("<transitRoute ", text))) %>%
      filter(route > 0)
    routeIds <- lines %>%
      filter(grepl("<transitRoute ", text)) %>%
      mutate(route_id = sub('.* id="([^"]*)".*', "\\1", text)) %>%
      dplyr::select(route, route_id)
    routeModes <- lines %>%
      filter(grepl("<transportMode>", text)) %>%
      mutate(mode = sub(".*<transportMode>(.*)</transportMode>.*", "\\1", text)) %>%
      dplyr::select(route, mode)
    routeStops <- lines %>%
      filter(grepl("<stop .*refId=", text)) %>%
      mutate(stop = sub('.* refId="([^"]*)".*', "\\1", text)) %>%
      left_join(facilities, by = "stop") %>%
      group_by(route) %>%
      summarise(stops = n(),
                straight_km = sum(sqrt(diff(x)^2 + diff(y)^2)) / 1000)
    # network link ids, as written to the MATSim network
    linkLengths <- links %>%
      mutate(xml_id = ifelse(!is.na(id), as.character(id), as.character(link_id))) %>%
      dplyr::select(xml_id, length, road)
    routeLinks <- lines %>%
      filter(grepl("<link refId=", text)) %>%
      mutate(xml_id = sub('.*refId="([^"]*)".*', "\\1", text)) %>%
      left_join(linkLengths, by = "xml_id") %>%
      group_by(route) %>%
      summarise(links = n(),
                links_missing = sum(is.na(length)),
                network_km = sum(length, na.rm = TRUE) / 1000,
                road_pct = 100 * sum(length[road %in% TRUE]) / sum(length, na.rm = TRUE))
    routeChecks <- routeIds %>%
      left_join(routeModes, by = "route") %>%
      left_join(routeStops, by = "route") %>%
      left_join(routeLinks, by = "route") %>%
      mutate(ratio = network_km / straight_km)
  
    # the ratio is only a check of routes found along the roads: routes outside
    # the network's region may follow straight public transport links instead
    routes <- routeChecks %>%
      group_by(mode) %>%
      summarise(routes = n(),
                links_missing = sum(links_missing, na.rm = TRUE),
                routes_on_roads = sum(road_pct >= 90, na.rm = TRUE),
                median_ratio = round(median(ratio[road_pct >= 90], na.rm = TRUE), 2),
                p95_ratio = round(quantile(ratio[road_pct >= 90], 0.95, na.rm = TRUE), 2),
                over_2 = sum(ratio[road_pct >= 90] > 2, na.rm = TRUE))
    longest.routes <- routeChecks %>%
      filter(road_pct >= 90) %>%
      arrange(desc(ratio)) %>%
      head(10) %>%
      mutate(across(c(straight_km, network_km, ratio), ~ round(.x, 2)),
             road_pct = round(road_pct)) %>%
      dplyr::select(route_id, mode, stops, straight_km, network_km, road_pct, ratio)
  }
  
  return(list(size = size, integrity = integrity, connectivity = connectivity,
              destinations = destinationChecks, stops = stops, routes = routes,
              longest.routes = longest.routes))
}


# a dataframe as a markdown table
mdTable <- function(df) {
  df <- as.data.frame(df)
  cells <- lapply(df, function(column) {
    if (is.numeric(column)) format(column, big.mark = ",", scientific = FALSE, trim = TRUE)
    else as.character(column)
  })
  rows <- do.call(paste, c(cells, sep = " | "))
  c(paste("|", paste(names(df), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(df)), collapse = " | "), "|"),
    paste("|", rows, "|"))
}


# run from the command line
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  checkNetwork(args[1],
               if (length(args) > 1) args[2] else NA,
               if (length(args) > 2) args[3] else NA)
}
