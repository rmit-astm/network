# function to create a destination layer to add to output network

# uses functions for various destination types with tag combinations set out
# in 'getDestinationTypes.R'

addDestinations <- function(nodes_current, 
                            edges_current, 
                            osmGpkg,
                            city, 
                            gtfs_feed,
                            outputCrs,
                            region,
                            regionBufferDist,
                            localDestinations,
                            maxcores) {
   
  # nodes_current = networkDensified[[1]]
  # edges_current = networkDensified[[2]]
  # osmGpkg = "./data/melbourne_osm.gpkg"
  # city = "Melbourne"
  # gtfs_feed = "./data/gtfs.zip"
  # outputCrs = 28355
  
  # # check keys
  # options(max.print = 2000)
  # point.tags <- oe_get_keys(osmGpkg, layer = "points") %>% sort()
  # polygon.tags <- oe_get_keys(osmGpkg, layer = "multipolygons") %>% sort()

  # reading layers ----
  # ----------------------------------#
  echo("Reading in the OSM layers\n")

  extra.tag.string <- "SELECT *, 
                      hstore_get_value(other_tags, 'access') AS access,
                      hstore_get_value(other_tags, 'amenity') AS amenity,
                      hstore_get_value(other_tags, 'garden:type') AS garden_type,
                      hstore_get_value(other_tags, 'grades') AS grades,
                      hstore_get_value(other_tags, 'healthcare') AS healthcare,
                      hstore_get_value(other_tags, 'isced:level') AS isced_level,
                      hstore_get_value(other_tags, 'landuse') AS landuse,
                      hstore_get_value(other_tags, 'leisure') AS leisure,
                      hstore_get_value(other_tags, 'parking') AS parking,
                      hstore_get_value(other_tags, 'school') AS school,
                      hstore_get_value(other_tags, 'shop') AS shop,
                      hstore_get_value(other_tags, 'sport') AS sport"

  # read in the layers
  points <- oe_read(osmGpkg, query = paste(extra.tag.string, "FROM points"), quiet = TRUE)
  polygons <- oe_read(osmGpkg, query = paste(extra.tag.string, "FROM multipolygons"), quiet = TRUE)
  
  # read in and buffer the region
  region.poly <- st_read(region)
  if (st_crs(region.poly)$epsg != outputCrs) {
    region.poly <- st_transform(region.poly, outputCrs)
  }
  study.area <- st_buffer(region.poly, regionBufferDist)  %>%
    st_snap_to_grid(1)

  # function to extract specific destination types from point or polygon layers ----
  # ----------------------------------#
  # all the tag combination functions in 'getDestinationTypes.R' apply to both
  # points and polygons
  
  destination.layer <- function(layer) {
    return(
      bind_rows(
        getPlayground(layer) %>% mutate(dest_type = "playground"),
        getPark(layer) %>% mutate(dest_type = "park"),
        getSport(layer) %>% mutate(dest_type = "sport"),
        getKindergarten(layer) %>% mutate(dest_type = "kindergarten"),
        getCommunity(layer) %>% mutate(dest_type = "community_centre"),
        getLibrary(layer) %>% mutate(dest_type = "library"),
        getPrimary(layer) %>% mutate(dest_type = "primary_school"),
        getSecondary(layer) %>% mutate(dest_type = "secondary_school"),
        getClinic(layer) %>% mutate(dest_type = "health_clinic"),
        getDentist(layer) %>% mutate(dest_type = "dentist"),
        getPharmacy(layer) %>% mutate(dest_type = "pharmacy"),
        getConvenience(layer) %>% mutate(dest_type = "convenience_store"),
        getSupermarket(layer) %>% mutate(dest_type = "supermarket"),
        getShop(layer) %>% mutate(dest_type = "shop"),
        getPost(layer) %>% mutate(dest_type = "post_office"),
        getBank(layer) %>% mutate(dest_type = "bank"),
        getRestaurant(layer) %>% mutate(dest_type = "restaurant"),
        getCafe(layer) %>% mutate(dest_type = "cafe"),
        getParking(layer) %>% mutate(dest_type = "parking")
      ))
  }
  
  # create tables of point and polygon destinations ----
  # ----------------------------------#
  echo("Finding destinations and their nearby nodes\n")
  
  # create tables for points, polygons and lines (if any), allocate unique id's 
  # (so features with multiple nodes can be grouped by the id where
  # required), and store area and location details
  
  # note - localDestinations are assumed to be POINT, MULTIPOINT, POLYGON,
  # MULTIPOLYGON, LINESTRING or MULTILINESTRING (GEOMETRYCOLLECTION is not handled)
  
  # Points
  echo("Destination point features\n")
  destination.pt.base <- 
    bind_rows(destination.layer(points),
              
              # add PT stops (from GTFS feed) to point table
              getPTStops(city, gtfs_feed, outputCrs, study.area) %>%
                mutate(dest_type = "pt_stop"))
  
  # add in any local destination points
  if (length(localDestinations) > 0) {
    local.pt <- localDestinations %>%
      filter(st_geometry_type(.) == "POINT" |
               st_geometry_type(.) == "MULTIPOINT") %>%
      st_set_geometry("geom")
    
    if (nrow(local.pt) > 0) {
      destination.pt.base <- bind_rows(destination.pt.base,
                                       local.pt)
    }
  }
  
  destination.pt <- destination.pt.base %>%
    mutate(dest_id = row_number(),
           area_m2 = 0,
           centroid_x = st_coordinates(.)[, 1],
           centroid_y = st_coordinates(.)[, 2])  %>%
    st_intersection(study.area %>% st_geometry()) %>%
    filter(!st_is_empty(.))
  
  
  # Polygons
  echo("Destination polygon features\n")
  destination.poly.base <- 
    destination.layer(polygons) %>%
    filter(st_is_valid(geom)) 
  
  # add in any local destination polygons
  if (length(localDestinations) > 0) {
    local.poly <- localDestinations %>%
      filter(st_geometry_type(.) == "POLYGON" |
               st_geometry_type(.) == "MULTIPOLYGON") %>%
      st_set_geometry("geom")
    
    if (nrow(local.poly) > 0) {
      destination.poly.base <- bind_rows(destination.poly.base,
                                       local.poly)
    }
  }
  
  destination.poly <- destination.poly.base %>%
    mutate(dest_id = max(destination.pt$dest_id) + row_number(),
           area_m2 = as.numeric(st_area(.)),
           centroid_x = st_coordinates(st_centroid(.))[, 1],
           centroid_y = st_coordinates(st_centroid(.))[, 2]) %>%
    st_intersection(study.area %>% st_geometry()) %>%
    filter(!st_is_empty(.))
  

  # Lines (local destinations only)
  if (length(localDestinations) > 0) {
    echo("Destination line features\n")
    local.line <- localDestinations %>%
      filter(st_geometry_type(.) == "LINESTRING" |
               st_geometry_type(.) == "MULTILINESTRING") %>%
      st_set_geometry("geom")
    
    if (nrow(local.line) > 0) {
      destination.line <- local.line %>%
        mutate(dest_id = max(destination.poly$dest_id) + row_number(),
               area_m2 = 0,
               centroid_x = st_coordinates(st_centroid(.))[, 1],
               centroid_y = st_coordinates(st_centroid(.))[, 2]) %>%
        st_intersection(study.area %>% st_geometry()) %>%
        filter(!st_is_empty(.))
    }
  } else {
    destination.line <- NULL
  }
  
  
  # # check numbers of each destination type
  # chk <- full_join(destination.poly %>%
  #                    st_drop_geometry() %>%
  #                    group_by(dest_type) %>%
  #                    summarise(poly = n()),
  #                  destination.pt %>%
  #                    st_drop_geometry() %>%
  #                    group_by(dest_type) %>%
  #                    summarise(pt = n()),
  #                  by = "dest_type")
  # if (!is.null(destination.line)) {
  #   chk <- full_join(chk,
  #                    destination.line %>%
  #                      st_drop_geometry() %>%
  #                      group_by(dest_type) %>%
  #                      summarise(line = n()),
  #                    by = "dest_type")
  # }

  
  # find relevant nodes ----
  # For all destinations except parks and schools ('small features'), relevant 
  # node is nearest node to point or to polygon centroid
  
  # For parks and schools ('large features'):
  # - points are buffered to 50m to create a polygon feature,
  # - for buffered points and polygons, relevant nodes are all nodes within the 
  #   feature and terminal nodes of links within 30m of boundary, or if none, 
  #   then nearest node to boundary
  
  # Find nodes available for each mode
  links.cycle <- edges_current %>% filter(is_cycle == 1)
  nodes.cycle <- nodes_current %>% filter(id %in% links.cycle$from_id | id %in% links.cycle$to_id)
  
  links.walk <- edges_current %>% filter(is_walk == 1)
  nodes.walk <- nodes_current %>% filter(id %in% links.walk$from_id | id %in% links.walk$to_id)
  
  links.car <- edges_current %>% filter(is_car == 1)
  nodes.car <- nodes_current %>% filter(id %in% links.car$from_id | id %in% links.car$to_id)
  
  # 'small' features
  dest.small <- bind_rows(destination.pt, destination.poly, destination.line) %>%
    filter(!(dest_type %in% c("park", "primary_school", "secondary_school")))
  # nearest node of each mode
  node_cycle <- nodes.cycle$id[st_nearest_feature(dest.small %>% st_centroid(), nodes.cycle)]
  node_walk <- nodes.walk$id[st_nearest_feature(dest.small %>% st_centroid(), nodes.walk)]
  node_car <- nodes.car$id[st_nearest_feature(dest.small %>% st_centroid(), nodes.car)]
  # join nearest nodes to features
  dest.small.with.nodes <- cbind(dest.small, node_cycle, node_walk, node_car) 
  
  # 'large' features - points
  dest.large.pt <- destination.pt %>%
    filter(dest_type %in% c("park", "primary_school", "secondary_school"))
  # nearest node of each mode
  node_cycle <- nodes.cycle$id[st_nearest_feature(dest.large.pt %>% st_centroid(), nodes.cycle)]
  node_walk <- nodes.walk$id[st_nearest_feature(dest.large.pt %>% st_centroid(), nodes.walk)]
  node_car <- nodes.car$id[st_nearest_feature(dest.large.pt %>% st_centroid(), nodes.car)]
  # join nearest nodes to features
  dest.large.pt.with.nodes <- cbind(dest.large.pt, node_cycle, node_walk, node_car) 

  # 'large' features - polygons
  dest.large.poly <- destination.poly %>%
    filter(dest_type %in% c("park", "primary_school", "secondary_school"))
  
  echo(paste("Finding entry nodes for", nrow(dest.large.poly), "parks and schools\n"))
  
  # internal nodes
  internal.nodes.cycle <- nodes.cycle %>%
    st_intersection(dest.large.poly) %>%
    st_drop_geometry() %>%
    dplyr::select(id, dest_id)
  internal.nodes.walk <- nodes.walk %>%
    st_intersection(dest.large.poly) %>%
    st_drop_geometry() %>%
    dplyr::select(id, dest_id)
  internal.nodes.car <- nodes.car %>%
    st_intersection(dest.large.poly) %>%
    st_drop_geometry() %>%
    dplyr::select(id, dest_id)
  
  # pseudo entry points
  # whether points are within a distance of any of the links, from the distance
  # to their nearest link (buffering the links instead uses too much memory for
  # large networks, such as all of Victoria)
  nearLinks <- function(points, links, dist) {
    nearest <- st_nearest_feature(points, links)
    as.numeric(st_distance(points, st_geometry(links)[nearest], by_element = TRUE)) <= dist
  }
  
  # setup for parallel processing and progress reporting
  cluster <- startCluster(maxcores)
  cores <- length(cluster)
  # stop the cluster if this function exits early with an error
  on.exit(try(stopCluster(cluster), silent = TRUE), add = TRUE)
  pb <- txtProgressBar(max = nrow(dest.large.poly), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report
  echo(paste("Finding boundary points for", nrow(dest.large.poly), 
             "parks and schools; parallel processing with", cores, "cores\n"))
  
  # loop to find list of boundary points
  boundary.points <-
    foreach(i = 1:nrow(dest.large.poly),
            # foreach(i = 1:8,
            # .combine = rbind,
            .packages = c("dplyr", "sf"),
            .options.snow = opts) %dopar% {
              
              dest <- dest.large.poly[i,]
              
              dest.boundary.points <- dest %>% 
                # convert destination polygons boundaries to linestring
                st_cast(to = "MULTILINESTRING") %>%
                st_cast(to = "LINESTRING") %>%
                # locate points at 20m along boundaries
                st_line_sample(., density = units::set_units(20, m))
              
              return(dest.boundary.points)
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  # convert the list of boundary points into a dataframe
  # extract coordinates and ids (row numbers, corresponding to dest.large.poly rows)
  coordinates <- lapply(boundary.points, function(x) st_coordinates(x[[1]]))
  ids <- seq_along(boundary.points)
  # make dataframe of ids and x and y coordinates
  boundary.df <- data.frame(
    id = rep(ids, sapply(coordinates, nrow)),
    x = unlist(lapply(coordinates, function(x) x[,1])),
    y = unlist(lapply(coordinates, function(x) x[,2])))
  # convert to sf object
  boundary.sf <- st_as_sf(boundary.df, coords = c("x", "y")) %>%
    st_set_crs(outputCrs)
  
  # pseudo entry points with nodes
  pseudo.entry.nodes.cycle <- boundary.sf %>%
    # keep only those within 30m of cyclable links
    filter(nearLinks(., links.cycle, 30)) %>%
    # nearest nodes to the entry points
    mutate(node_cycle = nodes.cycle$id[st_nearest_feature(., nodes.cycle)]) %>%
    st_drop_geometry()
  
  pseudo.entry.nodes.walk <- boundary.sf %>%
    # keep only those within 30m of walkable links
    filter(nearLinks(., links.walk, 30)) %>%
    # nearest nodes to the entry points
    mutate(node_walk = nodes.walk$id[st_nearest_feature(., nodes.walk)]) %>%
    st_drop_geometry()
  
  pseudo.entry.nodes.car <- boundary.sf %>%
    # keep only those within 30m of driveable links
    filter(nearLinks(., links.car, 30)) %>%
    # nearest nodes to the entry points
    mutate(node_car = nodes.car$id[st_nearest_feature(., nodes.car)]) %>%
    st_drop_geometry()
  
  # find the internal and pseudo entry nodes of each large destination polygon
  # (and fallback nearest node), and combine; done for all the polygons at once,
  # as a parallel loop copies the whole network's nodes to every worker
  echo(paste("Assembling entry points for", nrow(dest.large.poly), "parks and schools\n"))
  
  entryNodes <- function(internal.nodes, pseudo.entry.nodes, nodes) {
    # nodes within features (identifier is 'dest_id'), then pseudo entry nodes
    # (identifier is 'id', which is row number), grouped by row number
    rows <- c(match(internal.nodes$dest_id, dest.large.poly$dest_id),
              pseudo.entry.nodes$id)
    entry.nodes <- split(c(internal.nodes$id, pseudo.entry.nodes[[2]]),
                         factor(rows, levels = seq_len(nrow(dest.large.poly))))
    entry.nodes <- lapply(entry.nodes, unique)
    
    # fallback if no internal or pseudo entry nodes
    none <- which(lengths(entry.nodes) == 0)
    if (length(none) > 0) {
      entry.nodes[none] <- nodes$id[st_nearest_feature(dest.large.poly[none, ], nodes)]
    }
    
    # convert the entry nodes to strings
    return(sapply(entry.nodes, toString, USE.NAMES = FALSE))
  }
  
  dest.large.poly.with.nodes <- dest.large.poly %>%
    mutate(node_cycle = entryNodes(internal.nodes.cycle, pseudo.entry.nodes.cycle, nodes.cycle),
           node_walk = entryNodes(internal.nodes.walk, pseudo.entry.nodes.walk, nodes.walk),
           node_car = entryNodes(internal.nodes.car, pseudo.entry.nodes.car, nodes.car))
  
  
  # combine all destinations for output ----
  dest.with.nodes <- bind_rows(dest.small.with.nodes %>%
                                 mutate(node_cycle = as.character(node_cycle),
                                        node_walk = as.character(node_walk),
                                        node_car = as.character(node_car)),
                               dest.large.pt.with.nodes %>%
                                 mutate(node_cycle = as.character(node_cycle),
                                        node_walk = as.character(node_walk),
                                        node_car = as.character(node_car)),
                               dest.large.poly.with.nodes) %>%
    relocate(dest_id) %>%
    relocate(dest_type, .after = dest_id) %>%
    relocate(node_cycle, .after = dest_type) %>%
    relocate(node_walk, .after = node_cycle) %>%
    relocate(node_car, .after = node_walk) %>%
    relocate(other_tags, .after = last_col())
  
  return(dest.with.nodes)
  
}

