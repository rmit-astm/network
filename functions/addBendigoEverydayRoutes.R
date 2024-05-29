# function to join the Bendigo 'everyday routes' file to the network

addBendigoEverydayRoutes <- function(network.nodes,
                                     network.links,
                                     everyday.route.location,
                                     outputCrs) {
  
  # network.nodes = networkOneway[[1]]
  # network.links = networkOneway[[2]]
  # everyday.route.location = bendigoEverydayRoutes
  
  # reporting 
  # -----------------------------------#
  echo("Adding attributes from Bendigo Everyday Routes\n")
  
  # prepare routes 
  # -----------------------------------#
  everyday.routes <- st_read(everyday.route.location)
  
  routes.uncast <- everyday.routes %>%
    st_transform(outputCrs) %>%
    # remove empty geometries
    filter(!is.na(st_dimension(geometry)))
  
  # manual fixes of small gaps/overlaps that cause routing problems
  # replace endpoint of feature 450 with startpoint of feature 451)
  st_geometry(routes.uncast)[450] <- 
    st_linestring(rbind(st_coordinates(st_startpoint(routes.uncast[450,])),
                        st_coordinates(st_startpoint(routes.uncast[451,]))))
  # replace startpoint of feature 288 with endpoint of feature 287
  st_geometry(routes.uncast)[288] <- 
    st_linestring(rbind(st_coordinates(st_endpoint(routes.uncast[287,])),
                        st_coordinates(st_endpoint(routes.uncast[288,]))))
  
  
  # collect start and end points of line segments 
  # -----------------------------------#
  # cast to linestring (splits into more segments, but also drops loops and some offshoots)
  routes.cast <- routes.uncast %>%
    st_cast("MULTILINESTRING") %>%
    st_cast("LINESTRING") %>%
    filter(!is.na(st_dimension(geometry)))
  
  # find self-intersection points
  routes.selfintersected <- routes.uncast %>%
    st_intersection(routes.uncast %>% dplyr::select(Ride_Type_a = Ride_Type)) %>%
    st_collection_extract("POINT") %>%
    st_geometry() %>%
    st_sf()
  
  # start and endpoints for cast and uncast routes & self-intersection points
  start.end.points <- rbind(st_startpoint(routes.uncast) %>% st_sf(),
                            st_endpoint(routes.uncast) %>% st_sf(),
                            st_startpoint(routes.cast) %>% st_sf(),
                            st_endpoint(routes.cast) %>% st_sf(),
                            routes.selfintersected) %>%
    distinct() %>%
    summarise() %>%
    st_buffer(0.01)
  
  
  # split routes at start/end points
  # -----------------------------------#
  
  # create segments, broken at every touching point (but then they need to 
  # be rejoined where the touching point joins only 2 segments)
  route.segments <- routes.uncast %>%
    # split at the start/end points: difference, then multi linestrings to single
    st_difference(., start.end.points) %>%
    st_cast(to = "MULTILINESTRING") %>%
    st_cast(to = "LINESTRING") %>%
    # snap to grid and remove empty geometries
    st_snap_to_grid(1) %>%
    filter(!is.na(st_dimension(geometry)))
  
  
  # find 'intersections' where more than 2 segments touch 
  # -----------------------------------#
  
  # start/end points for the new segments
  route.segment.start.end.points <- 
    rbind(st_startpoint(route.segments) %>% st_sf(),
          st_endpoint(route.segments) %>% st_sf())
  
  # distinct start/end points with id
  route.segment.start.end.points.distinct <- route.segment.start.end.points %>%
    distinct() %>%
    mutate(id = row_number())
  
  # id's of those that join more than 2 segments
  intersection.ids <- route.segment.start.end.points %>%
    # join the identifying number
    st_join(route.segment.start.end.points.distinct, join = st_intersects) %>%
    # find number of segments for each id
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    # keep those where over 2
    filter(n > 2) %>%
    .$id
  
  # intersections: the distinct points where the id's join more than 2 segments
  intersections <- route.segment.start.end.points.distinct %>%
    filter(id %in% intersection.ids) %>%
    summarise() %>%
    st_buffer(0.01)
  
  
  # processing groups into useable linestrings
  # -----------------------------------#
  
  # set up groups
  route.segment.types <- route.segments$Ride_Type %>% unique()
  
  # for each group, combine contiguous sections, then split at intersections, and convert to useful linestrings
  # https://stackoverflow.com/questions/69175360/is-there-a-way-to-merge-contiguous-multilinestrings-sf-object-in-r
  split.routes <- c()
  
  for (i in 1:length(route.segment.types)) {
    route.segment.group <- route.segments %>%
      filter(Ride_Type == route.segment.types[i])
    
    print(paste("Processing group:", route.segment.types[i]))
    
    # find indices of lines that share boundary point
    my_idx_touches <- st_touches(route.segment.group)
    # convert to a graph object
    my_igraph <- graph_from_adj_list((my_idx_touches))
    # find the groups of touching lines
    my_components <- components(my_igraph)$membership
    
    # merge each group of sections that touch each other  
    group.merged <- route.segment.group %>%
      group_by(section = as.character({{my_components}})) %>%
      summarise() %>%
      ungroup()
    
    # split group routes at intersections which connect > 2 points (so those
    # that previously connected at an intersection now won't connect)
    group.split <- group.merged %>%
      st_difference(., intersections) %>%
      st_cast(to = "MULTILINESTRING") %>%
      st_cast(to = "LINESTRING")
    
    # now combine the contiguous sections again - this time, as they've been
    # split at intersections, each should be sections of a single line
    
    # find indices of lines that share boundary point
    my_idx_touches <- st_touches(group.split)
    # convert to a graph object
    my_igraph <- graph_from_adj_list((my_idx_touches))
    # find the groups of touching lines
    my_components <- components(my_igraph)$membership
    
    # merge each group (single lines)
    group.split.merged <- group.split %>%
      group_by(section = as.character({{my_components}})) %>%
      summarise() %>%
      ungroup()
    
    # before converting the multilinestrings to linestrings, extract the 'useful'
    # start/end points that are within [20m] of nodes
    group.linestrings <- group.split.merged %>%
      st_cast("MULTILINESTRING") %>%
      st_cast("LINESTRING")
    group.start.end.points <- 
      rbind(st_startpoint(group.linestrings) %>% st_sf(),
            st_endpoint(group.linestrings) %>% st_sf()) %>%
      distinct() %>%
      mutate(n.node = network.nodes$id[st_nearest_feature(., network.nodes)])
    
    distances <- c()
    for (j in 1:nrow(group.start.end.points)) {
      point <- group.start.end.points[j, ]
      node <- network.nodes %>% filter(id == group.start.end.points$n.node[j])
      distances[j] <- as.numeric(st_distance(point, node))
    }
    
    useful.start.end.points <- group.start.end.points %>%
      cbind(distances) %>%
      filter(distances <= 20) %>%
      summarise() %>%
      st_buffer(0.01)
    
    
    # now where there are multilinestrings,  arrange them as single lines
    for (j in 1:nrow(group.split.merged)) {
      if (st_geometry_type(group.split.merged$geometry[j]) == "MULTILINESTRING") {
        group.split.merged$geometry[j] <- st_line_merge(group.split.merged$geometry[j])
      }
    }
    
    # and split them again at the useful start/end points, so each useful
    # sub-section can be routed separately, and finalise
    group.resplit <- group.split.merged %>%
      st_difference(., useful.start.end.points) %>%
      st_cast(to = "MULTILINESTRING") %>%
      st_cast(to = "LINESTRING") %>%
      # snap to grid
      st_snap_to_grid(1) %>%
      # add ride type and rank (which is used to eliminate duplicate links of 
      # different types and keep only the highest ranked)
      mutate(Ride_Type = route.segment.types[i]) %>%
      mutate(ride_type_rank = case_when(
        Ride_Type == "On Road High Stress"              ~ 1,
        Ride_Type == "One Way - On Road Low Stress"     ~ 2,
        Ride_Type == "On Road Low Stress"               ~ 3,
        Ride_Type == "Natural Trail"                    ~ 4,
        Ride_Type == "Shared Path / Protected Cycleway" ~ 5
      ))
    
    # add to the dataframe
    split.routes <- bind_rows(split.routes, group.resplit)
  }
  
  
  # set up graph
  # -----------------------------------#
  
  # graph for finding distances (directed)
  g.links <- network.links %>%
    st_drop_geometry() %>%
    # reduced weight where cycleway tag, and increased weight if non-cyclable
    mutate(weight = case_when(
      !is.na(cycleway) ~ length * 0.9,
      is_cycle == 0    ~ length * 1.15,
      TRUE             ~ length
    )) %>%
    dplyr::select(from_id, to_id, weight, link_id)
  
  g <- graph_from_data_frame(g.links, directed = T)
  
  
  # routing loop
  # -----------------------------------#
  
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  pb <- txtProgressBar(max = max(nrow(split.routes), 2), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report
  echo(paste("Finding network routes for", nrow(split.routes), "route segments;",
             "parallel processing with", cores, "cores\n"))
  
  # loop to find list of boundary points
  output.links <-
    foreach(i = 1:nrow(split.routes),
            # foreach(i = 1:10,
            .combine = rbind,
            .packages = c("dplyr", "sf", "igraph", "lwgeom"),
            .options.snow = opts) %dopar% {
              
              output.links <- data.frame()
              
              route <- split.routes[i, ]
              route.type <- route$Ride_Type
              route.rank <- route$ride_type_rank
              
              # find start and end nodes (nearest node on nearest), and their distances
              startpoint <- st_startpoint(route)  
              start.link <- network.links[st_nearest_feature(startpoint, network.links), ]
              eligible.start.nodes <- network.nodes %>%
                filter(id == start.link$from_id | id == start.link$to_id)
              start.node <- network.nodes %>%
                filter(id == eligible.start.nodes$id[st_nearest_feature(startpoint, eligible.start.nodes)]) %>%
                .$id
              endpoint <- st_endpoint(route)
              end.link <- network.links[st_nearest_feature(endpoint, network.links), ]
              eligible.end.nodes <- network.nodes %>%
                filter(id == end.link$from_id | id == end.link$to_id)
              end.node <- network.nodes %>%
                filter(id == eligible.end.nodes$id[st_nearest_feature(endpoint, eligible.end.nodes)]) %>%
                .$id
              start.dist <- st_distance(startpoint, network.nodes %>% filter(id == start.node)) %>%
                as.numeric()
              end.dist <- st_distance(endpoint, network.nodes %>% filter(id == end.node)) %>%
                as.numeric()
              
              # if distance from startpoint or endpoint to nearest node exceeds 50m, then exit
              if (round(start.dist) > 150 | round(end.dist) > 150) {
                return(output.links)
              }
              
              if (start.node != end.node) {
                
                # outward direction
                
                # find shortest path and its edges
                path <- shortest_paths(g,
                                       from = as.character(start.node),
                                       to = as.character(end.node),
                                       mode = "out",
                                       output = "epath")  
                
                path.edges <- edge_attr(g, "link_id", path$epath[[1]])
                
                if (length(path.edges) > 0) {
                  for (j in 1:length(path.edges)) {
                    output.row <- cbind(link_id = path.edges[j],
                                        everyday_type = route.type,
                                        rank = route.rank) %>%
                      as.data.frame()
                    output.links <- bind_rows(output.links, output.row)
                  }
                } 
                
                # reverse direction, except where one way
                if (route.type != "One Way - On Road Low Stress") {
                  # find shortest path and its edges
                  path <- shortest_paths(g,
                                         from = as.character(start.node),
                                         to = as.character(end.node),
                                         mode = "in",
                                         output = "epath")  
                  
                  path.edges <- edge_attr(g, "link_id", path$epath[[1]])
                  
                  if (length(path.edges) > 0) {
                    for (j in 1:length(path.edges)) {
                      output.row <- cbind(link_id = path.edges[j],
                                          everyday_type = route.type,
                                          rank = route.rank) %>%
                        as.data.frame()
                      output.links <- bind_rows(output.links, output.row)
                    }
                  } 
                }
              }
              
              return(output.links)
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  
  # selecting the links matching the routes
  # -----------------------------------#
  
  # distinct output links
  output.links.distinct <- output.links %>%
    distinct() %>%
    mutate(link_id = as.integer(link_id),
           rank = as.integer(rank)) %>%
    # if there are duplicate links with different ride types, select the highest rank
    group_by(link_id) %>%
    filter(rank == max(rank)) %>%
    ungroup() %>%
    # remove the rank column
    dplyr::select(-rank)
  
  
  # join with links to produce output
  # -----------------------------------#
  
  # join everyday details to links
  links.with.everyday <- network.links %>%
    left_join(output.links.distinct %>%
                dplyr::select(link_id, bend_everyday_type = everyday_type),
              by = "link_id")
  
  
  # # check whether any links with an everyday_type are is_cycle=0
  # chk <- links.with.everyday %>%
  #   filter(!is.na(bend_everyday_type)) %>%
  #   filter(is_cycle == 0)  # there are 37 of these
  
  # alter is_cycle and mode to match those that are everyday links
  for (i in 1:nrow(links.with.everyday)) {
    if (!is.na(links.with.everyday$bend_everyday_type[i]) & 
        links.with.everyday$is_cycle[i] == 0) {
      links.with.everyday$is_cycle[i] <- 1
      link.modes <- links.with.everyday$modes[i]
      links.with.everyday$modes[i] <- case_when(
        link.modes == "car"          ~ "car,bike",
        link.modes == "walk"         ~ "bike,walk",
        link.modes == "bus"          ~ "bike,bus",
        link.modes == "car,walk"     ~ "car,bike,walk",
        link.modes == "car,bus"      ~ "car,bike,bus",
        link.modes == "walk,bus"     ~ "bike,walk,bus",
        link.modes == "car,walk,bus" ~ "car,bike,walk,bus"
      )
      # print(paste("row no", i, "now has modes", links.with.everyday$modes[i]))
    }
  }
  
  # # comparision of everyday route and cycleway
  # comparison.table <- links.with.everyday %>%
  #   filter(!is.na(everyday_type)) %>%
  #   st_drop_geometry() %>%
  #   group_by(everyday_type, cycleway) %>%
  #   summarise(n = n())
  
  
  return(links.with.everyday)
  
}

