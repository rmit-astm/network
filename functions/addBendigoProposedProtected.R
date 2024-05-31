# functions to join the Bendigo 'proposed protected network' routes to the network

# includes a 'bend_protected_newlink' attribute, which is 0 if the link exists
# and is used in the proposed protected network, or 1 if the link is a new link
# created to be part of the proposed protected network (or is NA otherwise)

addBendigoProposedProtected <- function(network.nodes,
                                        network.links,
                                        proposed.protected.location,
                                        defaults_df,
                                        addNDVI, ndviFile, ndviBuffDist, 
                                        addTreeCanopyCover, treeCanopyCoverFile,
                                        addElevation,
                                        outputCrs) {
  
  # network.nodes = networkOneway[[1]]
  # network.links = networkOneway[[2]]
  # proposed.protected.location = bendigoProposedProtected
  
  # reporting 
  # -----------------------------------#
  echo("Adding attributes from Bendigo Proposed Protected Network\n")
  
  
  # load routes 
  # -----------------------------------#
  protected.routes <- st_read(proposed.protected.location)
  

  # set up graph
  # -----------------------------------#
  
  # graph for finding distances (directed)
  g.links <- network.links %>%
    st_drop_geometry() %>% 
    # adjust weight, to encourage major roads over minor & roads over footpaths
    mutate(weight = case_when(
      highway == "service" ~ length * 1.2,
      highway == "footway" ~ length * 1.4,
      TRUE                 ~ length
    )) %>%
    dplyr::select(from_id, to_id, weight, link_id)
  
  g <- graph_from_data_frame(g.links, directed = T)
  
  
  # routing loop (small job for parallel; but structure matches addBendigoEverydayRoutes)
  # -----------------------------------#
  
  cores <- detectCores()
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  pb <- txtProgressBar(max = max(nrow(protected.routes), 2), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report
  echo(paste("Finding network routes for", nrow(protected.routes), "route segments on existing links;",
             "parallel processing with", cores, "cores\n"))
  
  # loop to find list of boundary points
  output.links <-
    foreach(i = 1:nrow(protected.routes),
            # foreach(i = 1:10,
            .combine = rbind,
            .packages = c("dplyr", "sf", "igraph", "lwgeom"),
            .options.snow = opts) %dopar% {
              
              output.links <- data.frame()
              
              route <- protected.routes[i, ]
              newlink.status <- route$new_link
              
              # only proceed where newlink status is 0 (ie existing links, not new links
              # which have been added separately) - otherwise return empty output
              if (newlink.status == 0) {
                
                # find start and end nodes
                startpoint <- st_startpoint(route)  
                endpoint <- st_endpoint(route)
                start.node <- network.nodes$id[st_nearest_feature(startpoint, network.nodes)]
                end.node <- network.nodes$id[st_nearest_feature(endpoint, network.nodes)]
                
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
                                          newlink_status = newlink.status) %>%
                        as.data.frame()
                      output.links <- bind_rows(output.links, output.row)
                    }
                  } 
                  
                  # reverse direction,
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
                                          newlink_status = newlink.status) %>%
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
    mutate(link_id = as.integer(link_id))
  
  
  # join with links to produce existing link output
  # -----------------------------------#
  
  # join everyday details to links
  links.with.proposed.existing <- network.links %>%
    left_join(output.links.distinct %>%
                dplyr::select(link_id, bend_protected_newlink = newlink_status),
              by = "link_id")
  

  # extract and process the new links
  # -----------------------------------#
  
  # extract new links
  newlinks <- protected.routes %>%
    filter(new_link == 1) %>%
    st_set_geometry("geom") %>%
    dplyr::select(-new_link)
  
  echo(paste("Creating new links for", nrow(newlinks), "route segments\n"))
       
  # extract details of start and end nodes, and bind
  startpoints <- st_startpoint(newlinks) %>% 
    as.data.frame() %>% st_sf() %>% st_set_crs(outputCrs)
  startnode.details <- network.nodes[st_nearest_feature(startpoints, network.nodes), ] %>%
    st_drop_geometry() %>% dplyr::select(from_id = id, fromx = x, fromy = y)
  endpoints <- st_endpoint(newlinks) %>% 
    as.data.frame() %>% st_sf() %>% st_set_crs(outputCrs)
  endnode.details <- network.nodes[st_nearest_feature(endpoints, network.nodes), ] %>%
    st_drop_geometry() %>% dplyr::select(to_id = id, tox = x, toy = y)
  
  newlinks <- cbind(newlinks, startnode.details, endnode.details)
  
  # add other attributes
  newlinks <- newlinks %>%
    mutate(length = as.numeric(st_length(geom)),
           # default cycleway attributes
           freespeed = defaults_df$freespeed[16], 
           permlanes = defaults_df$permlanes[16],
           capacity = defaults_df$laneCapacity[16],  # to be reviewed - one way/two way?
           highway = "proposed_cycleway",
           # all modes set to 0, so not routeable unless changed
           is_cycle = 0,
           is_walk = 0,
           is_car = 0,
           # newlink status
           bend_protected_newlink = 1)

  # add equivalent links in reverse direction
  newlinks.reversed <- newlinks %>%
    # store original from/to details
    mutate(orig_from_id = from_id,
           orig_to_id = to_id,
           orig_fromx = fromx,
           orig_fromy = fromy,
           orig_tox = tox,
           orig_toy = toy) %>%
    # swap from/to
    mutate(from_id = orig_to_id,
           to_id = orig_from_id,
           fromx = orig_tox,
           fromy = orig_toy,
           tox = orig_fromx,
           toy = orig_fromy) %>%
    # remove the new fields
    dplyr::select(-orig_from_id, -orig_to_id, 
                  -orig_fromx, -orig_fromy, -orig_tox, -orig_toy)
  
  newlinks <- bind_rows(newlinks, newlinks.reversed) %>%
    # add sequential link_id
    mutate(link_id = max(network.links$link_id) + row_number())
  
  # add NDVI, tree canopy cover and elevation, if required
  if(addNDVI) { 
    newlinks <- addNDVI2Links(newlinks,
                              ndviFile, 
                              ndviBuffDist, 
                              outputCrs)  
  }
  
  if(addTreeCanopyCover) { 
    # 'highway order' required: set to 16 (cycleway) temporarily
    newlinks$highway_order <- 16
    newlinks <- addTreeCanopyCover2Links(newlinks,
                                         treeCanopyCoverFile, 
                                         outputCrs)  
    newlinks <- newlinks %>% dplyr::select(-highway_order, 
                                           -tree_pixels, -total_pixels)
  }
  
  if(addElevation){ 
    # 'is_oneway' required: set to 1 (yes) temporarily
    newlinks$is_oneway <- 1
    newlinks <- addElevation2Links(list(network.nodes, newlinks))
    # rename 'fwd_slope_pct' and drop 'rvs_slope_pc
    newlinks <- newlinks %>% 
      rename(slope_pct = fwd_slope_pct) %>%
      dplyr::select(-is_oneway, -rvs_slope_pct)
  }
  
  
  # join existing and new links
  # -----------------------------------#

  links.with.proposed.protected <- bind_rows(links.with.proposed.existing,
                                             newlinks)
  
  return(links.with.proposed.protected)
  
}

