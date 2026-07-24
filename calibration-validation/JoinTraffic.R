# script to determine links from traffic volumes which correspond to network links

joinTraffic <- function(network.file, traffic.file, region, regionBufferDist) {
  
  # network.file <- "./output/bendigo_network/network.sqlite"
  # traffic.file <- "./data/Traffic_Volume.zip"
  # region <- "./data/greater_bendigo.sqlite"
  # regionBufferDist <- 10000

  # 1 packages and functions ----
  library(tidyverse)
  library(sf)
  library(igraph)
  library(lwgeom)  # st_startpoint/st_endpoint
  source("./functions/readZippedGIS.R")
  
  
  # 2 load network, traffic and region files ----
  
  # network
  network.nodes <- st_read(network.file, layer = "nodes")
  network.links <- st_read(network.file, layer = "links")
  
  # region / study area
  region.poly <- st_read(region)
  if (st_crs(region.poly)$epsg != st_crs(network.links)$epsg) {
    region.poly <- st_transform(region.poly, st_crs(network.links))
  }
  # buffer to 'regionBufferDist' (10km)
  study.area <- st_buffer(region.poly, regionBufferDist)  %>%
    st_snap_to_grid(1)
  
  # traffic, clipped at boundary of study area
  traffic <- read_zipped_GIS(zipfile = traffic.file) %>%
    st_transform(st_crs(network.links)) %>%
    st_intersection(., st_geometry(study.area))


  # 3 filter network to relevant roads ----
  # traffic only applies to secondary roads and higher (reviewed in QGIS),
  # so only secondary and higher in network are required
  selected.links <- network.links %>%
    filter(highway %in% c("motorway", "motorway_link",
                          "trunk", "trunk_link",
                          "primary", "primary_link",
                          "secondary", "secondary_link"))
  
  selected.nodes <- network.nodes %>%
    filter(id %in% selected.links$from_id | id %in% selected.links$to_id)
  
  
  # 4 graph of links ----
  g.links <- selected.links %>%
    st_drop_geometry() %>%
    mutate(weight = length) %>%
    dplyr::select(from_id, to_id, link_id, weight) 
  
  graph <- graph_from_data_frame(g.links, directed = T)
  

  # 5 find network links corresponding to each traffic link ----
  # output table to hold results
  traffic.table <- data.frame()
  
  for (i in 1:nrow(traffic)) {
    
    # traffic link
    traffic.link <- traffic[i,]
    
    # direction - first word in the "flow" column for the road: EAST, WEST, NORTH or SOUTH
    direction <-  stringr::word(traffic.link$FLOW, 1) 
    
    # points and nodes for link start and end points
    point1 <- lwgeom::st_startpoint(traffic.link)
    node1 <- selected.nodes[st_nearest_feature(point1, selected.nodes), ]
    point2 <- lwgeom::st_endpoint(traffic.link)
    node2 <- selected.nodes[st_nearest_feature(point2, selected.nodes), ]
    
    # see whether traffic link is digitised in correct direction
    if (# point1 (startpoint) has lower easting coordinate
      direction == "EAST" & node1$x < node2$x | 
      # point1 (startpoint) has higher easting coordinate
      direction == "WEST" & node1$x > node2$x | 
      # point1 (startpoint) has lower northing coordinate
      direction == "NORTH" & node1$y < node2$y | 
      # point1 (startpoint) has higher northing coordinate
      direction == "SOUTH" & node1$y > node2$y) {
      correct_dir <- 1
    } else {
      correct_dir <- 0
    }
    
    # allocate from and to nodes, according to whether direction is correct
    if (correct_dir == 1) {
      from_node <- node1
      to_node <- node2
    } else {
      from_node <- node2
      to_node <- node1
    }
    
    # find shortest path
    path <- shortest_paths(graph,
                           from = as.character(from_node$id),
                           to = as.character(to_node$id),
                           mode = "out",
                           output = "epath")
    
    if (length(path$epath[[1]]) > 0) {
      path.edges <- edge_attr(graph, "link_id", path$epath[[1]])
    } else {
      path.edges <- c()
    }
    
    # add the edges and corresponding traffic link id to output table
    if (length(path.edges) > 0) {
      for (j in 1:length(path.edges)) {
        traffic.table <- rbind(traffic.table,
                               cbind(link_id = path.edges[j],
                                     OBJECTID = traffic.link$OBJECTID))
      }
    }
  }
  
  # there will be some traffic links where no path is found; these are in
  # the buffer area
  
  
  # 6 clean output ----
  
  # some network links appear in more than one traffic link; typically this 
  # occurs when the nearest node to one end of the traffic link is on the
  # wrong side of a divided road, and a long double-back is required to reach it
  
  # address the duplicates and double-backs by finding the extent by which the
  # length of the found links exceeds the length of the original traffic link,
  # as a percentage of the length of the traffic link; and then, for each 
  # duplicate, allocate it to the link that has the lowest excess
  
  # compare lengths
  # find the total length ('found.length') of the links located for each object ID
  found.link.lengths <- selected.links %>%
    # join the related objectid's
    left_join(traffic.table, by = "link_id") %>%
    # find length for each objectid
    st_drop_geometry() %>%
    group_by(OBJECTID) %>%
    summarise(found.length = sum(length))
  
  # find the length of each traffic link, and compare the found length
  traffic.lengths <- traffic %>%
    dplyr::select(OBJECTID) %>%
    mutate(traffic.link.length = as.numeric(st_length(.))) %>%
    left_join(found.link.lengths, by = "OBJECTID") %>%
    # calculate percentage diff
    mutate(pct.diff = found.length / traffic.link.length * 100)
  
  # duplicates (for information)
  # find the network links which are found for more than one traffic link
  duplicated.links <- traffic.table %>%
    group_by(link_id) %>%
    summarise(n = n()) %>%
    filter(n > 1)  # 82 links

  # remove duplicates from traffic table by keeping only the objectid with min 'pct.diff'
  traffic.table.filtered <- traffic.table %>%
    # join the pct diff figures
    left_join(traffic.lengths %>%
                st_drop_geometry() %>%
                dplyr::select(OBJECTID, pct.diff),
              by = "OBJECTID") %>%
    # find the minimum pct.diff for each link
    group_by(link_id) %>%
    mutate(min.pct.diff = min(pct.diff)) %>%
    ungroup() %>%
    # remove duplicates by keeping the min
    filter(pct.diff == min.pct.diff) %>%
    dplyr::select(-pct.diff, -min.pct.diff)
  

  # 7 finalising ----
  
  # join links to traffic table
  links.with.traffic <- selected.links %>%
    left_join(traffic.table.filtered, by = "link_id") %>%
    left_join(traffic %>% st_drop_geometry, by = "OBJECTID") %>%
    # remove any with no traffic
    filter(!is.na(OBJECTID))
  
  # write output
  st_write(links.with.traffic, "./output/links with DTP traffic volume.sqlite", delete_layer = T)
  
  
  # 8 checking output secondary links AADT > 10k ----
  
  # for LTS purposes, the LTS level for secondary roads (and below, but there are
  # no roads below secondary in the traffic file) depends on AADT, with 10k being
  # the cut-off level for different treatment
  
  # check what secondary links have traffic volumes > 10k
  secondary.high <- links.with.traffic %>%
    filter(highway %in% c("secondary", "secondary_link")) %>%
    filter(ALLVEHS_AA > 10000) %>%
    st_drop_geometry() %>%
    dplyr::select(link_id, highway, OBJECTID, HMGNS_LN_1, FLOW, ALLVEHS_AA) %>%
    arrange(OBJECTID)
  
  secondary.high
  # link_id   highway OBJECTID                                      HMGNS_LN_1             FLOW ALLVEHS_AA
  # 1    6964 secondary     4400 BARNARD STREET btwn DON STREET & EAGLEHAWK ROAD NORTH EAST BOUND      11000
  # 2    6965 secondary     4400 BARNARD STREET btwn DON STREET & EAGLEHAWK ROAD NORTH EAST BOUND      11000
  # 3    8723 secondary     4400 BARNARD STREET btwn DON STREET & EAGLEHAWK ROAD NORTH EAST BOUND      11000
  # 4   11008 secondary     4400 BARNARD STREET btwn DON STREET & EAGLEHAWK ROAD NORTH EAST BOUND      11000
  # 5   11007 secondary    12976 BARNARD STREET btwn DON STREET & EAGLEHAWK ROAD SOUTH WEST BOUND      12000
  # 6   12137 secondary    12976 BARNARD STREET btwn DON STREET & EAGLEHAWK ROAD SOUTH WEST BOUND      12000
}

