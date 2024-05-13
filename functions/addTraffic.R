# functions to add traffic to network links

# assumes a network links file, and a second file which is the same (or, at 
# least, contains the same links with the same 'from_id', 'to_id' and 
# highway type), and with a 'total_vol' column containing 1-way daily traffic

# traffic volumes are for each link in its operating direction (so one way
# traffic only); when comparing to two-way traffic volumes in 'addLTS.R', the
# two-way volumes need to be halved

# the multiplier is used where volumes are for a sample of traffic only
# (eg use multiplier of 20 if the volumes are a 5% sample)

addTraffic <- function(network.nodes, network.links, traffic.links, multiplier = 1) {
  
  # testing
  # network.nodes = st_read("./output/test/melbourne_network.sqlite", layer = "nodes)
  # network.links = st_read("./output/test/melbourne_network.sqlite", layer = "links")
  # traffic.links = st_read("./output/test/links_with_traffic.sqlite", layer = "cars_aht")
  # multiplier = 10
  
  # select traffic fields from traffic.links
  traffic <- traffic.links %>%
    dplyr::select(from_id, to_id, highway, length, total_vol) %>%
    st_drop_geometry()
  
  # join traffic to links, in each direction using 'highway' and 'length' as
  # additional join ids: simplification can result in two links sharing nodes (eg loop)
  links.with.traffic <- network.links %>%
    left_join(traffic, by = c("from_id", "to_id", "highway", "length")) %>%
    # convert NAs to zeros, and apply multiplier
    mutate(total_vol = if_else(is.na(total_vol), 0, total_vol)) %>%
    mutate(total_vol = total_vol * multiplier) %>%
    rename(ADT = total_vol) %>%

    # remove duplicates, if any, which have arisen because of original duplicate links,
    # summing their ATDs: group, sum the ATDs, then remove the duplicate with 'distinct'
    group_by(link_id) %>%
    mutate(ADT = sum(ADT)) %>%
    distinct() %>%
    ungroup()
  
  # return network with traffic volumes added
  return(list(network.nodes, links.with.traffic))
  
}


# alternative function to add assumed traffic, where actual/simulated figures
# aren't available, being the lowest volume for each category where used in 'addLTS.R'
addAssumedTraffic <- function(network.nodes, network.links) {
  
  # road groups
  local <- c("residential", "road", "unclassified", "living_street", "service")
  tertiary <- c("tertiary", "tertiary_link")
  secondary <- c("secondary", "secondary_link")
  
  # add assumed traffic - figures from 'addLTS.R' grid, divided by 2 because the grid
  # uses 2-way traffic and these are figures applied to 1-way links
  links.with.traffic <- network.links %>%
    mutate(ADT = case_when(
      highway %in% local     ~ 750 / 2,
      highway %in% tertiary  ~ 3000 / 2,
      highway %in% secondary ~ 10000 / 2,
      TRUE                   ~ NA
    ))
  
  return(list(network.nodes, links.with.traffic))
}


