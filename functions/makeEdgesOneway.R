# function to convert two-way edges to one-way

makeEdgesOneway <- function(nodes_current, edges_current) {
  
  # testing
  # nodes_current <- input.nodes
  # edges_current <- input.links
  
  # ensure fromx, fromy, tox and toy column names are lower case (eg not 'fromX')
  names.to.change <- c("fromX", "fromY", "toX", "toY")
  edges_current <- rename_with(edges_current, tolower, any_of(names.to.change))
  
  # for two-way, divide permlanes by 2, rounded up (as they will be split into 2 * one-way)
  edges_current <- edges_current %>%
    mutate(permlanes = ifelse(is_oneway == 0, ceiling(permlanes / 2), permlanes))

  # select only two-way edges
  edges_twoway <- edges_current %>%
    filter(is_oneway == 0)
  
  # swap from/to details
  edges_twoway_reversed <- edges_twoway %>%
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
           toy = orig_fromy)
  
  # if elevation is present, use the reverse slope
  if("rvs_slope_pct" %in% colnames(edges_twoway_reversed)) {
    edges_twoway_reversed <- edges_twoway_reversed %>%
      mutate(slope_pct = rvs_slope_pct)
  }
  
  # select required fields (excluding 'is_oneway') [note that "id" is not 
  # retained here - it is replaced by link_id]
  required_fields <- c("from_id", "to_id", "fromx", "fromy", "tox", "toy",
                       "length", "freespeed", "permlanes", "capacity", "highway",
                       "cycleway", "surface", "is_cycle", "is_walk", "is_car", 
                       "modes")
  if ("slope_pct" %in% colnames(edges_twoway_reversed)) {
    required_fields <- c(required_fields, "slope_pct")
  }
  if ("osm_id" %in% colnames(edges_twoway_reversed)) {
    required_fields <- c(required_fields, "osm_id")
  }
  ndvi_columns <- colnames(edges_twoway_reversed)[grep("ndvi", colnames(edges_twoway_reversed))]
  if (length(ndvi_columns) > 0) {
    required_fields <- c(required_fields, ndvi_columns)
  }
  tcc_columns <- colnames(edges_twoway_reversed)[grep("tcc", colnames(edges_twoway_reversed))]
  if (length(tcc_columns) > 0) {
    required_fields <- c(required_fields, tcc_columns)
  }
  
  # update edges for bikelane columns: for one-way edges or forward direction of
  # two-way, fwdLeft and fwdRight become left and right; for reverse direction 
  # of two-way edges, rvsLeft becomes left; also update required fields
  bikelaneUpdateOutputs <- 
    updateBikelaneEdges(edges_current, edges_twoway_reversed, required_fields)
  edges_current <- bikelaneUpdateOutputs[[1]]
  edges_twoway_reversed <- bikelaneUpdateOutputs[[2]]
  required_fields <- bikelaneUpdateOutputs[[3]]
  
  edges_twoway_reversed <- edges_twoway_reversed %>%
    dplyr::select(any_of(required_fields))
  
  # modify original edges to rename fwd_slope_pct if present
  if ("fwd_slope_pct" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(slope_pct = fwd_slope_pct)
  }
  
  # bind with reversed two-way edges
  edges_current <- edges_current %>%
    dplyr::select(any_of(required_fields)) %>%
    bind_rows(., edges_twoway_reversed)
  
  # add link_id, based on rownumber (at the end, not beginning, because igraph 
  # requires from_id and to_id to be the first two columns)
  edges_current <- edges_current %>%
    mutate(link_id = row_number())
  
  # clean up bikelane columns
  edges_current <- cleanBikelaneColumns(edges_current)
  
  return(list(nodes_current, edges_current))
}


# function to update edges for bikelane columns: for one-way edges or forward 
# direction of two-way, fwdLeft and fwdRight become left and right; 
# for reverse direction for two-way edges, rvsLeft becomes left
updateBikelaneEdges <- function(edges_current, 
                                edges_twoway_reversed, 
                                required_fields) {
  
  # one-way edges and foward direction of two_way  
  if ("bikelaneFwdLeft" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelaneLeft = bikelaneFwdLeft)
    if (!"bikelaneLeft" %in% required_fields) {
      required_fields <- c(required_fields, "bikelaneLeft")
    }
  }
  if ("bikelaneFwdRight" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelaneRight = bikelaneFwdRight)
    if (!"bikelaneRight" %in% required_fields) {
      required_fields <- c(required_fields, "bikelaneRight")
    }
  }
  if ("bikelaneWidthFwdLeft" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelaneLeftWidth = bikelaneWidthFwdLeft)
    if (!"bikelaneLeftWidth" %in% required_fields) {
      required_fields <- c(required_fields, "bikelaneLeftWidth")
    }
  }
  if ("bikelaneWidthFwdRight" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelaneRightWidth = bikelaneWidthFwdRight)
    if (!"bikelaneRightWidth" %in% required_fields) {
      required_fields <- c(required_fields, "bikelaneRightWidth")
    }
  }
  if ("bikelaneTrafFwdLeft" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelaneLeftTraf = bikelaneTrafFwdLeft)
    if (!"bikelaneLeftTraf" %in% required_fields) {
      required_fields <- c(required_fields, "bikelaneLeftTraf")
    }
  }
  if ("bikelaneTrafFwdRight" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelaneRightTraf = bikelaneTrafFwdRight)
    if (!"bikelaneRightTraf" %in% required_fields) {
      required_fields <- c(required_fields, "bikelaneRightTraf")
    }
  }
  
  # reverse direction of two-way
  if ("bikelaneRvsLeft" %in% colnames(edges_twoway_reversed)) {
    edges_twoway_reversed <- edges_twoway_reversed %>%
      mutate(bikelaneLeft = bikelaneRvsLeft)
    if (!"bikelaneLeft" %in% required_fields) {
      required_fields <- c(required_fields, "bikelaneLeft")
    }
  }
  if ("bikelaneWidthRvsLeft" %in% colnames(edges_twoway_reversed)) {
    edges_twoway_reversed <- edges_twoway_reversed %>%
      mutate(bikelaneLeftWidth = bikelaneWidthRvsLeft)
    if (!"bikelaneLeftWidth" %in% required_fields) {
      required_fields <- c(required_fields, "bikelaneLeftWidth")
    }
  }
  if ("bikelaneTrafRvsLeft" %in% colnames(edges_twoway_reversed)) {
    edges_twoway_reversed <- edges_twoway_reversed %>%
      mutate(bikelaneLeftTraf = bikelaneTrafRvsLeft)
    if (!"bikelaneLeftTraf" %in% required_fields) {
      required_fields <- c(required_fields, "bikelaneLeftTraf")
    }
  }
  
  return(list(edges_current, edges_twoway_reversed, required_fields))
} 

# function to clean up bikelane columns
cleanBikelaneColumns <- function(edges_current) {
  bikelaneinfra_columns <- c("bikelaneLeft", "bikelaneRight")
  
  bikelaneinfra_clean <- function(x) {
    # remove leading and trailing commas
    x <- gsub("^,|,$", "", x)
    # remove 'no' or 'yes' when combined with another tag
    x <- gsub("no,|,no", "", x)
    x <- gsub("yes,|,yes", "", x)
    # remove 'lane' when combined with another tag (there are very few of these)
    x <- gsub("lane,|,lane", "", x)
    # replace empty strings with NA
    x <- ifelse(x == "", NA, x)
    return(x)
  }
  
  for(column in bikelaneinfra_columns) {
    edges_current[[column]] <- bikelaneinfra_clean(edges_current[[column]])
  }
  
  return(edges_current)
}
