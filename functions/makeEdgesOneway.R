# function to convert two-way edges to one-way

makeEdgesOneway <- function(nodes_current, edges_current) {
  
  # testing
  # nodes_current <- input.nodes
  # edges_current <- input.links
  
  # ensure fromx, fromy, tox and toy column names are lower case (eg not 'fromX')
  names.to.change <- c("fromX", "fromY", "toX", "toY")
  edges_current <- rename_with(edges_current, tolower, any_of(names.to.change))
  
  # for two-way, divide permlanes and capacity by 2, rounded up (as they will be split into 2 * one-way)
  edges_current <- edges_current %>%
    mutate(permlanes = ifelse(is_oneway == 0, ceiling(permlanes / 2), permlanes),
           capacity = ifelse(is_oneway == 0, ceiling(capacity / 2), capacity))

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
  
  # process bikelane columns
  edges_current <- processBikelaneColumns(edges_current)
  
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
      rename(bikelane_left = bikelaneFwdLeft)
    if (!"bikelane_left" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_left")
    }
  }
  if ("bikelaneFwdRight" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelane_right = bikelaneFwdRight)
    if (!"bikelane_right" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_right")
    }
  }
  if ("bikelaneWidthFwdLeft" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelane_left_width = bikelaneWidthFwdLeft)
    if (!"bikelane_left_width" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_left_width")
    }
  }
  if ("bikelaneWidthFwdRight" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelane_right_width = bikelaneWidthFwdRight)
    if (!"bikelane_right_width" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_right_width")
    }
  }
  if ("bikelaneTrafFwdLeft" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelane_left_traf = bikelaneTrafFwdLeft)
    if (!"bikelane_left_traf" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_left_traf")
    }
  }
  if ("bikelaneTrafFwdRight" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelane_right_traf = bikelaneTrafFwdRight)
    if (!"bikelane_right_traf" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_right_traf")
    }
  }
  if ("bikelaneBuffFwdLeft" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelane_left_buff = bikelaneBuffFwdLeft)
    if (!"bikelane_left_buff" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_left_buff")
    }
  }
  if ("bikelaneBuffFwdRight" %in% colnames(edges_current)) {
    edges_current <- edges_current %>%
      rename(bikelane_right_buff = bikelaneBuffFwdRight)
    if (!"bikelane_right_buff" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_right_buff")
    }
  }
  
  # reverse direction of two-way
  if ("bikelaneRvsLeft" %in% colnames(edges_twoway_reversed)) {
    edges_twoway_reversed <- edges_twoway_reversed %>%
      mutate(bikelane_left = bikelaneRvsLeft)
    if (!"bikelane_left" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_left")
    }
  }
  if ("bikelaneWidthRvsLeft" %in% colnames(edges_twoway_reversed)) {
    edges_twoway_reversed <- edges_twoway_reversed %>%
      mutate(bikelane_left_width = bikelaneWidthRvsLeft)
    if (!"bikelane_left_width" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_left_width")
    }
  }
  if ("bikelaneTrafRvsLeft" %in% colnames(edges_twoway_reversed)) {
    edges_twoway_reversed <- edges_twoway_reversed %>%
      mutate(bikelane_left_traf = bikelaneTrafRvsLeft)
    if (!"bikelane_left_traf" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_left_traf")
    }
  }
  if ("bikelaneBuffRvsLeft" %in% colnames(edges_twoway_reversed)) {
    edges_twoway_reversed <- edges_twoway_reversed %>%
      mutate(bikelane_left_buff = bikelaneBuffRvsLeft)
    if (!"bikelane_left_buff" %in% required_fields) {
      required_fields <- c(required_fields, "bikelane_left_buff")
    }
  }
  
  return(list(edges_current, edges_twoway_reversed, required_fields))
} 

# function to process bikelane columns into required form
processBikelaneColumns <- function(edges_current) {
  
  # clean infrastructure columns to simplify conflicting categories
  bikelaneinfra_columns <- c("bikelane_left", "bikelane_right")
  
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
  
  for (i in 1:length(bikelaneinfra_columns)) {
    column <- bikelaneinfra_columns[i]
    if (column %in% names(edges_current)) {
      edges_current[[column]] <- bikelaneinfra_clean(edges_current[[column]])
    }
  }
  
  # split traffic and buffer columns into left and right
  bikelanetraffic_columns <- c("bikelane_left_traf", "bikelane_right_traf")
  bikelanebuffer_columns <- c("bikelane_left_buff", "bikelane_right_buff")
  cols_to_split <- c(bikelanetraffic_columns, bikelanebuffer_columns)
  
  for (i in 1:length(cols_to_split)) {
    column <- cols_to_split[i]
    
    # new column names
    left_col <- paste0(column, "_left")
    right_col <- paste0(column, "_right")
    
    edges_current <- edges_current %>%
      
      # populate the new left and right columns
      mutate(!!left_col := case_when(
        # values for 'both' or 'left' tags
        stringr::str_detect(!!sym(column), "both") ~ str_extract(!!sym(column), "(?<=both=)[^,]*"),  # from 'both=' up to following comma or end
        stringr::str_detect(!!sym(column), "left") ~ str_extract(!!sym(column), "(?<=left=)[^,]*"),  # from 'left=' up to following comma or end
        TRUE ~ NA
      ),
      !!right_col := case_when(
        # values for 'both' or 'right' tags
        stringr::str_detect(!!sym(column), "both") ~ str_extract(!!sym(column), "(?<=both=)[^,]*"),  # from 'both=' up to following comma or end
        stringr::str_detect(!!sym(column), "right") ~ str_extract(!!sym(column), "(?<=right=)[^,]*"),  # from 'right=' up to following comma or end
        TRUE ~ NA
      )) %>%
      
      # remove the former column
      dplyr::select(-!!sym(column))
    
    # convert buffer columns to numeric
    if (column %in% bikelanebuffer_columns) {
      edges_current <- edges_current %>%
        mutate(!!left_col := as.numeric(!!sym(left_col)),
               !!right_col := as.numeric(!!sym(right_col)))
    }
  }

  return(edges_current)
}
