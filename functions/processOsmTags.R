#########################################################
# OSM TAG         | Network Name       | Bike hierarchy |
#-----------------|--------------------|----------------|
# cycleway        | bikepath           | 5              |
# cycleway + walk | shared_path        | 4              | 
# track           | separated_lane     | 3              |
# lane            | simple_lane        | 2              |
# shared_lane     | shared_street      | 1              |
# -               | no lane/track/path | 0              |
#########################################################

processOsmTags <- function(osm_df, this_defaults_df, simplifyEdges){
  # osm_df <- osm_metadata
  # this_defaults_df <- defaults_df
  
  osmWithDefaults <- inner_join(osm_df,this_defaults_df,by="highway")
  
  # pre splitting the tags to save time: replace tag separator "," with unique
  # symbol ',,'; replace key:value separator "=>" with same unique symbol ',,'; 
  # remove " at start and end of tag string; then split at the unique symbol ',,'
  tagList <-strsplit(gsub('"', '', gsub('"=>"', ',,', gsub('","', '",,"', osmWithDefaults$other_tags))), ',,')
  
  osmWithDefaults <- osmWithDefaults %>%
    mutate(cycleway=ifelse(highway=="cycleway",4,0)) %>%
    dplyr::select(osm_id,highway,highway_order,freespeed,permlanes,laneCapacity,is_oneway,cycleway,is_cycle,is_walk,is_car)

  getMetadataInfo <- function(i) {
    df <- osmWithDefaults[i,]
    tags=tagList[[i]]

    if (length(tags)>1) {
      
      # keys and values are odd and even-numbered tags respectively
      keys <- tags[seq(1, length(tags), by = 2)]
      values <- tags[seq(2, length(tags), by = 2)]
      
      cycleway_tags <- values[which(keys %like% "cycleway")]
      if(any(is.na(cycleway_tags))) cycleway_tags <- c()
      bicycle_tags <- values[which(keys=="bicycle")]
      if(any(is.na(bicycle_tags))) bicycle_tags <- c()
      car_tags <- values[which(keys %in% c("car","motor_vehicle"))]
      if(any(is.na(car_tags))) car_tags <- c()
      foot_tags <- values[which(keys %like% "foot")]
      if(any(is.na(foot_tags))) foot_tags <- c()
      surface_tags <- values[which(keys=="surface")]
      if(any(is.na(surface_tags))) surface_tags <- c()
      oneway_tags <-  as.character(values[which(keys=="oneway")])
      if(length(oneway_tags)==0) oneway_tags <- c()
      
      if("maxspeed" %in% keys) {
        maxSpeed=as.integer(values[which(keys=="maxspeed")])
        # added this as some links had weird "masxspeed" values such as 500km/h!
        # 150km/h limit might cause issues for autobahns in Germany, AJ Jan 2021.
        if(!(is.na(maxSpeed)) & 140 < maxSpeed){
          message("Skiping speeds higher than 140km/h from OSM, consider editing processOSMTags.R if higher speeds are desired - AJ Jan 2021.")
          freeSpeed <- NA
        }else{
          freeSpeed=maxSpeed/3.6
        }
        # added is.na since one of the maxspeed has a value of "50; 40"
        if(!is.na(freeSpeed)) {
          df$freespeed[1]=freeSpeed
        }
      }
      
      df$surface[1]=surface_tags
      if(any(oneway_tags=="yes")) df$is_oneway[1]=1
      #if(any(bicycle_tags %in% c("yes","designated"))) df$cycleway[1]="unmarked"
      if(any(cycleway_tags=="shared_lane")) df$cycleway[1]=1
      if(any(cycleway_tags=="lane") & df$highway[1]!="cycleway") df$cycleway[1]=2
      if(any(cycleway_tags=="track")& df$highway[1]!="cycleway") df$cycleway[1]=3
      if(any(foot_tags=="no")& df$highway[1]=="cycleway") df$cycleway[1]=5
      if(any(car_tags=="no")) df$is_car[1]=0
      if(df$is_car[1]==0 & any(bicycle_tags %in% c("yes", "designated")) & df$cycleway[1]<5) df$cycleway[1]=4
      if(any(foot_tags=="no")) df$is_walk[1]=0
      if(any(foot_tags %in% c("yes","designated"))) df$is_walk[1]=1
      if(df$cycleway[1]>0 | any(bicycle_tags %in% c("yes","designated"))) df$is_cycle[1]=1
      if(any(bicycle_tags %in% "no")) df$is_cycle[1]=0
      
      if ("lanes" %in% keys) {
        # lanes is number of tagged lanes
        taggedLanes = as.integer(values[which(keys == "lanes")])
        df$permlanes[1] = taggedLanes
      } else {
        # lanes is default number, multiplied by 2 if two-way
        newLanes = ifelse(df$is_oneway[1] == 0, df$permlanes[1] * 2, df$permlanes[1])
        df$permlanes[1] = newLanes
      }
      
      # add Melbourne Bikelane Project tags, if not simplifying edges
      if (!simplifyEdges) df <- getBikelaneProjectTags(df, keys, values)
      
    }
    return(df)
  }

  osmAttributed <- lapply(1:nrow(osmWithDefaults),getMetadataInfo) %>%
    bind_rows() %>%
    # looks like the ones with no modes are mostly closed walking or cycling tracks
    filter(is_cycle+is_walk+is_car>0)
    
  return(osmAttributed)
}


# Melbourne bike lane project details
# see https://wiki.openstreetmap.org/wiki/Melbourne_Bike_Lane_Project
getBikelaneProjectTags <- function(df, keys, values) {
  
  # cycleway:[side] tags - infrastructure type
  ## empty vectors for the tags
  bikelaneFwdLeft <- c()
  bikelaneFwdRight <- c()
  bikelaneRvsLeft <- c()
  ## extract the tags
  if (df$is_oneway[1] == 1) {
    # for one way, left and right values are forward left and right
    if (any(keys == "cycleway:left")) {
      tag <- values[which(keys == "cycleway:left")]
      bikelaneFwdLeft <- c(bikelaneFwdLeft, tag)
    }
    if (any(keys == "cycleway:right")) {
      tag <- values[which(keys == "cycleway:right")]
      bikelaneFwdRight <- c(bikelaneFwdRight, tag)
    }
    if (any(keys == "cycleway:both")) {
      tag <- values[which(keys == "cycleway:both")]
      bikelaneFwdLeft <- c(bikelaneFwdLeft, tag)
      bikelaneFwdRight <- c(bikelaneFwdRight, tag)
    }
  } else {
    # for two way, left values are forward left and right values are reverse left
    if (any(keys == "cycleway:left")) {
      tag <- values[which(keys == "cycleway:left")]
      bikelaneFwdLeft <- c(bikelaneFwdLeft, tag)
    }
    if (any(keys == "cycleway:right")) {
      tag <- values[which(keys == "cycleway:right")]
      bikelaneRvsLeft <- c(bikelaneRvsLeft, tag)
    }
    if (any(keys == "cycleway:both")) {
      tag <- values[which(keys == "cycleway:both")]
      bikelaneFwdLeft <- c(bikelaneFwdLeft, tag)
      bikelaneRvsLeft <- c(bikelaneRvsLeft, tag)
    }
  }
  ## add the infrastructure tags to the df
  df$bikelaneFwdLeft[1] <- 
    ifelse(is.null(bikelaneFwdLeft), NA, 
           stringr::str_flatten(unique(bikelaneFwdLeft), collapse = ","))
  df$bikelaneFwdRight[1] <- 
    ifelse(is.null(bikelaneFwdRight), NA, 
           stringr::str_flatten(unique(bikelaneFwdRight), collapse = ","))
  df$bikelaneRvsLeft[1] <- 
    ifelse(is.null(bikelaneRvsLeft), NA, 
           stringr::str_flatten(unique(bikelaneRvsLeft), collapse = ","))
  
  # cycleway:[side]:width tags - width of bike lane
  ## empty vectors for the tags
  bikelaneWidthFwdLeft <- c()
  bikelaneWidthFwdRight <- c()
  bikelaneWidthRvsLeft <- c()
  if (df$is_oneway[1] == 1) {
    # for one way, left and right values are forward left and right
    if (any(keys == "cycleway:left:width")) {
      tag <- values[which(keys == "cycleway:left:width")] %>% as.numeric()
      bikelaneWidthFwdLeft <- c(bikelaneWidthFwdLeft, tag)
    }
    if (any(keys == "cycleway:right:width")) {
      tag <- values[which(keys == "cycleway:right:width")] %>% as.numeric()
      bikelaneWidthFwdRight <- c(bikelaneWidthFwdRight, tag)
    }
    if (any(keys == "cycleway:both:width")) {
      tag <- values[which(keys == "cycleway:both:width")] %>% as.numeric()
      bikelaneWidthFwdLeft <- c(bikelaneWidthFwdLeft, tag)
      bikelaneWidthFwdRight <- c(bikelaneWidthFwdRight, tag)
    }
  } else {
    # for two way, left values are forward left and right values are reverse left
    if (any(keys == "cycleway:left:width")) {
      tag <- values[which(keys == "cycleway:left:width")] %>% as.numeric()
      bikelaneWidthFwdLeft <- c(bikelaneWidthFwdLeft, tag)
    }
    if (any(keys == "cycleway:right:width")) {
      tag <- values[which(keys == "cycleway:right:width")] %>% as.numeric()
      bikelaneWidthRvsLeft <- c(bikelaneWidthRvsLeft, tag)
    }
    if (any(keys == "cycleway:both:width")) {
      tag <- values[which(keys == "cycleway:both:width")] %>% as.numeric()
      bikelaneWidthFwdLeft <- c(bikelaneWidthFwdLeft, tag)
      bikelaneWidthRvsLeft <- c(bikelaneWidthRvsLeft, tag)
    }
  }
  ## add the width tags to the df
  df$bikelaneWidthFwdLeft[1] <- 
    ifelse(is.null(bikelaneWidthFwdLeft), NA, 
          max(bikelaneWidthFwdLeft))
  df$bikelaneWidthFwdRight[1] <- 
    ifelse(is.null(bikelaneWidthFwdRight), NA, 
           max(bikelaneWidthFwdRight))
  df$bikelaneWidthRvsLeft[1] <- 
    ifelse(is.null(bikelaneWidthRvsLeft), NA, 
           max(bikelaneWidthRvsLeft))
  
  # cycleway:[side]:traffic_mode tags - traffic conditions to left or right of bike lane
  ## empty vectors for the tags
  bikelaneTrafFwdLeft <- c()
  bikelaneTrafFwdRight <- c()
  bikelaneTrafRvsLeft <- c()
  if (df$is_oneway[1] == 1) {
    # for one way, left and right values are forward left and right
    # eg: 'cycleway:left:traffic_mode:both' describes the traffic mode on both sides
    # of the cycleway that is to the left of the road; in extracting tags, take 
    # the final left/right/both (after 'traffic_mode) and add it at the start of
    # the tag, eg 'cycleway:left:traffic_mode:both=parking' becomes 'both:parking'
    if (any(keys == "cycleway:left:traffic_mode:left")) {
      tag <- paste0("left=", values[which(keys == "cycleway:left:traffic_mode:left")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
    }
    if (any(keys == "cycleway:left:traffic_mode:right")) {
      tag <- paste0("right=", values[which(keys == "cycleway:left:traffic_mode:right")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
    }
    if (any(keys == "cycleway:left:traffic_mode:both")) {
      tag <- paste0("both=", values[which(keys == "cycleway:left:traffic_mode:both")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
    }
    if (any(keys == "cycleway:right:traffic_mode:left")) {
      tag <- paste0("left=", values[which(keys == "cycleway:right:traffic_mode:left")])
      bikelaneTrafFwdRight <- c(bikelaneTrafFwdRight, tag)
    }
    if (any(keys == "cycleway:right:traffic_mode:right")) {
      tag <- paste0("right=", values[which(keys == "cycleway:right:traffic_mode:right")])
      bikelaneTrafFwdRight <- c(bikelaneTrafFwdRight, tag)
    }
    if (any(keys == "cycleway:right:traffic_mode:both")) {
      tag <- paste0("both=", values[which(keys == "cycleway:right:traffic_mode:both")])
      bikelaneTrafFwdRight <- c(bikelaneTrafFwdRight, tag)
    }
    if (any(keys == "cycleway:both:traffic_mode:left")) {
      tag <- paste0("left=", values[which(keys == "cycleway:both:traffic_mode:left")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
      bikelaneTrafFwdRight <- c(bikelaneTrafFwdRight, tag)
    }
    if (any(keys == "cycleway:both:traffic_mode:right")) {
      tag <- paste0("right=", values[which(keys == "cycleway:both:traffic_mode:right")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
      bikelaneTrafFwdRight <- c(bikelaneTrafFwdRight, tag)
    }
    if (any(keys == "cycleway:both:traffic_mode:both")) {
      tag <- paste0("both=", values[which(keys == "cycleway:both:traffic_mode:both")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
      bikelaneTrafFwdRight <- c(bikelaneTrafFwdRight, tag)
    }
  } else {
    # for two way, left values are forward left and right values are reverse left
    if (any(keys == "cycleway:left:traffic_mode:left")) {
      tag <- paste0("left=", values[which(keys == "cycleway:left:traffic_mode:left")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
    }
    if (any(keys == "cycleway:left:traffic_mode:right")) {
      tag <- paste0("right=", values[which(keys == "cycleway:left:traffic_mode:right")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
    }
    if (any(keys == "cycleway:left:traffic_mode:both")) {
      tag <- paste0("both=", values[which(keys == "cycleway:left:traffic_mode:both")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
    }
    if (any(keys == "cycleway:right:traffic_mode:left")) {
      tag <- paste0("left=", values[which(keys == "cycleway:right:traffic_mode:left")])
      bikelaneTrafRvsLeft <- c(bikelaneTrafRvsLeft, tag)
    }
    if (any(keys == "cycleway:right:traffic_mode:right")) {
      tag <- paste0("right=", values[which(keys == "cycleway:right:traffic_mode:right")])
      bikelaneTrafRvsLeft <- c(bikelaneTrafRvsLeft, tag)
    }
    if (any(keys == "cycleway:right:traffic_mode:both")) {
      tag <- paste0("both=", values[which(keys == "cycleway:right:traffic_mode:both")])
      bikelaneTrafRvsLeft <- c(bikelaneTrafRvsLeft, tag)
    }
    if (any(keys == "cycleway:both:traffic_mode:left")) {
      tag <- paste0("left=", values[which(keys == "cycleway:both:traffic_mode:left")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
      bikelaneTrafRvsLeft <- c(bikelaneTrafRvsLeft, tag)
    }
    if (any(keys == "cycleway:both:traffic_mode:right")) {
      tag <- paste0("right=", values[which(keys == "cycleway:both:traffic_mode:right")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
      bikelaneTrafRvsLeft <- c(bikelaneTrafRvsLeft, tag)
    }
    if (any(keys == "cycleway:both:traffic_mode:both")) {
      tag <- paste0("both=", values[which(keys == "cycleway:both:traffic_mode:both")])
      bikelaneTrafFwdLeft <- c(bikelaneTrafFwdLeft, tag)
      bikelaneTrafRvsLeft <- c(bikelaneTrafRvsLeft, tag)
    }
  }
  ## add the traffic mode tags to the df
  df$bikelaneTrafFwdLeft[1] <- 
    ifelse(is.null(bikelaneTrafFwdLeft), NA, 
           stringr::str_flatten(unique(bikelaneTrafFwdLeft), collapse = ","))
  df$bikelaneTrafFwdRight[1] <- 
    ifelse(is.null(bikelaneTrafFwdRight), NA, 
           stringr::str_flatten(unique(bikelaneTrafFwdRight), collapse = ","))
  df$bikelaneTrafRvsLeft[1] <- 
    ifelse(is.null(bikelaneTrafRvsLeft), NA, 
           stringr::str_flatten(unique(bikelaneTrafRvsLeft), collapse = ","))

  return(df)
}

