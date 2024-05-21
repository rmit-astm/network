# processing network files to create lightweight versions for online display map

makeDisplayLayers <- function() {
  library(sf)
  library(tidyverse)
  
  # load network (working directory assumed to be 'network-astm')
  links <- st_read("./output/bendigo_network/network.sqlite", layer = "links")
  nodes <- st_read("./output/bendigo_network/network.sqlite", layer = "nodes")
  destinations <- st_read("./output/bendigo_network/network.sqlite", layer = "destinations")
  
  # simple links layers
  highway_type <- links %>%
    filter(!(highway %in% c("bus", "train"))) %>%
    mutate(highway_type = case_when(
      highway %in% c("motorway", "motorway_link")   ~ "motorway",
      highway %in% c("trunk", "trunk_link", 
                     "primary", "primary_link", 
                     "secondary", "secondary_link") ~ "arterial",
      highway %in% c("tertiary", "tertiary_link")   ~ "collector",
      highway %in% c("residential", "road", "unclassified",
                     "living_street", "service")    ~ "local",
      TRUE                                          ~ "offroad"
    )) %>%
    dplyr::select(highway_type)
  
  cycling_offroad_path <- links %>%
    filter(cycleway %in% c("bikepath", "shared_path")) %>%
    dplyr::select(cycleway)
  
  cycling_separated_lane <- links %>%
    filter(cycleway == "separated_lane") %>%
    dplyr::select(cycleway)
  
  cycling_onroad_lane <- links %>%
    filter(cycleway == "simple_lane") %>%
    dplyr::select(cycleway)
  
  cycling_mixed_traffic <- links %>%
    filter(!(highway %in% c("bus", "train"))) %>%
    filter(!cycleway %in% c("bikepath", "shared_path", "separated_lane", "simple_lane")) %>%
    filter(is_cycle == 1) %>%
    dplyr::select(cycleway)
  
  speed_limit <- links %>%
    filter(!(highway %in% c("bus", "train"))) %>%
    mutate(speed = round(freespeed * 3.6),
           speed_limit = case_when(
             speed <= 30 ~ "Up to 30 km/h",
             speed <= 40 ~ "> 30 km/h up to 40 km/h",
             speed <= 50 ~ "> 40 km/h up to 50 km/h",
             speed <= 60 ~ "> 50 km/h up to 60 km/h",
             speed <= 80 ~ "> 60 km/h hp to 80 km/h",
             speed > 80  ~ "> 80 km/h"
           )) %>%
    dplyr::select(speed_limit)
  
  lane_numbers <- links %>%
    filter(!(highway %in% c("bus", "train"))) %>%
    mutate(lanes = case_when(
      permlanes == 1         ~ "1",
      permlanes == 2         ~ "2",
      permlanes %in% c(3, 4) ~ "3 to 4",
      permlanes >= 5         ~ "5 or more"
    )) %>%
    dplyr::select(lanes)
  
  slope <- links %>%
    filter(!(highway %in% c("bus", "train"))) %>%
    mutate(slope = case_when(
      abs(slope_pct) <= 1  ~ "Up to 1%",
      abs(slope_pct) <= 2.5 ~ "> 1% up to 2.5%",
      abs(slope_pct) <= 5   ~ "> 2.5% up to 5%",
      abs(slope_pct) <= 10  ~ "> 5% up to 10%",
      abs(slope_pct) > 10   ~ "> 10%"
    )) %>%
    filter(!is.na(slope)) %>%
    dplyr::select(slope)
  
  canopy_cover <- links %>%
    filter(!(highway %in% c("bus", "train"))) %>%
    mutate(canopy_cover = case_when(
      tcc_percent <= 10 ~ "Up to 10%",
      tcc_percent <= 25 ~ "> 10% up to 25%",
      tcc_percent <= 50 ~ "> 25% up to 50%",
      tcc_percent > 50  ~ "> 75%"
    )) %>%
    dplyr::select(canopy_cover)

  level_of_traffic_stress <- links %>%
    filter(!(highway %in% c("bus", "train"))) %>%
    mutate(lvl_traf_stress = as.character(lvl_traf_stress)) %>%
    dplyr::select(lvl_traf_stress)
  
  public_transport <- links %>%
    filter(highway != "bus") %>%
    mutate(public_transport = case_when(
      modes == "train"         ~ "train",
      str_detect(modes, "bus") ~ "bus",
      TRUE                     ~ "none"
    )) %>%
    dplyr::select(public_transport)
  
  # simple nodes layer
  intersection_type <- nodes %>%
    dplyr::select(type)
  
  # simple destination layer
  destination_layer <- destinations %>%
    dplyr::select(dest_type, pt_stop_type, stop_name, number_bay) %>%
    arrange(dest_type)
  
  
  # write outputs
  output.file <- "./output/bendigo display.sqlite"
  st_write(highway_type, output.file, layer = "highway_type", delete_layer = T)
  st_write(cycling_offroad_path, output.file, layer = "cycling_offroad_path", delete_layer = T)
  st_write(cycling_separated_lane, output.file, layer = "cycling_separated_lane", delete_layer = T)
  st_write(cycling_onroad_lane, output.file, layer = "cycling_onroad_lane", delete_layer = T)
  st_write(cycling_mixed_traffic, output.file, layer = "cycling_mixed_traffic", delete_layer = T)
  st_write(speed_limit, output.file, layer = "speed_limit", delete_layer = T)
  st_write(lane_numbers, output.file, layer = "lane_numbers", delete_layer = T)
  st_write(slope, output.file, layer = "slope", delete_layer = T)
  st_write(canopy_cover, output.file, layer = "canopy_cover", delete_layer = T)
  st_write(level_of_traffic_stress, output.file, layer = "level_of_traffic_stress", delete_layer = T)
  st_write(public_transport, output.file, layer = "public_transport", delete_layer = T)
  st_write(intersection_type, output.file, layer = "intersection_type", delete_layer = T)
  st_write(destination_layer, output.file, layer = "destinations", delete_layer = T)
  
}

makeDisplayLayers()
            