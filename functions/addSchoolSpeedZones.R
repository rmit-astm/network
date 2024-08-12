# function to add school time speed zones to links

addSchoolSpeedZones <- function(input.links,
                                schoolZoneFile,
                                outputCrs) {
  
  # input.links <- networkOneway[[2]]
  
  # read in school zone file
  school.zones <- st_read(schoolZoneFile)
  if (st_crs(school.zones)$epsg != outputCrs) {
    school.zones <- st_transform(school.zones, outputCrs)
  }
  
  # calculate school zone azimuth 
  school.zones.az <- school.zones %>%
    mutate(zone_az = st_azimuth(st_startpoint(.), st_endpoint(.))) %>%
    # correct azimuth where in wrong direction
    mutate(correct_az = case_when(
      direction == "North" & zone_az > 90 & zone_az < 270         ~ 0,
      direction == "North East" & zone_az > 135 & zone_az < 315   ~ 0,
      direction == "East" & zone_az > 180                         ~ 0,
      direction == "South East" & (zone_az > 225 | zone_az < 45)  ~ 0,
      direction == "South" & (zone_az > 270 | zone_az < 90)       ~ 0,
      direction == "South West" & (zone_az > 315 | zone_az < 135) ~ 0,
      direction == "West" & zone_az < 180                         ~ 0,
      direction == "North West" & zone_az > 45 & zone_az < 225    ~ 0,
      .default = 1))

  # for NAs, create a second link in opposite direction
  school.zones.az.reversedNA <- school.zones.az %>%
    filter(is.na(direction)) %>% 
    mutate(correct_az = 0)
  
  # combine, reverse azimuth where necessary, and buffer
  school.zones.buffered <- school.zones.az %>%
    bind_rows(., school.zones.az.reversedNA) %>%
    mutate(zone_az = ifelse(correct_az == 0,
                            (zone_az + 180) %% 360,
                            zone_az)) %>%
    st_buffer(30)
  
  # filter links to those that are driveable and with speed > 40,
  links.filtered <- input.links %>%
    mutate(speed = round(freespeed * 3.6)) %>%
    filter(is_car == 1 & speed > 40) %>%
    mutate(link_length = length)
  
  # and calculate azimuth for links: create point geometries for start and end points,
  # find azimuth
  from_points <- st_as_sf(links.filtered %>% st_drop_geometry, 
                          coords = c("fromx", "fromy"), crs = outputCrs)
  to_points <- st_as_sf(links.filtered %>% st_drop_geometry, 
                        coords = c("tox", "toy"), crs = outputCrs)
  links.filtered$link_az <- st_azimuth(from_points, to_points)
  
 
  # intersect links with school zones and calculate length
  links.intersected <- st_intersection(links.filtered, school.zones.buffered) %>%
    mutate(isec_length = as.numeric(st_length(.)))
  
  # function to find absolute difference between 2 azimuths (note: adding then
  # subtracting 180 constrains the result to the abs of a range of -180 to 180)
  azDiff <- function(az1, az2) {
    return(abs((az1 - az2 + 180) %% 360 - 180))
  }
  
  # filter out based on azimuth, length and speed criteria: 
  # (1) azimuth of link must be within 60 degrees of azimuth of the school zone road; a
  # (2) length of intersecting section must be at least 25% of length of the link;
  # (3) the school speed must be lower than the original speed
  selected.links <- links.intersected %>%
    st_drop_geometry() %>%
    # azimuth filter
    mutate(az_diff = azDiff(zone_az, link_az)) %>%
    filter(az_diff <= 60) %>%  # 8666 links (unsimplified)
    # length filter
    filter(isec_length > link_length / 4) %>%  # 6500 links (unsimplified)
    # speed filter
    filter(speed_limit < round(freespeed * 3.6)) 
  
  # some links may fall into two or more school speed zones; if so, then select
  # the speed that applies for the longest part of the link:
  # find the duplicates
  duplicate_ids <- selected.links %>%
    dplyr::select(link_id, speed_limit) %>%
    distinct()  # 4617
  duplicates <- duplicate_ids %>%
    group_by(link_id) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    filter(n > 1)  # 5
  # select the links, and choose the one with the longest length
  duplicate.links <- selected.links %>%
    filter(link_id %in% duplicates$link_id) %>%
    group_by(link_id) %>%
    filter(isec_length == max(isec_length)) %>%
    ungroup()
  # take original selection, but for duplicates only the longest
  selected.links.final <- selected.links %>%
    filter(!link_id %in% duplicate.links$link_id) %>%
    bind_rows(duplicate.links) %>%
    # keep only required fields
    dplyr::select(link_id, speed_limit)
  
  # join the school speeds to the input links
  output.links <- input.links %>%
    # join speeds
    left_join(selected.links.final, by = "link_id") %>%
    # calculate school speed
    mutate(speed_limit = as.numeric(speed_limit),
           freespeed_school = ifelse(!is.na(speed_limit),
                                     speed_limit / 3.6,
                                     freespeed)) %>%
    dplyr::select(-speed_limit)
  
  return(output.links)
}
