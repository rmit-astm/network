# function to expand the destination layer by adding specific Bendigo datasets

getDestinationsBendigo <- function(bendigoParking,
                                   bendigoParkingPoly,
                                   bendigoParkingLine,
                                   bendigoBikeRacks,
                                   osmGpkg,
                                   outputCrs) {
  
  # parking
  parkingPoly <- read_zipped_GIS(zipfile = bendigoParking, file = bendigoParkingPoly) %>%
    st_transform(outputCrs) %>%
    mutate(dest_type = "bendigo_parking") %>%
    st_make_valid(.) %>%
    st_set_geometry("geom")
  
  parkingLine <- read_zipped_GIS(zipfile = bendigoParking, file = bendigoParkingLine) %>%
    st_transform(outputCrs) %>%
    mutate(dest_type = "bendigo_parking") %>%
    st_make_valid(.) %>%
    st_set_geometry("geom")
  
  # bike racks
  bikeRacks <- read.csv(bendigoBikeRacks) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(outputCrs) %>%
    mutate(dest_type = "bendigo_bike_racks") %>%
    st_make_valid(.) %>%
    st_set_geometry("geom")
  
  # convert names to lower case (matching the osm destinations)
  names(parkingPoly) <- tolower(names(parkingPoly))
  names(parkingLine) <- tolower(names(parkingLine))
  names(bikeRacks) <- tolower(names(bikeRacks))
  
  # specific Bendigo parks from OSM that don't meet general destination tag criteria
  # polygons <- oe_read(osmGpkg, query = paste(extra.tag.string, "FROM multipolygons"), quiet = TRUE)
  polygons <- oe_read(osmGpkg, query = "SELECT * FROM multipolygons", quiet = TRUE)
  extraParks <- polygons %>%
    filter(name == "Spring Gully Bushland Reserve") %>%
    mutate(dest_type = "park") %>%
    st_set_geometry("geom")
  
  return(bind_rows(parkingPoly, parkingLine, bikeRacks, extraParks))
  
}

# function to add a 'premium_park' field to certain higher-value parks
addPremiumParkTagBendigo <- function(destinations) {
  
  # parks that contain playground, sport or community centre
  park.features <- destinations %>%
    filter(dest_type %in% c("playground", "sport", "community_centre")) %>%
    st_geometry(.)
  premium.parks.1 <- destinations %>%
    filter(dest_type == "park") %>%
    st_intersection(., park.features)
  
  # specific named parks
  premium.parks.2 <- destinations %>%
    filter(dest_type == "park") %>%
    filter(name == "Rosalind Park" | name == "Bendigo Botanic Gardens" |
             osm_way_id == "201716419")  # unnamed park near Canterbury Park containing Tom Thumb Lake
  
  premium.parks <- bind_rows(premium.parks.1, premium.parks.2)
  
  # add premium park tag
  destinations <- destinations %>%
    mutate(is_premium_park = ifelse(dest_id %in% premium.parks$dest_id, 1, 0))
  
  return(destinations)
}
