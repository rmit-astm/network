# function to expand the destination layer by adding specific Bendigo datasets

getDestinationsBendigo <- function(bendigoParking,
                                   bendigoParkingPoly,
                                   bendigoParkingLine,
                                   bendigoBikeRacks,
                                   outputCrs) {
  
  # parking
  parkingPoly <- read_zipped_GIS(zipfile = bendigoParking, file = bendigoParkingPoly) %>%
    st_transform(outputCrs) %>%
    mutate(dest_type = "bendigo_parking") %>%
    st_make_valid(.)
  
  parkingLine <- read_zipped_GIS(zipfile = bendigoParking, file = bendigoParkingLine) %>%
    st_transform(outputCrs) %>%
    mutate(dest_type = "bendigo_parking") %>%
    st_make_valid(.)
  
  # bike racks
  bikeRacks <- read.csv(bendigoBikeRacks) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(outputCrs) %>%
    mutate(dest_type = "bendigo_bike_racks") %>%
    st_make_valid(.)
  
  # convert names to lower case (matching the osm destinations)
  names(parkingPoly) <- tolower(names(parkingPoly))
  names(parkingLine) <- tolower(names(parkingLine))
  names(bikeRacks) <- tolower(names(bikeRacks))
  
  return(bind_rows(parkingPoly, parkingLine, bikeRacks))
  
}

