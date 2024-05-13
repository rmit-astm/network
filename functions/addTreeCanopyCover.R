# function to add tree canopy coverage (TCC) percent to links, where each link is buffered
# a certain distance based on the 'highway_order' attribute. This buffer distance is stored
# in the 'tcc_buffer' attribute. Then, 'tcc_percent' is calculated as the ratio of tree canopy
# coverage and total buffer area. In this way, links in the network can be ranked based
# on their greenery presence.

addTreeCanopyCover2Links <- function(links, treeCanopyCoverFile, outputCrs) {
  
  echo("Calculating tree canopy cover buffer distance based on highway type\n")
  
  # Extract the 'highway_order' attribute from links input
  highway_order <- links$highway_order
  
  # Apply buffer distances based on 'highway' type
  tcc_buffer <- case_when(
    highway_order == 21 ~ 2,
    highway_order %in% c(16, 18, 19, 20, 22) ~ 6,
    highway_order %in% c(13, 15, 17) ~ 8,
    highway_order %in% c(5, 6, 7, 12, 14) ~ 10,
    highway_order %in% c(4, 11) ~ 12.5,
    highway_order %in% c(3, 10) ~ 15,
    highway_order %in% c(2, 9) ~ 18,
    highway_order %in% c(1, 8) ~ 20,
    TRUE ~ 10
  )
  
  # Add the 'tcc_buffer' attribute to links
  links$tcc_buffer <- tcc_buffer
  
  echo("Tree Canopy Coverage buffer distance 'tcc_buffer' added to links\n")
  
  echo("Reading in the tree canopy cover file and reprojecting if necessary\n")
  
  # Read in tree canopy cover file
  treeCanopyCover <- rast(treeCanopyCoverFile)
  outputCrsEPSG <- paste0("EPSG:", outputCrs)
  if (!same.crs(treeCanopyCover, outputCrsEPSG)) treeCanopyCover <- project(treeCanopyCover, outputCrsEPSG)
  
  echo("Buffering each link\n")
  
  # Buffer each link
  links.buffered <- st_buffer(links, links$tcc_buffer)
  
  echo("Extracting pixel counts of each link's buffer\n")
  
  # Extract the pixel count of each link's buffer
  pixels_count <- terra::extract(treeCanopyCover, links.buffered, touches=TRUE)
  
  echo("Counting pixels of each link's buffer\n")
  
  # Count the number of pixels that represent trees
  tree_pixels <- pixels_count %>%
    group_by(ID) %>%
    summarise(tree_pixels = sum(TCC == 1, na.rm = TRUE))
  
  # Count the total number of pixels in the buffer
  total_pixels <- pixels_count %>%
    group_by(ID) %>%
    summarise(total_pixels = sum(is.na(TCC) | TCC == 1))
  
  echo("Joining pixel counts to links\n")
  
  # Join 'tree_pixels' to links, using the row number and ID
  links <- links %>%
    mutate(row_no = row_number()) %>%
    left_join(., tree_pixels, by = c("row_no" = "ID")) %>%
    dplyr::select(-row_no)
  
  # Join 'total_pixels' to links, using the row number and ID
  links <- links %>%
    mutate(row_no = row_number()) %>%
    left_join(., total_pixels, by = c("row_no" = "ID")) %>%
    dplyr::select(-row_no)
  
  # Calculate tree canopy coverage percent and round to 2 decimal places
  links.with.tcc <- links %>%
    mutate(tcc_percent = round((tree_pixels / total_pixels) * 100, 2))
  
  # Un-comment the line below to export an intermediate output for testing
  #st_write(links.with.tcc, "./output/links_with_tcc.sqlite", delete_layer = TRUE)
  
  echo("Tree Canopy Coverage Percent 'tcc_percent' added to links\n")
  
  return(links.with.tcc)
}