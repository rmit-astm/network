# function to simplify the appearance of links to avoid short segments (using
# douglas-peucker algorithm) but ensure that endpoints remain unchanged
simplifyLinkAppearance <- function(edges_current, dTolerance = 20) {
  
  # reporting
  echo("Simplifying link appearance while preserving endpoints\n")
  
  # simplify the geometries
  simplified_edges <- st_simplify(edges_current, dTolerance = dTolerance)
  
  # start and end points may have been changed by simplification, so
  # restore the original start and end points using fromx, fromy, tox, toy
  
  # extract coordinates for all simplified geometries
  coords <- st_coordinates(simplified_edges)
  
  # find the indices for the coordinates that are start and end points
  line_ids <- coords[, "L1"]
  first_indices <- !duplicated(line_ids)
  last_indices <- !duplicated(line_ids, fromLast = TRUE)
  
  # extract just the coordinates for the first and last points
  first_coords <- coords[first_indices, ]
  last_coords <- coords[last_indices, ]
  
  # find the original start/endpoint geometry, based on from/to x/y, depending
  # on direction of digitisation
  edges_start_end <- edges_current %>%
    # check whether startpoint of geometry matches from_id ("forward" if yes, "reverse" if no)
    mutate(startpoint = st_coordinates(st_startpoint(geom))) %>%
    rowwise() %>%
    mutate(direction = 
             ifelse(startpoint[[1]] == fromx & startpoint[[2]] == fromy, 
                    "forward", 
                    "reverse")) %>%
    ungroup() %>%
    # set start/end x/y based on digitisation direction
    mutate(startx = ifelse(direction == "forward", fromx, tox),
           starty = ifelse(direction == "forward", fromy, toy),
           endx = ifelse(direction == "forward", tox, fromx),
           endy = ifelse(direction == "forward", toy, fromy))
 
  # replace first and last coordinates with original start/endpoint geometry
  first_coords[, c("X", "Y")] <- cbind(edges_start_end$startx, edges_start_end$starty)
  last_coords[, c("X", "Y")] <- cbind(edges_start_end$endx, edges_start_end$endy)
  
  # combine modified first and last coordinates with the rest of the coordinates
  modified_coords <- coords
  modified_coords[first_indices, c("X", "Y")] <- first_coords[, c("X", "Y")]
  modified_coords[last_indices, c("X", "Y")] <- last_coords[, c("X", "Y")]
  
  # create new geometries with the modified coordinates using split
  split_coords <- split(modified_coords[, c("X", "Y")], line_ids)
  new_geometries <- lapply(split_coords, function(x) {
    # convert coordinates to matrix
    mat <- matrix(x, ncol = 2)
    # create linestring from matrix
    st_linestring(mat)
  })
  
  # update the geometries in the simplified_edges object
  st_geometry(simplified_edges) <- st_sfc(new_geometries, crs = st_crs(edges_current))
  
  return(simplified_edges)
}


