# function to simplify the appearance of links to avoid short segments (using
# douglas-peucker algorithm) but ensure that endpoints remain unchanged
simplifyLinkAppearance <- function(edges_current, dTolerance = 20) {
  
  # reporting
  echo("Simplifying link appearance while preserving endpoints\n")
  
  # simplify the geometries
  simplified_edges <- st_simplify(edges_current, dTolerance = dTolerance)
  
  # start and end points may have been changed by simplification, so
  # restore the original start and end points using fromx, fromy, tox, toy
  
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
  
  # add the start/end coordinates to simplified_edges
  simplified_edges <- simplified_edges %>%
    left_join(edges_start_end %>%
                st_drop_geometry() %>%
                dplyr::select(link_id, startx, starty, endx, endy),
              by = "link_id")
    
  # correct coordinates at start and end of links to exactly match node coordinates
  simplified_edges <- correct_coords(simplified_edges) %>%
    dplyr::select(-startx, -starty, -endx, -endy)

  return(simplified_edges)
}


