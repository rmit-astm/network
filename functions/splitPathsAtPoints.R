# function to split paths at points within a given distance of the path, 
# using matching field (such as osm_id) in both paths and points

# returns the split paths as a single dataframe, with a row for each part, in
# the order of 'paths'

splitPathsAtPoints <- function(paths, points, buff.dist, field, maxcores) {
  
  # paths = paths
  # points = intersections
  # buff.dist = 0.001  # distance to which points are buffered to split
  # field = "osm_id"
  
  # paths = paths.to.resplit
  # points = endpoints.with.path.id
  # buff.dist = 0.1
  # field = "path_id"
  
  # coordinates of the points for each path (NULL if none), in the order of
  # 'paths', so each path only needs its own points
  coords <- st_coordinates(points)[, c("X", "Y"), drop = FALSE]
  path.points <- split.data.frame(coords, as.character(points[[field]]))
  path.points <- unname(path.points[as.character(paths[[field]])])
  
  # path geometries without their crs: the splitting is planar either way, but
  # with a crs, sf looks it up on every call, which takes most of the time
  path.geoms <- st_set_crs(st_geometry(paths), NA)
  
  # setup for parallel processing - detect no of available cores and create cluster
  cluster <- startCluster(maxcores)
  cores <- length(cluster)
  # stop the cluster if this function exits early with an error
  on.exit(try(stopCluster(cluster), silent = TRUE), add = TRUE)
  
  # split the paths in chunks of paths, rather than one task per path (each
  # task with only its own paths and points)
  chunks <- split(seq_along(path.geoms),
                  cut(seq_along(path.geoms), min(cores * 10, length(path.geoms)),
                      labels = FALSE))
  
  # set up progress reporting 
  pb <- txtProgressBar(max = max(length(chunks), 2), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # report progress
  echo(paste("Splitting", nrow(paths), "paths; parallel processing with",
             cores, "cores\n"))
  
  # loop to split paths: the difference between each path and its buffered
  # points, as a list of the linestring parts of each path
  split.geoms <-
    foreach(geoms = lapply(chunks, function(rows) path.geoms[rows]),
            pts = lapply(chunks, function(rows) path.points[rows]),
            .combine = c,
            .packages = c("sf"), 
            .options.snow = opts) %dopar% {
              lapply(seq_along(geoms), function(i) {
                
                # paths with no points are not split
                if (is.null(pts[[i]])) return(list(geoms[[i]]))
                
                # split path at its points
                buffered.points <- st_buffer(st_sfc(st_multipoint(pts[[i]])), buff.dist)
                split.path <- st_difference(geoms[i], buffered.points)
                
                # parts of the split path as linestrings
                split.geom <- split.path[[1]]
                if (inherits(split.geom, "LINESTRING") && !st_is_empty(split.geom)) {
                  return(list(split.geom))
                } else if (inherits(split.geom, "MULTILINESTRING") && !st_is_empty(split.geom)) {
                  return(lapply(split.geom, st_linestring))
                } else {
                  return(as.list(st_geometry(st_cast(st_sf(geom = split.path), to = "LINESTRING"))))
                }
              })
            }
  
  # close the progress bar and cluster
  close(pb)
  stopCluster(cluster)
  
  # a row for each part of each path, with the part's geometry
  split.paths <- paths[rep(seq_len(nrow(paths)), lengths(split.geoms)), ]
  st_geometry(split.paths) <- st_sfc(unlist(split.geoms, recursive = FALSE),
                                     crs = st_crs(paths))
  rownames(split.paths) <- NULL
  
  return(split.paths)
  
}
