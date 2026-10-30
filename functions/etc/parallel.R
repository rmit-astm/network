# start a cluster for parallel processing with foreach (using all available
# cores, or 'maxcores' if fewer), and register it; the caller stops it with
# stopCluster, eg on.exit(try(stopCluster(cluster), silent = TRUE), add = TRUE)
startCluster <- function(maxcores = NA) {
  cores <- detectCores()
  if (!is.na(maxcores)) cores <- min(cores, maxcores)
  cluster <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cluster)
  return(cluster)
}
