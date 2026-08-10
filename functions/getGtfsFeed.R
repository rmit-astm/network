# function to download a GTFS feed and, if required, convert it to a flat feed
#
# Some agencies (eg PTV in Victoria) distribute a nested bundle holding a separate
# GTFS feed for each transport mode, rather than the single flat feed that the
# pipeline reads. Where 'gtfsNested' is true, the downloaded bundle is flattened
# with prepareGtfs(); otherwise the downloaded file is used as-is.
#
# The download location is set by 'gtfsUrl' so that feeds for other cities can be
# used without editing this function.

getGtfsFeed <- function(gtfsUrl,
                        gtfsBundle,
                        gtfs_feed,
                        gtfsNested = T,
                        gtfsFolders = 1:6,
                        retainDownload = T) {

  # gtfsUrl = "https://data.ptv.vic.gov.au/downloads/gtfs.zip"
  # gtfsBundle = "./data/gtfs_ptv_bundle.zip"
  # gtfs_feed = "./data/gtfs.zip"

  # where the feed is not nested, it is downloaded straight to its final location
  downloadLocation <- ifelse(gtfsNested, gtfsBundle, gtfs_feed)

  # increase timeout to allow time for large GTFS feed to download
  default.timeout <- getOption("timeout")
  options(timeout = 7200)

  echo(paste("Downloading GTFS feed from", gtfsUrl, "\n"))
  download.file(gtfsUrl, downloadLocation, mode = "wb", quiet = TRUE)

  options(timeout = default.timeout)

  if (!file.exists(downloadLocation)) {
    stop(paste("GTFS feed could not be downloaded from", gtfsUrl))
  }
  echo(paste0("Downloaded GTFS feed (",
              round(file.size(downloadLocation) / 1024 ^ 2), " MB)\n"))

  # flatten the bundle if required, or else the download is already the feed
  if (gtfsNested) {
    echo("Converting nested GTFS bundle to a single flat feed\n")
    prepareGtfs(downloadLocation, gtfs_feed, folders = gtfsFolders)
    if (!retainDownload) unlink(downloadLocation)
  }

  return(invisible(gtfs_feed))
}
