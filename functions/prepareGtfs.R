# function to convert a raw PTV GTFS bundle into a single flat GTFS feed
#
# PTV distributes its GTFS as a nested bundle: the outer zip contains numbered
# folders (1..N), each holding a 'google_transit.zip' for one transport mode:
#   1 = Regional Train, 2 = Metropolitan Train, 3 = Metropolitan Tram,
#   4 = Metropolitan Bus, 5 = Regional Coach, 6 = Regional Bus (others skipped)
#
# The downstream pipeline (gtfs2PtNetwork.R) reads a single flat feed and
# classifies modes by 'agency_id' using exactly those folder numbers
# (agency_id 1,2 = train; 3 = tram; 4,5,6 = bus). Current PTV feeds set every
# sub-feed's agency_id to "1", so this function overwrites agency_id with the
# folder number and namespaces the ids of each sub-feed (to avoid collisions)
# before merging them into one feed. Without this, trams and buses would be
# silently dropped by the mode classification.

# map GTFS 'extended route types' (eg PTV's 400 urban rail, 204 regional coach,
# 701 bus) back to the basic types the downstream classification expects
# (0 = tram/light rail, 1 = metro/urban rail, 2 = rail, 3 = bus/coach)
normalizeRouteType <- function(rt) {
  rt <- suppressWarnings(as.integer(rt))
  dplyr::case_when(
    rt == 0 | (rt >= 900 & rt <= 906)                          ~ 0L,  # tram / light rail
    rt == 1 | (rt >= 400 & rt <= 405)                          ~ 1L,  # metro / urban rail
    rt == 2 | (rt >= 100 & rt <= 117)                          ~ 2L,  # rail
    rt == 3 | (rt >= 200 & rt <= 209) |
      (rt >= 700 & rt <= 716) | rt == 800                      ~ 3L,  # bus / coach
    TRUE ~ rt
  )
}

prepareGtfs <- function(inputBundle,
                        outputZip,
                        folders = 1:6) {

  # inputBundle = "./data/gtfs_ptv_bundle.zip"
  # outputZip = "./data/gtfs.zip"
  # folders = 1:6

  # tables kept and, for each, the id columns to namespace with the folder prefix
  idCols <- list(
    agency         = character(0),               # agency_id set to folder number
    stops          = c("stop_id", "parent_station"),
    routes         = c("route_id"),              # agency_id set to folder number
    trips          = c("route_id", "service_id", "trip_id", "shape_id", "block_id"),
    stop_times     = c("trip_id", "stop_id"),
    calendar       = c("service_id"),
    calendar_dates = c("service_id"),
    shapes         = c("shape_id")
  )
  keepTables <- names(idCols)

  message("Unzipping PTV bundle: ", inputBundle)
  tmp <- file.path(tempdir(), paste0("ptv_", as.integer(Sys.time())))
  dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(inputBundle, exdir = tmp)

  merged <- list()

  for (n in folders) {
    inner <- file.path(tmp, n, "google_transit.zip")
    if (!file.exists(inner)) {
      message("  folder ", n, ": no google_transit.zip, skipping")
      next
    }

    # read all tables (gtfsio keeps raw types; no date/time coercion)
    g <- gtfsio::import_gtfs(inner)
    g <- g[intersect(names(g), keepTables)]

    prefix <- paste0(n, "_")

    # force agency_id to the folder number (single agency row)
    if (!is.null(g$agency)) {
      g$agency <- g$agency[1, , drop = FALSE]
      g$agency$agency_id <- as.character(n)
    }
    if (!is.null(g$routes)) {
      g$routes$agency_id <- as.character(n)
      # normalize any extended route types to the basic types the pipeline expects
      g$routes$route_type <- normalizeRouteType(g$routes$route_type)
    }

    # namespace id columns so ids don't collide across the merged sub-feeds
    for (tbl in names(idCols)) {
      if (is.null(g[[tbl]])) next
      for (col in idCols[[tbl]]) {
        if (col %in% names(g[[tbl]])) {
          v <- as.character(g[[tbl]][[col]])
          g[[tbl]][[col]] <- ifelse(is.na(v) | v == "", v, paste0(prefix, v))
        }
      }
    }

    # accumulate
    nRoutes <- if (is.null(g$routes)) 0 else nrow(g$routes)
    message("  folder ", n, ": merged (", nRoutes, " routes)")
    for (tbl in names(g)) {
      merged[[tbl]] <- if (is.null(merged[[tbl]])) g[[tbl]] else
        dplyr::bind_rows(merged[[tbl]], g[[tbl]])
    }
  }

  if (length(merged) == 0) stop("No sub-feeds were merged; check inputBundle and folders")

  # one agency row per agency_id
  if (!is.null(merged$agency)) {
    merged$agency <- dplyr::distinct(merged$agency, agency_id, .keep_all = TRUE)
  }

  # drop duplicate ids that would make the feed invalid (some source feeds
  # contain a small number of duplicate trips or stop_times)
  if (!is.null(merged$trips)) {
    merged$trips <- dplyr::distinct(merged$trips, trip_id, .keep_all = TRUE)
  }
  if (!is.null(merged$stop_times)) {
    merged$stop_times <- dplyr::distinct(merged$stop_times, trip_id, stop_sequence, .keep_all = TRUE)
  }
  if (!is.null(merged$stops)) {
    merged$stops <- dplyr::distinct(merged$stops, stop_id, .keep_all = TRUE)
  }

  message("Writing flat merged feed: ", outputZip)
  gtfsio::export_gtfs(gtfsio::new_gtfs(merged), outputZip)

  # brief summary for validation
  if (!is.null(merged$calendar)) {
    sd <- suppressWarnings(as.integer(as.character(merged$calendar$start_date)))
    ed <- suppressWarnings(as.integer(as.character(merged$calendar$end_date)))
    message("  routes: ", nrow(merged$routes),
            " | trips: ", nrow(merged$trips),
            " | stops: ", nrow(merged$stops))
    message("  calendar range: ", min(sd, na.rm = TRUE), " to ", max(ed, na.rm = TRUE))
  }

  return(invisible(outputZip))
}
