# read the network generation parameters from a YAML config file
#
# The config file (eg 'config/melbourne.yml') is merged over 'config/default.yml',
# which lists every parameter with its default value, grouped in sections.
# 'overrides' is a named list of further values, given by parameter name without
# its section (eg list(extractOsm = FALSE)), which are applied last; it is useful
# for one-off changes, such as resuming a build from saved intermediates.
#
# Before anything is built, the parameters are checked: names that are not in
# 'config/default.yml' are rejected (so a mistyped name is not silently ignored),
# and every input file required by the selected options must exist.
#
# Returns a list with 'params' (a flat named list of parameter values, with
# 'null' converted to NA and 'analysis_date' to a Date) and 'resolved' (the
# merged config, in the same sections as the files, for saving with the outputs).

readNetworkConfig <- function(configFile,
                              overrides = list(),
                              defaultFile = "./config/default.yml") {

  # configFile = "./config/melbourne.yml"
  # overrides = list(extractOsm = FALSE, networkFromOsm = FALSE)

  if (!file.exists(defaultFile)) stop("Default config file not found: ", defaultFile)
  if (!file.exists(configFile)) stop("Config file not found: ", configFile)
  defaults <- yaml::read_yaml(defaultFile)
  config <- yaml::read_yaml(configFile)
  if (is.null(config)) config <- list()

  # path of each parameter in the defaults, eg "gtfs/addGtfs"; a section is a
  # named list, anything else (including an unnamed sequence) is a value
  isSection <- function(x) is.list(x) && !is.null(names(x))
  paramPaths <- function(x, prefix = "") {
    unlist(lapply(names(x), function(n) {
      path <- paste0(prefix, n)
      if (isSection(x[[n]])) paramPaths(x[[n]], paste0(path, "/")) else path
    }))
  }
  defaultPaths <- paramPaths(defaults)
  paramNames <- basename(defaultPaths)
  if (any(duplicated(paramNames))) {
    stop("Parameter names in ", defaultFile, " must be unique: ",
         paste(unique(paramNames[duplicated(paramNames)]), collapse = ", "))
  }

  # reject parameters that aren't in the defaults, or are in the wrong section
  unknownPaths <- setdiff(paramPaths(config), defaultPaths)
  if (length(unknownPaths) > 0) {
    hints <- sapply(unknownPaths, function(p) {
      match <- defaultPaths[paramNames == basename(p)]
      if (length(match) == 1) paste0(p, " (should be ", match, ")") else p
    })
    stop("Unknown parameters in ", configFile, ": ", paste(hints, collapse = ", "))
  }
  unknownOverrides <- setdiff(names(overrides), paramNames)
  if (length(unknownOverrides) > 0) {
    stop("Unknown parameters in overrides: ", paste(unknownOverrides, collapse = ", "))
  }

  # merge: defaults, then config file, then overrides (placed in their sections)
  merged <- modifyList(defaults, config)
  for (n in names(overrides)) {
    path <- strsplit(defaultPaths[paramNames == n], "/")[[1]]
    if (length(path) == 1) {
      merged[n] <- list(overrides[[n]])
    } else {
      merged[[path[1]]][path[2]] <- list(overrides[[n]])
    }
  }

  # flatten to one value per parameter, as used in makeNetwork()
  params <- list()
  for (path in defaultPaths) {
    parts <- strsplit(path, "/")[[1]]
    value <- if (length(parts) == 1) merged[[parts]] else merged[[parts[1]]][[parts[2]]]
    params[[basename(path)]] <- if (is.null(value)) NA else value
  }
  if (!is.na(params$analysis_date)) {
    params$analysis_date <- as.Date(as.character(params$analysis_date))
  }

  # check that the input files required by the selected options exist
  required <- c(region = TRUE,
                surroundingRegion = TRUE,
                fullExtractLocation = params$extractOsm & params$useFullExtractHeld & !params$startFromCheckpoint,
                osmGpkg = !params$extractOsm & params$networkFromOsm & !params$startFromCheckpoint,
                unconfiguredSqlite = !params$networkFromOsm & !params$startFromCheckpoint,
                checkpointSqlite = params$startFromCheckpoint,
                demFile = params$addElevation,
                ndviFile = params$addNDVI,
                treeCanopyCoverFile = params$addTreeCanopyCover,
                schoolZoneFile = params$addSchoolZones,
                gtfs_feed = (params$addGtfs | params$addDestinationLayer) & !params$downloadGtfs,
                bendigoParking = params$addBendigoData,
                bendigoBikeRacks = params$addBendigoData,
                bendigoEverydayRoutes = params$addBendigoData,
                bendigoExistingProtected = params$addBendigoData,
                bendigoProposedProtected = params$addBendigoData)
  problems <- c()
  for (n in names(required)[required]) {
    if (is.na(params[[n]]) || params[[n]] == "") {
      problems <- c(problems, paste0(n, " is not set"))
    } else if (!file.exists(params[[n]])) {
      problems <- c(problems, paste0(n, " not found at '", params[[n]], "'"))
    }
  }
  if (params$saveCheckpoint & params$checkpointSqlite == "") {
    problems <- c(problems, "checkpointSqlite is not set, but saveCheckpoint is true")
  }
  if (params$crop2Area & params$cropAreaPoly == "") {
    problems <- c(problems, "cropAreaPoly is not set, but crop2Area is true")
  }
  if (params$downloadGtfs & (params$addGtfs | params$addDestinationLayer) & params$gtfsUrl == "") {
    problems <- c(problems, "gtfsUrl is not set, but downloadGtfs is true")
  }
  if (length(problems) > 0) {
    stop("Config ", configFile, " can't be used:\n  ", paste(problems, collapse = "\n  "))
  }

  # rebuild the merged config in its sections from the final values, for saving
  # (merging drops parameters set to 'null', so they are restored here)
  resolved <- defaults
  for (path in defaultPaths) {
    parts <- strsplit(path, "/")[[1]]
    value <- params[[basename(path)]]
    value <- if (length(value) == 1 && is.na(value)) list(NULL) else
      list(if (inherits(value, "Date")) as.character(value) else value)
    if (length(parts) == 1) resolved[parts] <- value else resolved[[parts[1]]][parts[2]] <- value
  }

  return(list(params = params, resolved = resolved))
}
