# MATSim network for Melbourne
`master`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=master) `dev`![passing?](https://github.com/matsim-melbourne/network/workflows/build/badge.svg?branch=dev)

This page explains the steps for building a road network model for active transport simulation models such as MATSim, including active transportation related infrastructure and attributes.    

## Publications
- Jafari, A., Both, A., Singh, D., Gunn, L., & Giles-Corti, B. (2022). [Building the road network for city-scale active transport simulation models](https://doi.org/10.1016/j.simpat.2021.102398). *Simulation Modelling Practice and Theory*, 114, 102398 ( [Pre-print version](https://arxiv.org/abs/2104.03063) )

## Prerequisites
* GDAL
* R 4.2+

## Building the network

Network generation code is written primarily in R programming language,therefore a working knowledge of R is expected.

All required R packages must be installed before running the algorithm. `renv` will take of that for you and you just need to run the following in R to install the packages:
```
install.packages("renv")
renv::restore()
```

The parameters for a build are set in a YAML config file in the `config` directory. `config/default.yml` lists every parameter, with its documentation and default value, grouped in sections (`osm`, `elevation`, `gtfs` etc). Each city has its own config file (`config/melbourne.yml`, `config/bendigo.yml`, `config/victoria.yml`) which only sets the values that differ from the defaults, using the same sections, and is merged over `config/default.yml` when the network is built. To build a network for a new location, add a config file for it, using the existing ones as a template.

Before anything is built, the config is checked: parameter names that are not in `config/default.yml` are rejected, and every input file required by the selected options must exist. The parameters used are saved as `config.yml` alongside the build's outputs.

The city-specific parameters are as follows.

| Parameter          | Parameter type | Requirements                            |
|--------------------|----------------|-----------------------------------------|
| region             | .sqlite file   | The location of a file in sqlite format which defines the boundary of the area for which the OSM extract is required, to be used for a fully-detailed road and public transport network and extracted destinations.|
| surroundingRegion  | .sqlite file   | The location of a file in sqlite format which defines the boundary of a wider area for which the OSM extract is required, to be used for a sparser wide road and public transport network. |
| outputCrs          | CRS            |Specify the appropriate EPSG coordinate reference system number for the region.|
| fullExtractLocation | .osm.pbf file | Required if 'extractOsm' and 'useFullExtractHeld' are both set to 'true'.  The location of a file in .osm.pbf file which is already held (instead of downloading the .osm.pbf file) and is to be clipped to the region. |
| osmGpkg            | .gpkg file     | The location to which an OSM extract in .gpkg format will be saved, or where an existing .gpkg file is stored if already held.|
| unconfiguredSqlite | .sqlite file   | The location to which an unconfigured network in .sqlite format will be saved, or where an existing unconfigured network is stored if already held.|
| cropAreaPoly       | area location  | An optional parameter for cropping the OSM extract to a smaller test area. See https://github.com/JamesChevalier/cities/tree/master/australia/victoria for available locations.  |
| demFile            | .tif file      | Required if 'addElevation' is set to 'true'.  This must be the location of a digital elevation model raster file.   |
| ndviFile           | .tif file      | Required if 'addNDVI' is set to 'true'.  This must be the location of a raster file with NDVI values.   |
| treeCanopyCoverFile | .tif file      | Required if 'addTreeCanopyCover' is set to 'true'.  This must be the location of a raster file with values 1 where tree canopy coverage is present and 0 where not.   |
| schoolZoneFile     | .sqlite file   | Required if 'addSchoolZones' is set to 'true'.  This must be the location of a file containing the location of roads with lower speed limits that apply during school times. |
| gtfs_feed          | gtfs .zip file | Required if 'addGtfs' or 'addDestinationLayer' is set to 'true'.  This must be the location of a zip file containing GTFS data in flat format (that is, with 'stops.txt' etc at the top level); it is the location to which the feed will be saved if 'downloadGtfs' is set to 'true', or where an existing feed is stored if not. |
| gtfsUrl            | URL            | Required if 'downloadGtfs' is set to 'true'.  The location from which the GTFS feed is to be downloaded. |
| gtfsBundle         | .zip file      | Required if 'downloadGtfs' and 'gtfsNested' are both set to 'true'.  The location to which the downloaded bundle will be saved before being converted to a flat feed. |

**The parameters assume that the region file and (if used) the demFile, ndviFile and gtfs_feed are stored in the 'data' subdirectory.**  See `data/README.md` for more detail on obtaining the data files. 

The algorithm will do the following:
* if 'extractOsm' is set to 'true', download an OSM extract for the selected 'region' (and also including the lines layer for the broader 'surroundingRegion') and save it as a .gpkg file. 
* if 'networkFromOsm' is set to 'true', process the downloaded OSM extract to an unconfigured network in the form of an .sqlite file with layers of nodes, edges and osm tags ('osm_metadata').
* if 'downloadGtfs' is set to 'true' (and public transport or destinations are required), download the GTFS feed from 'gtfsUrl' and, if 'gtfsNested' is set to 'true', convert the downloaded bundle to a single flat feed.
* simplify the network, producing an output network in .sqlite format (with options to select .shp and .xml formats as well).
* if 'addDestinationLayer' is set to 'true', include a layer of destination points of interest for use in accessibility analysis, such as as supermarkets, doctors and schools, drawn from OSM and GTFS layers.

To run the network generation algorithm from the terminal, give the config file, optionally followed by parameters to change for this run only, as `name=value`:
```
Rscript NetworkGenerator.R config/melbourne.yml
Rscript NetworkGenerator.R config/melbourne.yml outputSubdirectory=example extractOsm=false networkFromOsm=false
```

or from R:
```
source("NetworkGenerator.R")
makeNetwork("config/melbourne.yml", overrides = list(outputSubdirectory = "example"))
```

Overrides are useful for one-off changes, such as resuming a build from the saved OSM intermediates, without editing the config files.

Versions up to v1.2 set the parameters in `NetworkGenerator.R` itself and were run with `makeNetwork(city = "Melbourne")`; config files were introduced in v2.0.

## Using the pipeline for other cities

The pipeline was developed and tested for Melbourne and Bendigo (Victoria, Australia), but is intended to work for other cities that have OpenStreetMap coverage and a GTFS feed. To build a network for another city, add a config file for it in `config` using the existing ones as a template, and check the following. Users outside Australia should pay particular attention to the same points.

* **Coordinate system**: set `outputCrs` to a projected EPSG code suitable for the city (a metre-based local projection, not lat/long).
* **Region boundaries**: supply `region` (and `surroundingRegion`) boundary files for the city. If a wider surrounding region is not required, set `surroundingRegion` to the same file as `region`.
* **OpenStreetMap**: network quality depends on local OSM completeness, which varies between cities.
* **GTFS packaging**: the pipeline expects `gtfs_feed` to be a single flat GTFS zip (with `stops.txt`, `routes.txt` etc. at the top level). Set `gtfsUrl` to the city's own feed location. Some agencies (eg PTV in Victoria) instead distribute a nested bundle holding a separate feed for each mode: for those, leave `gtfsNested` as `true` and set `gtfsFolders` to the mode folders required; for an ordinary flat feed, set `gtfsNested` to `false` (see `data/README.md`).
* **GTFS route types**: public transport modes are classified from the standard GTFS `route_type` codes (see `getPTStops.R` and `gtfs2PtNetwork.R`). Feeds that use non-standard or 'extended' route type codes need to be converted first, or the codes in those two functions adjusted.
* **Regional service filter**: where `surroundingRegion` differs from `region`, `gtfs2PtNetwork.R` keeps only regional services in the wider area, using agency ids specific to the PTV feed (1 = V/Line, 5, 6 = regional coach/bus). For other feeds, either set `surroundingRegion` to the same file as `region`, or edit these ids.

## Troubleshooting
### Installing sf
SF package in R requires a few dependencies, see https://r-spatial.github.io/sf/ for more details.

### iGraph install on macOS
If running R from a homebrew install, be sure to unlink suite-sparse before installing iGraph
```
brew unlink suite-sparse
```

### sf install on macOS
The sf library requires the following to run
```
brew install gdal
brew install udunits
```
