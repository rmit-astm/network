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

Before running the algorithm, adjust the parameters and input/output file names for your scenario in `NetworkGenerator.R`.
Adjustable parameters are listed under the Parameters sub-heading.

Running the algorithm requires an input parameter 'city', and adjustable parameters must be completed for that city, specifying locations of relevant input files and the applicable CRS. If running for a location for which 'city' parameters have not already been defined, then these must be added, using existing city parameters as a template.

The city parameters are as follows.

| Parameter          | Parameter type | Requirements                            |
|--------------------|----------------|-----------------------------------------|
| region             | .sqlite file   | The location of a file in sqlite format which defines the boundary of the area for which the OSM extract is required, to be used for a fully-detailed road and public transport network and extracted destinations.|
| surroundingRegion  | .sqlite file   | The location of a file in sqlite format which defines the boundary of a wider area for which the OSM extract is required, to be used for a sparser wide road and public transport network. |
| outputCrs          | CRS            |Specify the appropriate EPSG coordinate reference system number for the region.|
| fullExtractLocation | .osm.pbf file | Required if 'extractOsm' and 'useFullExtractHeld' are both set to 'T'.  The location of a file in .osm.pbf file which is already held (instead of downloading the .osm.pbf file) and is to be clipped to the region. |
| osmGpkg            | .gpkg file     | The location to which an OSM extract in .gpkg format will be saved, or where an existing .gpkg file is stored if already held.|
| unconfiguredSqlite | .sqlite file   | The location to which an unconfigured network in .sqlite format will be saved, or where an existing unconfigured network is stored if already held.|
| cropAreaPoly       | area location  | An optional parameter for cropping the OSM extract to a smaller test area. See https://github.com/JamesChevalier/cities/tree/master/australia/victoria for available locations.  |
| demFile            | .tif file      | Required if 'addElevation' is set to 'T'.  This must be the location of a digital elevation model raster file.   |
| ndviFile           | .tif file      | Required if 'addNDVI' is set to 'T'.  This must be the location of a raster file with NDVI values.   |
| treeCanopyCoverageFile | .tif file      | Required if 'addTreeCanopyCoverage' is set to 'T'.  This must be the location of a raster file with values 1 where tree canopy coverage is present and 0 where not.   |
| schoolZoneFile     | .sqlite file   | Required if 'addSchoolZones' is set to 'T'.  This must be the location of a file containing the location of roads with lower speed limits that apply during school times. |
| gtfs_feed          | gtfs .zip file | Required if 'addGtfs' or 'addDestinationLayer' is set to 'T'.  This must be the location of a zip file containing GTFS data. |

**The parameters assume that the region file and (if used) the demFile, ndviFile and gtfs_feed are stored in the 'data' subdirectory.**  See `data/README.md` for more detail on obtaining the data files. 

The algorithm will do the following:
* if 'downloadOsm' is set to 'T', download an OSM extract for the selected 'region' (and also including the lines layer for the broader 'surroundingRegion') and save it as a .gpkg file. 
* if 'networkFromOsm' is set to 'T', process the downloaded OSM extract to an unconfigured network in the form of an .sqlite file with layers of nodes, edges and osm tags ('osm_metadata').
* simplify the network, producing an output network in .sqlite format (with options to select .shp and .xml formats as well).
* if 'addDestination' is set to 'T', include a layer of destination points of interest for use in accessibility analysis, such as as supermarkets, doctors and schools, drawn from OSM and GTFS layers.

To run the network generation algorithm from the terminal, you need to run something like below, specifying your city and your desired output folder name as the arguments for `makeNetwork()`:
```
Rscript -e 'source("NetworkGenerator.R"); makeNetwork(, "Melbourne", "example")'

```

## Using the pipeline for other cities

The pipeline was developed and tested for Melbourne and Bendigo (Victoria, Australia), but is intended to work for other cities that have OpenStreetMap coverage and a GTFS feed. To build a network for another city, add a new 'city' block in `NetworkGenerator.R` using the existing ones as a template, and check the following. Users outside Australia should pay particular attention to the same points.

* **Coordinate system**: set `outputCrs` to a projected EPSG code suitable for the city (a metre-based local projection, not lat/long).
* **Region boundaries**: supply `region` (and `surroundingRegion`) boundary files for the city. If a wider surrounding region is not required, set `surroundingRegion` to the same file as `region`.
* **OpenStreetMap**: network quality depends on local OSM completeness, which varies between cities.
* **GTFS packaging**: the pipeline expects `gtfs_feed` to be a single flat GTFS zip (with `stops.txt`, `routes.txt` etc. at the top level). Some agencies (eg PTV in Victoria) instead distribute a nested bundle of per-mode feeds; use `functions/prepareGtfs.R` to flatten these first (see `data/README.md`).
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
