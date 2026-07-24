# Network generation data

This directory contains input files required to generate MATSim networks for Bendigo and Melbourne, which can be found [here](https://osf.io/ajycn/). 

## Files to download

Download the following files (as required) for the relevant network location.

### Bendigo
| File                     | Description                                       |
|--------------------------|---------------------------------------------------|
| greater_bendigo.sqlite   | Boundary of the Greater Bendigo Local Government Area |
| victoria.sqlite          | Boundary of Victoria                              |
| dem_bendigo.tif          | Digital elevation model data for the Greater Bendigo area |
| NDVI_Bendigo_2023.tif    | NDVI data for the Greater Bendigo area              |
| TCC_Bendigo_5m.tif       | Tree canopy cover data for the Greater Bendigo area |
| school_zones_March_2024.sqlite | School zone locations for Victoria          |
| gtfs.zip                 | GTFS feed for Victoria (see 'GTFS feed' section below) |


### Melbourne
| File                     | Description                                       |
|--------------------------|---------------------------------------------------|
| greater_melbourne.sqlite | Boundary of the Greater Melbourne Greater Capital City Statistical Area |
| victoria.sqlite          | Boundary of Victoria                              |
| dem_melbourne.tif        | Digital elevation model data for the Greater Melbourne area |
| NDVI_Melbourne_2023.tif  | NDVI data for the Greater Melbourne area        |
| TCC_Melbourne_5m.tif       | Tree canopy cover data for the Greater Melbourne area |
| school_zones_March_2024.sqlite | School zone locations for Victoria          |
| gtfs.zip                 | GTFS feed for Victoria (see 'GTFS feed' section below) |


## Other files

The directory also contains the following other files, from which the region boundary files above were created.

| File                            | Description                                  |
|---------------------------------|----------------------------------------------|
| LGAs.zip                        | Local government areas of Victoria (Vicmap)  |
| GCCSA_2021_AUST_SHP_GDA2020.zip | Greater capital city statistical areas (ABS) |

And the following file, from which the school zone file above was created.

| File                            | Description                                  |
|---------------------------------|----------------------------------------------|
| Speed_Zones_March_2024.geojson  | Speed zones in Victoria (Department of Transport and Planning)  |



The file `data/data prep tools.R` contains:
* the script used to extract the region boundary files from the LGA and GCCSA files above,
* the script used to crop the digital elevation files from a DEM file for the whole of Victoria (available for download from https://discover.data.vic.gov.au/dataset/vicmap-elevation-dem-10m, 9.3 GB), and  
* the script used to extract the school speed zones from the speed zone file above.
Those scripts may also be useful to generate similar data input files for other locations if required.

## GTFS feed

The pipeline reads `gtfs.zip` as a single flat GTFS feed (with `stops.txt`,
`routes.txt` etc. at the top level). The current PTV feed, downloaded from
https://data.ptv.vic.gov.au/downloads/gtfs.zip, is instead a **nested bundle**:
the outer zip contains numbered folders (1..N), each holding a `google_transit.zip`
for one mode (1 = Regional Train, 2 = Metro Train, 3 = Metro Tram, 4 = Metro Bus,
5 = Regional Coach, 6 = Regional Bus). It also uses "extended" route types and
sets every sub-feed's `agency_id` to the same value.

Use `functions/prepareGtfs.R` to convert the raw PTV bundle into the flat feed the
pipeline expects. It merges folders 1–6 into one feed, sets each sub-feed's
`agency_id` to its folder number (which the mode classification relies on) and
normalizes extended route types to the basic types:

```r
source("functions/prepareGtfs.R"); library(dplyr)
prepareGtfs("./data/gtfs_ptv_bundle.zip", "./data/gtfs.zip", folders = 1:6)
```

By default `analysis_date` in `NetworkGenerator.R` is `NA`, which selects a
representative midweek day automatically from the downloaded feed's calendar. Set
it explicitly if you need the network built for a particular date.

The NDVI files were created from Sentinel 2 data using Google Earth Engine at https://code.earthengine.google.com/.

The Tree canopy cover raster data was sourced from here: https://discover.data.vic.gov.au/dataset/vicmap-vegetation-tree-extent and then manually resampled to 5m spatial resolution and merged together, and clipped to the Greater Bendigo boundary plus 10km buffer.
