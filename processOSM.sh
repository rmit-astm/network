#!/bin/bash

extract=$1
crs=$2
output=$3

#extract='./data/melbourne_australia.osm'
#crs=28355
#output='./data/network3.sqlite'

# change to the directory this script is located in
cd "$(dirname "$0")"
# extract the roads from the osm file, put in temp.sqlite
ogr2ogr -update -overwrite -nln roads -f "SQLite" -dsco SPATIALITE=YES \
  -dialect SQLite -sql \
  "SELECT CAST(osm_id AS DOUBLE PRECISION) AS osm_id, highway, other_tags, \
    GEOMETRY FROM lines \
    WHERE (highway IS NOT NULL AND \
      NOT highway = 'bridleway' AND \
      NOT highway = 'bus_stop' AND \
      NOT highway = 'co' AND \
      NOT highway = 'platform' AND \
      NOT highway = 'raceway' AND \
      NOT highway = 'services' AND \
      NOT highway = 'traffic_island' AND \
      highway NOT LIKE '%construction%' AND \
      highway NOT LIKE '%proposed%' AND \
      highway NOT LIKE '%disused%' AND \
      highway NOT LIKE '%abandoned%') AND \
      (other_tags IS NULL OR
       (other_tags NOT LIKE '%busbar%' AND \
        other_tags NOT LIKE '%abandoned%' AND \
        other_tags NOT LIKE '%\"access\"=>\"private\"%')) " \
  ./data/temp.sqlite $extract
#      highway NOT LIKE '%service%' AND \
# Removed since some service roads are used as footpaths (e.g., Royal Exhibition
# building)

# bridleway can be used for walking and cycling (provided you give way to horses
# they are more common in the UK.

# extract the traffic signals, put in temp.sqlite
ogr2ogr -update -overwrite -nln roads_points -f "SQLite" -dsco SPATIALITE=YES \
  -dialect SQLite -sql \
  "SELECT CAST(osm_id AS DOUBLE PRECISION) AS osm_id, highway, other_tags, \
    GEOMETRY FROM points \
    WHERE highway LIKE '%traffic_signals%' " \
  ./data/temp.sqlite $extract

# the postgres database name.
DB_NAME="network_test"

# Delete the database if it already exists
COMMAND="psql -U postgres -c 'DROP DATABASE ${DB_NAME}' postgres"
eval $COMMAND
# Create the database and add the postgis extension
createdb -U postgres ${DB_NAME}
psql -c 'create extension postgis' ${DB_NAME} postgres

ogr2ogr -overwrite -lco GEOMETRY_NAME=geom -lco SCHEMA=public -f "PostgreSQL" \
  PG:"host=localhost port=5432 user=postgres dbname=${DB_NAME}" \
  -a_srs "EPSG:4326" ./data/temp.sqlite roads
ogr2ogr -overwrite -lco GEOMETRY_NAME=geom -lco SCHEMA=public -f "PostgreSQL" \
  PG:"host=localhost port=5432 user=postgres dbname=${DB_NAME}" \
  -a_srs "EPSG:4326" ./data/temp.sqlite roads_points
  
# run the sql statements
psql -U postgres -d ${DB_NAME} -a -f network.sql -v v1="$crs"

# extract the nodes, edges, and osm metadata to the network file
ogr2ogr -update -overwrite -f SQLite -dsco SPATIALITE=yes $output PG:"dbname=${DB_NAME} user=postgres" public.line_cut3 -nln edges
ogr2ogr -update -overwrite -f SQLite -update $output PG:"dbname=${DB_NAME} user=postgres" public.nodes_attributed -nln nodes
ogr2ogr -update -overwrite -f SQLite -update $output PG:"dbname=${DB_NAME} user=postgres" public.osm_metadata -nln osm_metadata

