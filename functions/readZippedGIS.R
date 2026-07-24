# Extracts a zipped shapefile, mapinfo file, gpkg, etc, then reads uzing st_read
# 'subpath' is the string between the top zipped file and the ultimate file, 
#   eg "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMTRANS"
# 'file' not needed for files that don't have layers (eg shapefiles) if there is only one in the directory
# use 'file' (rather than 'layer') for shapefiles and mapinfo files; use both for gpkg [and sqlite?]
read_zipped_GIS <- function(zipfile, subpath = "", file = NULL, layer = NULL) {
  temp <- tempfile()
  unzip(zipfile, exdir = temp)
  if (is.null(layer)) {
    st_read(paste0(temp, subpath, file))
  } else {
    st_read(paste0(temp, subpath, file), layer)
  }
}