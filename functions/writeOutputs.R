
# SQlite ------------------------------------------------------------------
exportSQlite <- function(networkFinal, outputDir, outputCrs){
  
  cat('\n')
  echo(paste0('Writing the sqlite output: ', nrow(networkFinal[[2]]), 
              ' links and ', nrow(networkFinal[[1]]),' nodes\n'))
  
  # dir.create('./generatedNetworks/', showWarnings = FALSE)
  
  if(class(networkFinal[[1]])[1]!="sf"){
    networkFinal[[1]] <- networkFinal[[1]] %>% 
      mutate(GEOMETRY=paste0("POINT(",x," ",y,")")) %>%
      st_as_sf(wkt = "GEOMETRY", crs = outputCrs) %>% 
      as.data.frame() %>%
      st_sf()
  }
  if(class(networkFinal[[2]])[1]!="sf"){
    networkFinal[[2]] <- networkFinal[[2]] %>% 
      mutate(GEOMETRY=paste0("LINESTRING(",fromX," ",fromY,",",toX," ",toY,")")) %>%
      st_as_sf(wkt = "GEOMETRY", crs = outputCrs) %>% 
      as.data.frame() %>%
      st_sf()
  }
  
  # writing sqlite outputs
  st_write(networkFinal[[2]], paste0(outputDir,'/network.sqlite'), 
           layer = 'links', driver = 'SQLite', layer_options = 'GEOMETRY=AS_XY',
           delete_layer = T)
  st_write(networkFinal[[1]], paste0(outputDir,'/network.sqlite'),
           layer = 'nodes', driver = 'SQLite', layer_options = 'GEOMETRY=AS_XY',
           delete_layer = T)
  if (length(networkFinal) > 2) {
    st_write(networkFinal[[3]], paste0(outputDir,'/network.sqlite'),
             layer = 'destinations', driver = 'SQLite', 
             layer_options = 'GEOMETRY=AS_XY', delete_layer = T)
  }
  
  echo(paste0('Finished generating the sqlite output\n'))
}

# ShapeFile ---------------------------------------------------------------
exportShp <- function(networkFinal, outputDir, outputCrs){
  
  cat('\n')
  echo(paste0('Writing the ShapeFile output: ', nrow(networkFinal[[2]]), 
              ' links and ', nrow(networkFinal[[1]]),' nodes\n'))
  
  if(class(networkFinal[[1]])[1]!="sf"){
    networkFinal[[1]] <- networkFinal[[1]] %>% 
      mutate(GEOMETRY=paste0("POINT(",x," ",y,")")) %>%
      st_as_sf(wkt = "GEOMETRY", crs = outputCrs) %>% 
      as.data.frame() %>%
      st_sf()
  }
  if(class(networkFinal[[2]])[1]!="sf"){
    networkFinal[[2]] <- networkFinal[[2]] %>% 
      mutate(GEOMETRY=paste0("LINESTRING(",fromX," ",fromY,",",toX," ",toY,")")) %>%
      st_as_sf(wkt = "GEOMETRY", crs = outputCrs) %>% 
      as.data.frame() %>%
      st_sf()
  }
  shpDir <- paste0(outputDir,"/shapefiles")
  dir.create(shpDir, showWarnings = FALSE)
  # writing ShapeFile outputs
  st_write(networkFinal[[2]], paste0(shpDir,'/links.shp'), 
           driver = "ESRI Shapefile", layer_options = 'GEOMETRY=AS_XY', 
           delete_layer = T)
  st_write(networkFinal[[1]], paste0(shpDir,'/nodes.shp'), 
           driver = "ESRI Shapefile", layer_options = 'GEOMETRY=AS_XY', 
           delete_layer = T)
  if (length(networkFinal) > 2) {
    message("When writing destinations to shapefile, long 'other_tags' may be truncated; consider using sqlite instead.")
    dest.pt <- st_collection_extract(networkFinal[[3]], type = "POINT")
    dest.poly <- st_collection_extract(networkFinal[[3]], type = "POLYGON")
    st_write(dest.pt, paste0(shpDir,'/destinations_point.shp'),
             driver = "ESRI Shapefile", layer_options = 'GEOMETRY=AS_XY', 
             delete_layer = T)
    st_write(dest.poly, paste0(shpDir,'/destinations_polygon.shp'),
             driver = "ESRI Shapefile", layer_options = 'GEOMETRY=AS_XY', 
             delete_layer = T)
  }
  
  echo(paste0('Finished generating the ShapeFile output\n'))
  
}

# XML ---------------------------------------------------------------------
exportXML <- function(networkFinal, outputDir){
  # Functions ---------------------------------------------------------------
  # write lines to the file in chunks of rows, as the text for a whole network
  # can be too long for a single string
  writeRows <- function(rows) {
    for (chunk in split(rows, ceiling(seq_along(rows) / 100000))) {
      writeLines(chunk, xml_con, sep = "")
    }
  }
  
  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]
    if(length(add)!=0) data[add] <- NA
    data
  }
  
  # Prefix ------------------------------------------------------------------
  cat('\n')
  nodes <- networkFinal[[1]]
  links <- networkFinal[[2]]
  
  echo(paste0('Writing the XML output: ', nrow(links), ' links and ', nrow(nodes),' nodes\n'))
  
  echo('Starting to write the XML\n')
  # Set the output file
  # dir.create('./generatedNetworks/', showWarnings = FALSE)
  xml_file <- paste0(outputDir,'/network.xml')
  
  xml_con <- file(xml_file, "w")
  on.exit(close(xml_con), add = TRUE)
  
  # Adding the prefix
  str_header <- paste0("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
                       "<!DOCTYPE network SYSTEM \"http://www.matsim.org/files/dtd/network_v2.dtd\">\n",
                       "<network>\n",
                       "<nodes>\n") 
  writeRows(str_header)
  
  
  # Nodes -------------------------------------------------------------------
  if(class(nodes)[1]=="sf") nodes <- st_drop_geometry(nodes)
  echo('Starting to add nodes to XML\n')
  
  if ("z" %in% colnames(nodes)){
    nodes <- nodes %>% 
      mutate(z = if_else(is.na(z), true = 10, z))
    writeRows(paste0("    <node id=\"", nodes$id,
                     "\" x=\"", nodes$x,
                     "\" y=\"", nodes$y,
                     "\" z=\"", nodes$z, "\" />", "\n"))
  }else{
    writeRows(paste0("    <node id=\"", nodes$id,
                     "\" x=\"", nodes$x,
                     "\" y=\"", nodes$y, "\" />", "\n"))
  }
  writeRows("</nodes>\n")
  
  # Links -------------------------------------------------------------------
  if(class(links)[1]=="sf") links <- st_drop_geometry(links)
  # Adding empty id column if doesn't exist
  if(!("id" %in% colnames(links))) links <- links %>% mutate(id=NA)
  if(!("fwd_slope_pct" %in% colnames(links))) links <- links %>% mutate(fwd_slope_pct=NA, rvs_slope_pct=NA)
  
  # # Adding a reverse links for bi-directionals - not required as makeEdgesOneway has made them all oneway
  # bi_links <- links %>% 
  #   filter(is_oneway==0) %>% 
  #   rename(from_id=to_id, to_id=from_id, toX=fromX, toY=fromY, fromX=toX, 
  #          fromY=toY, slope=rvs_slope_pct) %>% 
  #   mutate(id=NA) %>%
  #   dplyr::select(from_id, to_id, fromX, fromY, toX, toY, length, freespeed, 
  #                 permlanes, capacity, is_oneway, cycleway, highway, surface, 
  #                 slope, is_cycle, is_walk, is_car, modes, id)
  # 
  # links <- rbind( 
  #   {links %>% dplyr::select(from_id, to_id, fromX, fromY, toX, toY, length, freespeed, 
  #                            permlanes, capacity, is_oneway, cycleway, highway, surface, 
  #                            slope=fwd_slope_pct, is_cycle, is_walk, is_car, modes, id)},
  #   bi_links) 
  
  # Adding bicycle and extra information
  links <-  fncols(links, c("id","osm_id", "highway", "cycleway","slope_pct", 
                            "bicycleInfrastructureSpeedFactor", "lvl_traf_stress")) 
  links <- links %>%
    # mutate(id = ifelse(is.na(id),row_number(),id)) %>%
    mutate(id = case_when(
      is.na(id) & "link_id" %in% names(links) ~ as.character(link_id),
      is.na(id) ~ as.character(row_number()),
      TRUE ~ as.character(id))
    ) %>%
    mutate(type = replace(highway, is.na(highway), "NotSpecified")) %>% 
    mutate(surface = ifelse(is.na(surface),"asphalt",surface)) %>% 
    mutate(cycleway = replace(cycleway, is.na(cycleway),"No")) %>% 
    mutate(slope_pct = replace(slope_pct, is.na(slope_pct),"NA")) %>% 
    mutate(bicycleInfrastructureSpeedFactor = 1) %>% 
    mutate(lvl_traf_stress = replace(lvl_traf_stress, is.na(lvl_traf_stress),"NA"))
    
  # Adding links
  echo('\n')
  echo('Starting to add links to XML\n')
  writeRows("<links>\n")
  writeRows(paste0("    <link id=\"", as.character(links$id),
                   "\" from=\"", as.character(links$from_id),
                   "\" to=\"", as.character(links$to_id),
                   "\" length=\"", links$length,
                   "\" capacity=\"", links$capacity,
                   "\" freespeed=\"", links$freespeed,
                   "\" permlanes=\"", links$permlanes,
                   "\" oneway=\"", "1",
                   "\" modes=\"", as.character(links$modes),
                   "\">\n",
                   "    <attributes>\n",
                   "        <attribute name=\"type\" class=\"java.lang.String\">", 
                   links$type, "</attribute>\n",
                   "        <attribute name=\"cycleway\" class=\"java.lang.String\">", 
                   links$cycleway, "</attribute>\n",
                   "        <attribute name=\"surface\" class=\"java.lang.String\">", 
                   links$surface, "</attribute>\n",
                   "        <attribute name=\"slopePct\" class=\"java.lang.String\">", 
                   links$slope_pct, "</attribute>\n",
                   "        <attribute name=\"bicycleInfrastructureSpeedFactor\" class=\"java.lang.String\">", 
                   links$bicycleInfrastructureSpeedFactor, "</attribute>\n",
                   "        <attribute name=\"lvl_traf_stress\" class=\"java.lang.String\">", 
                   links$lvl_traf_stress, "</attribute>\n",
                   "    </attributes>\n",
                   "    </link>\n"))
  
  writeRows(paste0("</links>\n", "</network>\n"))
  echo(paste0('Finished generating the xml output\n'))
}
