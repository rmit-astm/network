# function to add from- and to-node details to links
# based on 'makeEdgesDirect.R', but without changing geometry

addNodeDetails <- function(nodes_current,edges_current){
  
  edges_current <- edges_current %>%
    filter(!is.na(from_id)) %>%
    left_join(st_drop_geometry(nodes_current),by=c("from_id"="id")) %>%
    rename(fromX=X,fromY=Y) %>%
    left_join(st_drop_geometry(nodes_current),by=c("to_id"="id")) %>%
    rename(toX=X,toY=Y)
  return(list(nodes_current,edges_current))
}
