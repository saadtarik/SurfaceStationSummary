## CreateBaseMap.R

CreateBaseMap <- function(lat.range = c(NA,NA),
                          long.range = c(NA,NA),
                          data.in = NULL){    
  # download the map tile
  base.map.in <- get_map(location = c(min(long.range),
                                      min(lat.range),
                                      max(long.range),
                                      max(lat.range)),
                         source = "osm")
  .e <- environment()
  # create the map object
  if (is.null(data.in)){
    base.map <- ggmap(base.map.in)
  } else {    
    base.map <- ggmap(base.map.in,
                      base_layer = ggplot(data = data.in,
                                          aes(x = LONG,
                                              y = LAT),
                                          environment = .e))
  }
  base.map <- base.map +
    labs(x = "Longitude",
         y = "Latitude") + 
    coord_map()
  print(base.map)
  return(base.map)
}