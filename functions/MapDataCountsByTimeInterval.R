MapDataCountsByTimeInterval <- function(obs.all,
                                        stations.all,
                                        interval = "overall",
                                        plot.variable = "WIND.SPD.COUNT",
                                        lat.range = c(NA,NA),
                                        long.range = c(NA,NA)){
  cat("Mapping data counts\n")
  # get the counts
  cat("...calculating data counts\n")
  counts <- merge(stations.all,
                  GetDataCountsByTimeInterval(obs.all, 
                                              interval = interval),
                  by.x = "USAF",
                  by.y = "USAF")
  
  # create the map and add the data to it
  cat("...requesting a base map\n")
  map <- CreateBaseMap(lat.range = lat.range,
                       long.range = long.range,
                       data.in = counts)
  map <- map +
    geom_point(aes_string(size = plot.variable)) +
    scale_size_area(name = "Number of \ndata points") 
  
  # need to add to this plot depending on the time interval we chose
  cat("...customzing base map\n")  
  if(interval == "year"){
    map <- map +
      facet_wrap(~YR)    
  }         
  print(map)
  return(map)
}