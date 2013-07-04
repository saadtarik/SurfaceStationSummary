MapDataCountsByTimeInterval <- function(obs.all,
                                        stations.all,
                                        interval = "overall",
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
                       counts)
  map <- map + geom_point(aes(x = LONG,
                              y = LAT,
                              size = WIND.SPD.COUNT)) +
    scale_size_area(name = "Number of \nwind speed data") + 
    theme(legend.position = c(1,0),
          legend.justification = c(1,0)) + 
    coord_map()
  print(map)
  # need to add to this plot depending on the time interval we chose
  switch(interval,
         overall = MapDataCountsOverall(counts,
                                        map),
         year = MapDataCountsByYear(counts,
                                    map))
}

MapDataCountsOverall <- function(counts,
                                 map){
  # don't actually need to change anything
  count.map <- map 
  print(count.map)
  return(count.map)
}

MapDataCountsByYear <- function(counts,
                                map){
  # need to switch to faceting by year
  cat("...adding faceting\n")
  count.map <- map + 
    facet_wrap(~YR) +    
    print(count.map)
  return(count.map)
}