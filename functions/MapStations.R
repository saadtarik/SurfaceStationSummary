MapStations <- function(lat.range,
                        long.range,
                        base.map = NULL,
                        stations,
                        labels = FALSE){
  if (labels){
    # figure out the displacement of the label, which we will also use for the 
    # arrow to the original value
    stations$d.LONG <- diff(long.range)/50
    stations$d.LAT <- runif(NROW(stations), 
                            -diff(lat.range)/50, 
                            diff(lat.range)/50) 
  }
  
  if (is.null(base.map)){
    station.map <- CreateBaseMap(lat.range = lat.range,
                                 long.range = long.range,
                                 data.in = stations) 
  } else {
    station.map <- base.map %+% stations
    station.map <- station.map + aes(x = LONG,
                                     y = LAT)
  }    
  
  if (labels){
    # add labels
    station.map <- station.map +      
      geom_segment(aes(x = LONG,
                       xend = LONG + d.LONG,
                       y = LAT,
                       yend = LAT + d.LAT),
                   color = "black",
                   size = 0.25) + 
      geom_text(aes(x = LONG + 1.25*d.LONG,
                    y = LAT + d.LAT,
                    label = ID),
                size = 2,
                hjust = 0,
                vjust = 0.5,
                angle = 0)
  }  
  # finish up
  station.map <- station.map  +   
    geom_point(aes(x = LONG,
                   y = LAT),
               shape = "+") +
    coord_map() +
    labs(x = "Longitude",
         y = "Latitude") +
    theme_bw(base_size = 8, base_family = "")
  
  print(station.map)    
  return(station.map)
}

# see also solution that uses google to get the map
# http://stackoverflow.com/questions/13426470/justification-of-multiple-legends-in-ggmap-ggplot2