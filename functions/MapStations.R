# Map stations on base map ----
MapStations <- function(base.map,
                        stations,
                        labels = FALSE){
  station.map <- base.map +
    geom_point(data = stations,
               aes(x = LONG,
                   y = LAT),
               shape = "+") +    
    coord_map() +
    labs(x = "Longitude",
         y = "Latitude") +
    theme_bw(base_size = 8, base_family = "")
  
  if (labels){
    station.map <- station.map +
      geom_text(data = stations,
                aes(x = LONG,
                    y = LAT,
                    label = USAF),
                size = 2,
                hjust = 0,
                vjust = 0.5,
                angle = 30)
  }  
  print(station.map)    
  return(station.map)
}

# see also solution that uses google to get the map
# http://stackoverflow.com/questions/13426470/justification-of-multiple-legends-in-ggmap-ggplot2