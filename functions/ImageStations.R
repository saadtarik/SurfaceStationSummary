imageStation <- function(station){
  print(station)
  station$LON <- station$LONG
  
  # get the precision
  n.dp.lat = nchar(station$LAT) - (nchar(as.character(round(as.numeric(station$LAT),
                                                            digits=0))) + 1)
  n.dp.lon = nchar(station$LON) - (nchar(as.character(round(as.numeric(station$LON),
                                                            digits=0))) + 1)
  station$LAT.precision <- 5*10^-(n.dp.lat+1)
  station$LON.precision <- 5*10^-(n.dp.lon+1)
  # get the bounding box
  station$LON.min <- station$LON - station$LON.precision
  station$LON.max <- station$LON + station$LON.precision
  station$LAT.min <- station$LAT - station$LAT.precision
  station$LAT.max <- station$LAT + station$LAT.precision  
  station$max.precision <- max(station$LAT.precision,
                               station$LON.precision)
  
  # get the image zoom required  
  n.dp.max <- min(n.dp.lat, n.dp.lon)
  if (n.dp.max == 3){zoom = 15}
  if (n.dp.max == 2){zoom = 13}
  if (n.dp.max == 1){zoom = 10}
  if (n.dp.max == 0){zoom = 9}
  
  # grab the image
  image.in <- get_map(location = c(station$LON,
                                   station$LAT),
                      source = "google",
                      maptype = "satellite",
                      zoom = zoom)
  # create an image around this
  station.image <- ggmap(image.in) %+% station + aes(x = LON, y = LAT)
  station.image <- station.image +
    geom_point(shape = 16,
               color = "white",
               fill = "white") + 
    geom_rect(aes(xmin = LON.min,
                  xmax = LON.max,
                  ymin = LAT.min,
                  ymax = LAT.max,
                  colour=NULL,
                  group=NULL),
              color = "white",
              fill = NA) +
    geom_text(aes(x = LON.min,
                  y = LAT.max + 0.1*LAT.precision,
                  label = paste(LAT, " N,\n",
                                LON, " W", 
                                sep = "")),
              color = "white",
              size = 4,
              hjust = 0,
              vjust = 0) + 
    coord_map() +
    scale_x_continuous(limits=c(station$LON - 5*station$max.precision,
                                station$LON + 5*station$max.precision)) +
    scale_y_continuous(limits=c(station$LAT - 5*station$max.precision,
                                station$LAT + 5*station$max.precision)) +                      
    labs(x = "Longitude",
         y = "Latitude")
  print(station.image)
  return(station.image)
}