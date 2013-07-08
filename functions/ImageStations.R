showStation <- function(station,
                        precision = 0.0005,
                        image.half.width = 5* precision){
  print(station)
  station$LON <- station$LONG
  station$precision <- precision
  # get the image
  image.in <- get_map(location = c(station$LON,
                                   station$LAT),
                      source = "google",
                      maptype = "satellite",
                      zoom = 15)
  # create an image around this
  station.image <- ggmap(image.in) %+% station + aes(x = LON, y = LAT)
  station.image <- station.image +
    geom_point(shape = 16,
               color = "white",
               fill = "white") + 
    geom_rect(aes(xmin = LON - precision,
                  xmax = LON + precision,
                  ymin = LAT - precision,
                  ymax = LAT + precision,
                  colour=NULL,
                  group=NULL),
              color = "white",
              fill = NA) +
    geom_text(aes(x = LON - 2 * precision,
                  y = LAT + 2 * precision,
                  label = paste(LAT, "N,\n",
                                LON, "W", 
                                sep = "")),
              color = "white",
              size = 4,
              hjust = 0,
              vjust = 0) + 
    coord_map() +
    scale_x_continuous(limits=c(station$LON - image.half.width,
                                station$LON + image.half.width)) +
    scale_y_continuous(limits=c(station$LAT - image.half.width,
                                station$LAT + image.half.width)) +                      
    labs(x = "Longitude",
         y = "Latitude")
  print(station.image)
  return(station.image)
}