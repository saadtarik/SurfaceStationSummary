showStation <- function(station,
                        precision,
                        image.half.width){
  print(station)
  station$LON <- station$LONG
  # get the image
  image.in <- get_map(location = c(station$LON,
                                   station$LAT),
                      source = "google",
                      maptype = "satellite",
                      zoom = 15)
  # create an image around this
  station.image <- ggmap(image.in)
  station.image <- station.image +
    geom_point(data = station,
               aes(x = LON,
                   y = LAT),         
               shape = 16,
               color = "white",
               fill = "white") + 
    geom_rect(inherit.aes = FALSE,
              data = station,
              aes(xmin = LON - precision,
                  xmax = LON + precision,
                  ymin = LAT - precision,
                  ymax = LAT + precision,
                  colour=NULL,
                  group=NULL),
              color = "white",
              fill = NA) +
    geom_text(data = station,
              aes(x = LON - 2 * precision,
                  y = LAT + 2 * precision,
                  label = paste(LAT, "°N,\n",
                                LON, "°W")),
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
         y = "Latitude",
         title = paste("Region around station ",
                       station$NAME, 
                       " (USAF ",
                       station$USAF,
                       ")",
                       sep = ""))
  print(station.image)
  return(station.image)
}

ImageStations <- function(stations = NULL,
                          precision = 0.0005,
                          image.half.width = 5*precision,
                          data.dir = NULL){  
  # work through, row by row
  for (rowi in 1:dim(stations)[1]){    
    
    station <- stations[rowi,]
    station.image <- showStation(station,
                                 precision = precision,
                                 image.half.width = image.half.width)
    # display to screen
    station.image
    
    # write this image to file
    filepath <- file.path(data.dir,
                          "figures","StationImages",
                          paste(station$USAF, "_",
                                gsub("[/]","-",station$NAME),
                                ".png",
                                sep = ""))
    cat("Saving image to",filepath,"\n")
    ggsave(filepath,
           plot = station.image,
           scale = 1, 
           width = 9,
           height = 6, units = c("in"),
           dpi = 300, limitsize = TRUE) 
  }
}