FindStations <- function(country.code = NULL,
                         year.range = c(NULL,NULL),
                         lat.range = c(-1,1),
                         long.range = c(45,55),
                         data.dir = getwd){
  
  # Download the station list ----
  file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv"
  try.count = 0
  repeat {        
    if (is.na(file.info(file.path(data.dir,"data","ish-history.csv"))$size)){
      try(download.file(file, 
                        file.path(data.dir,"data","ish-history.csv"),
                        quiet = TRUE))  
    }else{
      if (file.info(file.path(data.dir,"data","ish-history.csv"))$size > 0){
        break
      } else {
        try(download.file(file, 
                          file.path(data.dir,"data","ish-history.csv"),
                          quiet = TRUE))  
      }
    }
    try.count <- try.count +1
    if (try.count > 10){
      cat("Tried 10 times to download is-history.csv. Giving up!")
      break
    }    
  }
  st <- read.csv(file.path(data.dir,"data/ish-history.csv"))
  
  ## Get station data ----
  # define the names that we will need later
  names(st)[c(3, 9, 10)] <- c("NAME", "LONG", "ELEV")
  st <- st[, -5]
  # fix something that means that names get treated as factors
  st$NAME <- as.character(st$NAME)
  # get more metadata
  st$LAT <- st$LAT/1000
  st$LONG <- st$LONG/1000
  st$ELEV <- st$ELEV/10
  st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
  st$END <- as.numeric(substr(st$END, 1, 4))
  
  # Filter stations ----
  # check the USAF ID
  valid.USAF <- !(st$USAF == 999999)
  
  # check the country code
  if (!is.null(country.code)){
    in.country <- (st$CTRY == country.code)
  } else {
    in.country < rep(as.logical("TRUE"),length(st))
  }
  
  # find stations that are within the bounding box
  if (!is.null(lat.range[1])){
    gteq.lat.min <- (st$LAT >= lat.range[1])
  } else{
    # all stations pass
    gteq.lat.min <- rep(as.logical("TRUE"),length(st))
  }
  if (!is.null(lat.range[2])){
    lteq.lat.max <- (st$LAT <= lat.range[2])
  } else{ # all stations pass
    lteq.lat.max <- rep(as.logical("TRUE"),length(st))
  }
  if (!is.null(long.range[1])){
    gteq.long.min <- (st$LONG >= long.range[1])
  } else{ # all stations pass
    gteq.long.min <- rep(as.logical("TRUE"),length(st))
  }
  if (!is.null(long.range[2])){
    lteq.long.max <- (st$LONG <= long.range[2])
  }else{ # all stations pass
    lteq.long.max <- rep(as.logical("TRUE"),length(st))
  }
  
  # find stations where the date matches what we are looking for
  if (!is.null(year.range[1])){
    end.gteq.year.min <- (as.numeric(st$END) >= as.numeric(format(year.range[1],"%Y")))
  } else{
    # all stations pass
    end.gteq.year.min <- rep(as.logical("TRUE"),length(st))
  }
  # assume that if the year is NA, we might as well chance it
  end.gteq.year.min[is.na(end.gteq.year.min)] <- TRUE
  if (!is.null(year.range[2])){
    start.lteq.year.max <- (as.numeric(st$BEGIN) <= as.numeric(format(year.range[2],"%Y")))
  } else{ # all stations pass
    start.lteq.year.max <- rep(as.logical("TRUE"),length(st))  
  }  
  start.lteq.year.max[is.na(start.lteq.year.max)] <- TRUE
  
  # filter the list
  station.list <- st[valid.USAF &
                       in.country &
                       start.lteq.year.max & end.gteq.year.min & 
                       gteq.lat.min  & lteq.lat.max & 
                       gteq.long.min & lteq.long.max,]
  
  # create the output
  return(station.list)
}