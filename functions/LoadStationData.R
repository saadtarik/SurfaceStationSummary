
## LOAD DATA FROM EACH STATION ----
LoadStationData <- function(stations.all = stations.all,
                            station.files = station.files,
                            data.dir = getwd,
                            debug.level = 0){
  # identify unique stations
  unique.USAF <- unique(station.files$USAF)
  # find the files belonging to each station
  file.count <- 0
  for (USAF in unique.USAF){
    # look for the files that include this USAF identifier
    files <- list.files(path = file.path(data.dir,
                                         "data",
                                         "csv"),
                        pattern = paste("^",USAF,"",sep=""),
                        full.names = FALSE)
    cat("Station ", USAF,":", files,"\n")
    for (file.id in files){
      # read the files
      obs.in <- read.csv(file = file.path(data.dir,
                                          "data",
                                          "csv",
                                          file.id))
      obs.in <- obs.in[order(obs.in$M,
                             obs.in$D,
                             obs.in$HR), ]
      obs.in$DATE <- as.Date(paste(obs.in$YR, 
                                   obs.in$M, 
                                   obs.in$D, 
                                   sep = "-"),
                             format = "%Y-%m-%d",
                             tz = "UTC")
      # add the year and month
      obs.in$YM <- format(obs.in$DATE,
                           "%Y-%m",
                           tz = "UTC")
      
      # get the meta data from the files
      obs.in$LAT <- stations.all$LAT[stations.all$USAF == USAF]
      obs.in$LONG <- stations.all$LONG[stations.all$USAF == USAF]
      obs.in$ELEV <- stations.all$ELEV[stations.all$USAF == USAF]
      obs.in$NAME <- stations.all$NAME[stations.all$USAF == USAF]
      if (file.count == 0){
        obs.all <- obs.in
      } else {
        obs.all <- rbind(obs.all,obs.in)
      }
      file.count <- file.count +1
    }
  }
  # replace missing values with NA
  obs.all[(obs.all$LAT >= 99.999),"LAT"] = NA
  obs.all[(obs.all$LONG >= 999.999),"LONG"] = NA
  obs.all[(obs.all$ELEV == 9999),"ELEV"] = NA
  obs.all[(obs.all$WIND.DIR >= 999),"WIND.DIR"] = NA
  obs.all[(obs.all$WIND.SPD >= 999),"WIND.SPD"] = NA
  obs.all[(obs.all$TEMP >= 999),"TEMP"] = NA
  obs.all[(obs.all$DEW.POINT >= 999),"DEW.POINT"] = NA
  obs.all[(obs.all$ATM.PRES >= 2000),"ATM.PRES"] = NA
    
  return(obs.all)
}