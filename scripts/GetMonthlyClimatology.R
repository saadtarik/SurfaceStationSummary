## GET DAILY DATA FROM EACH STATION ----

# generate daily data
climate.monthly <- vector("list", length(station.highrate))
dates <- seq(1,12)
n.years = as.numeric(format(target.date.max,"%Y")) -
  as.numeric(format(target.date.min,"%Y"))+1

# get the data, station-by-station (brute force it)
for (i in 1:length(station.highrate)){
  cat("Getting monthly climate data for station ",station.highrate[i],
      " (#",i," of ",length(station.highrate),")",
      sep="")
  # prepare the list
  climate.monthly[[i]]$M <- dates
  climate.monthly[[i]]$TEMP.MEAN <- 
    climate.monthly[[i]]$TEMP.MAX <-
    climate.monthly[[i]]$TEMP.MIN <- 
    climate.monthly[[i]]$WIND.SPD.MEAN <- 
    climate.monthly[[i]]$WIND.SPD.MAX <- 
    climate.monthly[[i]]$WIND.SPD.MIN <- 
    climate.monthly[[i]]$USAFID <- 
    climate.monthly[[i]]$WBAN <-    
    climate.monthly[[i]]$NAME <-    
    climate.monthly[[i]]$LAT <- 
    climate.monthly[[i]]$LONG <- rep(NA,length(dates))
  climate.monthly[[i]] <- as.data.frame(climate.monthly[[i]])
  
  # get the data for this station
  st <- obs.monthly[[i]]
  # get the year, month and day
  st$YR <- as.numeric(format(st$DATE, "%Y"))
  st$M <- as.numeric(format(st$DATE, "%m"))
  st$D <- as.numeric(format(st$DATE, "%d"))  
  # get the lat, long and elev
  st$LAT <- Mode(st$LAT)
  st$LONG <- Mode(st$LONG)
  st$ELEV <- Mode(st$ELEV)
  
  # create outputs
  climate.monthly[[i]]$USAFID <- rep(st$USAFID[1], length(dates))
  climate.monthly[[i]]$WBAN <- rep(st$WBAN[1], length(dates))
  climate.monthly[[i]]$NAME <- rep(st$NAME[1], length(dates))
  climate.monthly[[i]]$LAT <- rep(st$LAT[1], length(dates))
  climate.monthly[[i]]$LONG <- rep(st$LONG[1], length(dates))
  climate.monthly[[i]]$ELEV <- rep(st$ELEV[1], length(dates))
  
  # get values for each month
  for (j in 1:length(dates)){
    # this month's data 
    sub.st <- st[st$M == dates[j], ]
    # require data at least half of the years in the sample    
    pass.data.temp <- length(unique(sub.st$YR[!is.na(sub.st$TEMP.MEAN)])) >= n.years/2    
    pass.data.wind.spd <- length(unique(sub.st$YR[!is.na(sub.st$WIND.SPD.MEAN)])) >= n.years/2
    
    if (pass.data.temp) {      
      # temperature
      climate.monthly[[i]]$TEMP.MEAN[(climate.monthly[[i]]$M ==
                                     dates[j])] <- mean(sub.st$TEMP.MEAN,
                                                        na.rm = TRUE)
      climate.monthly[[i]]$TEMP.MAX[(climate.monthly[[i]]$M ==
                                    dates[j])] <- mean(sub.st$TEMP.MAX,
                                                       na.rm = TRUE)
      climate.monthly[[i]]$TEMP.MIN[(climate.monthly[[i]]$M ==
                                    dates[j])] <- mean(sub.st$TEMP.MIN,
                                                       na.rm = TRUE)      
    }
    if (pass.data.wind.spd) {   
      # wind speed
      climate.monthly[[i]]$WIND.SPD.MEAN[climate.monthly[[i]]$M ==
                                        dates[j]] <- mean(sub.st$WIND.SPD.MEAN,
                                                          na.rm = TRUE)
      climate.monthly[[i]]$WIND.SPD.MAX[climate.monthly[[i]]$M ==
                                       dates[j]] <- mean(sub.st$WIND.SPD.MAX,
                                                         na.rm = TRUE)
      climate.monthly[[i]]$WIND.SPD.MIN[climate.monthly[[i]]$M ==
                                       dates[j]] <- mean(sub.st$WIND.SPD.MIN,
                                                         na.rm = TRUE)
    }    
  }
  cat("...done\n")
}
