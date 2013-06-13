## GET DAILY DATA FROM EACH STATION ----

# generate daily data
obs.monthly <- vector("list", length(station.highrate))
dates <- seq(as.Date(target.date.min),
             as.Date(target.date.max),
             by="mon")

# get the data, station-by-station (brute force it)
for (i in 1:length(station.highrate)){
  cat("Getting monthly data for station ",station.highrate[i],
      " (#",i," of ",length(station.highrate),")",
      sep="")
  # prepare the list
  obs.monthly[[i]]$DATE <- dates
  obs.monthly[[i]]$TEMP.MEAN <- 
    obs.monthly[[i]]$TEMP.MAX <-
    obs.monthly[[i]]$TEMP.MIN <- 
    obs.monthly[[i]]$WIND.SPD.MEAN <- 
    obs.monthly[[i]]$WIND.SPD.MAX <- 
    obs.monthly[[i]]$WIND.SPD.MIN <- 
    obs.monthly[[i]]$USAFID <- 
    obs.monthly[[i]]$WBAN <- 
    obs.monthly[[i]]$NAME <- 
    obs.monthly[[i]]$LAT <- 
    obs.monthly[[i]]$LONG <- rep(NA,length(dates))
  obs.monthly[[i]] <- as.data.frame(obs.monthly[[i]])
  
  # get the data for this station
  st <- obs.daily[[i]]
  # get the year, month and day
  st$YR <- as.numeric(format(st$DATE, "%Y"))
  st$M <- as.numeric(format(st$DATE, "%m"))
  st$D <- as.numeric(format(st$DATE, "%d"))
  st$MON <- as.Date(paste(st$YR,"-",st$M,"-01",sep=""))
  # get the lat, long and elev
  st$LAT <- Mode(st$LAT)
  st$LONG <- Mode(st$LONG)
  st$ELEV <- Mode(st$ELEV)
  
  # create outputs
  obs.monthly[[i]]$USAFID <- rep(st$USAFID[1], length(dates))
  obs.monthly[[i]]$WBAN <- rep(st$WBAN[1], length(dates))
  obs.monthly[[i]]$NAME <- rep(st$NAME[1], length(dates))
  obs.monthly[[i]]$LAT <- rep(st$LAT[1], length(dates))
  obs.monthly[[i]]$LONG <- rep(st$LONG[1], length(dates))
  obs.monthly[[i]]$ELEV <- rep(st$ELEV[1], length(dates))
  
  # get values for each day
  for (j in 1:length(dates)){
    # this month's data 
    sub.st <- st[st$MON == dates[j], ]
    # require data at least 10 days of the month to get monthly values
    pass.data.temp <- length(unique(sub.st$D[!is.na(sub.st$TEMP.MEAN)])) > 10    
    pass.data.wind.spd <- length(unique(sub.st$D[!is.na(sub.st$WIND.SPD.MEAN)])) > 10    
    if (pass.data.temp) {      
      # temperature
      obs.monthly[[i]]$TEMP.MEAN[(obs.monthly[[i]]$DATE ==
                                     dates[j])] <- mean(sub.st$TEMP.MEAN,
                                                        na.rm = TRUE)
      obs.monthly[[i]]$TEMP.MAX[(obs.monthly[[i]]$DATE ==
                                    dates[j])] <- mean(sub.st$TEMP.MAX,
                                                       na.rm = TRUE)
      obs.monthly[[i]]$TEMP.MIN[(obs.monthly[[i]]$DATE ==
                                    dates[j])] <- mean(sub.st$TEMP.MIN,
                                                       na.rm = TRUE)      
    }
    if (pass.data.wind.spd) {   
      # wind speed
      obs.monthly[[i]]$WIND.SPD.MEAN[obs.monthly[[i]]$DATE ==
                                        dates[j]] <- mean(sub.st$WIND.SPD.MEAN,
                                                          na.rm = TRUE)
      obs.monthly[[i]]$WIND.SPD.MAX[obs.monthly[[i]]$DATE ==
                                       dates[j]] <- mean(sub.st$WIND.SPD.MAX,
                                                         na.rm = TRUE)
      obs.monthly[[i]]$WIND.SPD.MIN[obs.monthly[[i]]$DATE ==
                                       dates[j]] <- mean(sub.st$WIND.SPD.MIN,
                                                         na.rm = TRUE)
    }    
  }
  cat("...done\n")
}

# save the data ----
save(obs.all, obs.daily, obs.monthly, station.highrate, 
     file = file.path(data.dir,"data","Observations.R"))
