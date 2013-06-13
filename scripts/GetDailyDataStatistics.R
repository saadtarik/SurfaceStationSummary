## GET DAILY DATA FROM EACH STATION ----

## generate daily data ----
obs.daily <- vector("list", length(station.highrate))
dates <- seq(as.Date(target.date.min),
             as.Date(target.date.max),
             1)
duration.days = as.Date(target.date.max) -
  as.Date(target.date.min) +1
# get the data, station-by-station (brute force it)
for (i in 1:length(station.highrate)){
  cat("Getting daily data for station ",station.highrate[i],
      " (#",i," of ",length(station.highrate),")",
      sep="")
  # prepare the list
  obs.daily[[i]]$DATE <- dates
  obs.daily[[i]]$TEMP.MEAN <- 
    obs.daily[[i]]$TEMP.MAX <-
    obs.daily[[i]]$TEMP.MIN <- 
    obs.daily[[i]]$WIND.SPD.MEAN <- 
    obs.daily[[i]]$WIND.SPD.MAX <- 
    obs.daily[[i]]$WIND.SPD.MIN <- 
    obs.daily[[i]]$USAFID <- 
    obs.daily[[i]]$WBAN <-  
    obs.daily[[i]]$NAME <-  
    obs.daily[[i]]$LAT <- 
    obs.daily[[i]]$LONG <- rep(NA,length(dates))
  obs.daily[[i]] <- as.data.frame(obs.daily[[i]])
  
  # get the data for this station
  st <- obs.all[obs.all$USAFID==station.highrate[i],]
  # adjust the data points where the time is within 10 minutes of 0 minutes
  st$HR[st$MIN > 50] <- st$HR[st$MIN > 50] + 1
  st$MIN[st$MIN > 50] <- 0
  st$MIN[st$MIN < 10] <- 0
  st$DATE[st$MIN == 24] <- st$DATE[st$MIN ==24] + 1 
  st$YR <- as.numeric(format(st$DATE, "%Y"))
  st$M <- as.numeric(format(st$DATE, "%m"))
  st$D <- as.numeric(format(st$DATE, "%d"))
  # get the lat, long and elev
  st$LAT <- Mode(st$LAT)
  st$LONG <- Mode(st$LONG)
  st$ELEV <- Mode(st$ELEV)
  # only take data where the data are on the hour
  st <- st[st$MIN == 0, ]
  
  # create outputs
  obs.daily[[i]]$USAFID <- rep(st$USAFID[1], length(dates))
  obs.daily[[i]]$WBAN <- rep(st$WBAN[1], length(dates))
  obs.daily[[i]]$NAME <- rep(st$NAME[1], length(dates))
  obs.daily[[i]]$LAT <- rep(st$LAT[1], length(dates))
  obs.daily[[i]]$LONG <- rep(st$LONG[1], length(dates))
  obs.daily[[i]]$ELEV <- rep(st$ELEV[1], length(dates))
  
  # get values for each day
  for (j in 1:length(dates)){
    # this day's data 
    sub.st <- st[st$DATE == dates[j], ]
    # require data at least every 6 hours
    hours <- c(0,6,12,18)
    tempcount <-rep(FALSE,4)
    spdcount <-rep(FALSE,4)
    for (houri in 1:length(hours)){
      tempcount[houri] <- any(!is.na(sub.st$TEMP[(sub.st$HR >= hours[houri]) &
                                                   (sub.st$HR < (hours[houri]+6))]))
      spdcount[houri] <- any(!is.na(sub.st$WIND.SPD[(sub.st$HR >= hours[houri]) &
                                                      (sub.st$HR < (hours[houri]+6))]))
    }    
    
    pass.data.temp <- all(tempcount)
    pass.data.wind.spd <- all(spdcount)
    if (pass.data.temp) {      
      # temperature
      obs.daily[[i]]$TEMP.MEAN[(obs.daily[[i]]$DATE ==
                                   dates[j])] <- mean(sub.st$TEMP, na.rm = TRUE)
      obs.daily[[i]]$TEMP.MAX[(obs.daily[[i]]$DATE ==
                                  dates[j])] <- max(sub.st$TEMP, na.rm = TRUE)
      obs.daily[[i]]$TEMP.MIN[(obs.daily[[i]]$DATE ==
                                  dates[j])] <- min(sub.st$TEMP, na.rm = TRUE)      
    }
    if (pass.data.wind.spd) {   
      # wind speed
      obs.daily[[i]]$WIND.SPD.MEAN[obs.daily[[i]]$DATE ==
                                      dates[j]] <- mean(sub.st$WIND.SPD, na.rm = TRUE)
      obs.daily[[i]]$WIND.SPD.MAX[obs.daily[[i]]$DATE ==
                                     dates[j]] <- max(sub.st$WIND.SPD, na.rm = TRUE)
      obs.daily[[i]]$WIND.SPD.MIN[obs.daily[[i]]$DATE ==
                                     dates[j]] <- min(sub.st$WIND.SPD, na.rm = TRUE)
    }    
  }
  cat("...done\n")
}

# save the data ----
save(obs.all, obs.daily, station.highrate, 
     file = file.path(data.dir,"data","Observations.R"))
