getClimatologies <- function(obs,
                             year.min.count = 1){
  cat('...getting min/min\n')
  monthly.min.min <- getCalendarMonthStatistics(getDailyStatistics(obs,
                                                                   stat = "min"), 
                                                stat = "min",
                                                year.min.count)
  print(as.matrix(monthly.min.min[1:3,]),quote=F)   
  
  cat('...getting mean/min\n')
  monthly.mean.min <- getCalendarMonthStatistics(getDailyStatistics(obs,
                                                                    stat = "min"), 
                                                 stat = "mean",
                                                 year.min.count)  
  print(as.matrix(monthly.mean.min[1:3,]),quote=F)   
  
  cat('...getting mean/mean\n')
  monthly.mean.mean <- getCalendarMonthStatistics(getDailyStatistics(obs,
                                                                     stat = "mean"), 
                                                  stat = "mean",
                                                  year.min.count)  
  print(as.matrix(monthly.mean.mean[1:3,]),quote=F) 
  names(monthly.mean.mean)[2:4] <- paste(names(monthly.mean.mean)[2:4],
                                         "mean.mean",
                                         sep = ".")
  
  cat('...getting mean/max\n')
  monthly.mean.max <- getCalendarMonthStatistics(getDailyStatistics(obs,
                                                                    stat = "max"), 
                                                 stat = "mean",
                                                 year.min.count)
  print(as.matrix(monthly.mean.max[1:3,]),quote=F) 
  
  cat('...getting max/max\n')  
  monthly.max.max <- getCalendarMonthStatistics(getDailyStatistics(obs,
                                                                   stat = "max"), 
                                                stat = "max",
                                                year.min.count)
  print(as.matrix(monthly.max.max[1:3,]),quote=F) 
  
  cat('... merging climatologies, removing infinite values\n')    
  monthy.ranges <- merge(merge(merge(monthly.mean.min,
                                     monthly.min.min,
                                     by= c("B"),
                                     suffixes = c(".mean.min",".min.min")),
                               merge(monthly.max.max,
                                     monthly.mean.max,
                                     by= c("B"),
                                     suffixes = c(".max.max",".mean.max")),
                               by = c("B")),
                         monthly.mean.mean,
                         by = c("B"))
  
  # fix infinites
  is.na(monthy.ranges) <- do.call(cbind,lapply(monthy.ranges, is.infinite))
  # look!
  print(as.matrix(monthy.ranges[1:3,]),quote=F) 
  return(monthy.ranges)
}

plotClimatologies <- function(climatology,                              
                              param = "TEMP"){
  switch(param,
         TEMP = plotTEMPClimatology(climatology),
         WIND.SPD = plotWINDSPDClimatology(climatology))
}

plotTEMPClimatology <- function(climatology){
  cat("...plotting temperature climatology.\n")  
  plot.climatology <- ggplot(data = climatology,
                             aes(x = B,
                                 ymin = `TEMP.min.min`,
                                 lower = `TEMP.mean.min`,
                                 middle = `TEMP.mean.mean`,
                                 upper = `TEMP.mean.max`,
                                 ymax = `TEMP.max.max`)) +
    geom_boxplot(stat = "identity")  +
    scale_x_discrete("Month",
                     labels=substr(factor(seq(1,12,1),
                                          labels = format(as.Date(paste("2012",1:12,"01",sep = "-")),
                                                          "%B",
                                                          tz = "GMT"),
                                          ordered = TRUE),
                                   1,
                                   1),
                     drop = FALSE) + 
    scale_y_continuous("Temperature (C)",
                       limits = c(-40,40))    
  print(plot.climatology)  
  return(plot.climatology)    
}

plotWINDSPDClimatology <- function(climatology){
  cat("...plotting wind speed climatology.\n")
  plot.climatology <- ggplot(data = climatology,
                             aes(x = B,
                                 ymin = `WIND.SPD.min.min`,
                                 lower = `WIND.SPD.mean.min`,
                                 middle = `WIND.SPD.mean.mean`,
                                 upper = `WIND.SPD.mean.max`,
                                 ymax = `WIND.SPD.max.max`)) +
    geom_boxplot(stat = "identity") +
    scale_x_discrete(name = "Month",
                     labels = substr(factor(seq(1,12,1),
                                            labels = format(as.Date(paste("2012",1:12,"01",sep = "-")),
                                                            "%B",
                                                            tz = "GMT"),
                                            ordered = TRUE),
                                     1,
                                     1),
                     drop = FALSE) + 
    scale_y_continuous("Wind speed (m/s)",
                       limits = c(0,40))    
  print(plot.climatology)  
  return(plot.climatology)
}