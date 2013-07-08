getClimatologies <- function(obs,
                             stations,
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
  names(monthly.mean.mean)[3:5] <- paste(names(monthly.mean.mean)[3:5],
                                         "mean.mean",
                                         sep = ".")
  print(as.matrix(monthly.mean.mean[1:3,]),quote=F) 
  
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
  
  cat('... merging climatologies\n')    
  monthy.ranges <- merge(merge(merge(monthly.mean.min,
                                     monthly.min.min,
                                     by= c("ID","B"),
                                     suffixes = c(".mean.min",".min.min")),
                               merge(monthly.max.max,
                                     monthly.mean.max,
                                     by= c("ID","B"),
                                     suffixes = c(".max.max",".mean.max")),
                               by = c("ID","B")),
                         monthly.mean.mean,
                         by = c("ID","B"))
  print(as.matrix(monthy.ranges[1:3,]),quote=F) 
  
  cat('...station information\n')
  print(as.matrix(stations[1:3,]),quote=F) 
  
  cat('...combining climatologies with station information')  
  climatology <- merge(stations,
                       monthy.ranges,
                       by = "ID")
  return(climatology)
}

plotClimatologies <- function(climatology,
                              param = "TEMP"){
  switch(param,
         TEMP = plotTEMPClimatology(climatology),
         WIND.SPD = plotWINDSPDClimatology(climatology))
}

plotTEMPClimatology <- function(climatology){
  cat("...plotting temperature climatology.\n")
  is.single.station <- (NROW(unique(climatology$ID)) == 1)
  plot.climatology <- ggplot(data = climatology,
                             aes(x = B,
                                 ymin = `TEMP.min.min`,
                                 lower = `TEMP.mean.min`,
                                 middle = `TEMP.mean.mean`,
                                 upper = `TEMP.mean.max`,
                                 ymax = `TEMP.max.max`)) +
    geom_boxplot(stat = "identity")
  if (is.single.station){
    plot.climatology <- plot.climatology + 
      annotate("text",
               label = as.character(climatology$NOTE[1]),
               x = 12,
               y = -40,
               size = 1.5,
               hjust = 1,
               vjust = 0)
  } else{
    plot.climatology <-plot.climatology + facet_wrap(~ID)
  }
  plot.climatology <- plot.climatology +
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
  is.single.station <- (NROW(unique(climatology$ID)) == 1)  
  plot.climatology <- ggplot(data = climatology,
                             aes(x = B,
                                 ymin = `WIND.SPD.min.min`,
                                 lower = `WIND.SPD.mean.min`,
                                 middle = `WIND.SPD.mean.mean`,
                                 upper = `WIND.SPD.mean.max`,
                                 ymax = `WIND.SPD.max.max`)) +
    geom_boxplot(stat = "identity")
  if (is.single.station){
    plot.climatology <- plot.climatology +
      annotate("text",
               label = as.character(climatology$NOTE[1]),
               x = 12,
               y = 40,
               size = 1.5,
               hjust = 1,
               vjust = 1)
  } else{
    plot.climatology <-plot.climatology + facet_wrap(~ID)
  }
  plot.climatology <-plot.climatology +
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