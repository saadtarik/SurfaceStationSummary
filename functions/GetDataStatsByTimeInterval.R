getDailyStatistics <- function(obs.in,
                               stat = "mean"){
  
  # only get daily statistics if we have data at least every 6 hours
  # first divide the day up
  obs.in$ToD <- cut(obs.in$HR,
                    breaks = c(-1,6,12,18,24),
                    labels = paste(seq(0,18,6),
                                   "-",
                                   seq(6,24,6),
                                   ordered_result = TRUE)
  )
  # first check to see if we have enough data throughout the day
  # returning  a TRUE / FALSE value
  obs.in.ag.check.ToD <- aggregate(cbind(WIND.SPD.CHECK = WIND.SPD,
                                         TEMP.CHECK = TEMP,
                                         ATM.PRES.CHECK = ATM.PRES) ~ ID + DATE + ToD,
                                   data = obs.in,            
                                   function(x) sum( !is.na(x) ) > 1)
  # now check to see if each date has 4 pieces of data
  obs.in.ag.check.Date <- aggregate(cbind(WIND.SPD.CHECK = WIND.SPD.CHECK,
                                          TEMP.CHECK = TEMP.CHECK,
                                          ATM.PRES.CHECK = ATM.PRES.CHECK) ~ ID + DATE,
                                    data = obs.in.ag.check.ToD,            
                                    function(x) sum(x) == 4)  
  # get the statistics associated with that period
  obs.in.ag.stat <- aggregate(cbind(WIND.SPD = WIND.SPD,
                                    TEMP = TEMP,
                                    ATM.PRES = ATM.PRES) ~ ID + DATE,
                              data = obs.in,            
                              FUN = stat, na.rm = TRUE)
  # finally apply our TRUE / FALSE to them
  # create the output dataframe
  obs.out <- data.frame("ID" = obs.in.ag.stat$ID,
                        "DATE" = obs.in.ag.stat$DATE,
                        "WIND.SPD" = NA,
                        "TEMP" = NA,
                        "ATM.PRES" = NA)
  obs.out$WIND.SPD[obs.in.ag.check.Date$WIND.SPD.CHECK] <- obs.in.ag.stat$WIND.SPD[obs.in.ag.check.Date$WIND.SPD.CHECK]
  obs.out$TEMP[obs.in.ag.check.Date$TEMP.CHECK] <- obs.in.ag.stat$TEMP[obs.in.ag.check.Date$TEMP.CHECK]
  obs.out$ATM.PRES[obs.in.ag.check.Date$ATM.PRES] <- obs.in.ag.stat$ATM.PRES[obs.in.ag.check.Date$ATM.PRES]
  
  return(obs.out)
}

getCalendarMonthStatistics <- function(obs.in,
                                       stat = "mean",
                                       year.min.count = 1){
  # get statistics for each January, February, etc.
  # work on whatever data are passed in; these could be daily means, daily mins, whatever.
  
  # make sure we have a month and year to work on
  obs.in$Y <- format(obs.in$DATE,
                     "%y",
                     tz = "UTC")
  obs.in$B <- factor(format(obs.in$DATE,
                            "%B",
                            tz = "UTC"),
                     levels = format(as.Date(paste("2012",seq(1,12,1),"02",sep ="-")),
                                     "%B",
                                     tz = "UTC"),
                     ordered = TRUE)
  # how many years of data do we have?
  n.years <- max(as.numeric(obs.in$Y),na.rm = TRUE) - 
    min(as.numeric(obs.in$Y),na.rm = TRUE) + 1
  
  # first check to see if we have enough data, returning  a TRUE / FALSE value
  # each combination of year and month should have at least 15 daily values
  obs.in.ag.check.YM <- aggregate(cbind(WIND.SPD.CHECK = WIND.SPD.CHECK,
                                        TEMP.CHECK = TEMP.CHECK,
                                        ATM.PRES.CHECK = ATM.PRES.CHECK) ~ ID + B,
                                  data = aggregate(cbind(WIND.SPD.CHECK = WIND.SPD,
                                                         TEMP.CHECK = TEMP,
                                                         ATM.PRES.CHECK = ATM.PRES) ~ ID + Y + B,
                                                   data = obs.in,            
                                                   function(x) sum( !is.na(x) ) > 15),            
                                  function(x) sum( !is.na(x) ) > (year.min.count))
  
  # now get the statistic
  obs.in.ag.stat <- aggregate(cbind(WIND.SPD = WIND.SPD,
                                    TEMP = TEMP,
                                    ATM.PRES = ATM.PRES) ~ ID + B,
                              data = obs.in,            
                              FUN = stat, na.rm = TRUE)
  
  # finally apply our TRUE / FALSE to them
  # create the output dataframe
  obs.out <- data.frame("ID" = obs.in.ag.stat$ID,
                        "B" = obs.in.ag.stat$B,
                        "WIND.SPD" = NA,
                        "TEMP" = NA,
                        "ATM.PRES" = NA)
  obs.out$WIND.SPD[obs.in.ag.check.YM$WIND.SPD.CHECK] <- obs.in.ag.stat$WIND.SPD[obs.in.ag.check.YM$WIND.SPD.CHECK]
  obs.out$TEMP[obs.in.ag.check.YM$TEMP.CHECK] <- obs.in.ag.stat$TEMP[obs.in.ag.check.YM$TEMP.CHECK]
  obs.out$ATM.PRES[obs.in.ag.check.YM$ATM.PRES] <- obs.in.ag.stat$ATM.PRES[obs.in.ag.check.YM$ATM.PRES]
  
  return(obs.out)
}