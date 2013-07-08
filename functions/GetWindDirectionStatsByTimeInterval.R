getDailyWindStatistics <- function(obs.in){
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
  obs.in.ag.check.ToD <- aggregate(cbind(WIND.DIR.CHECK = WIND.DIR,                                        
                                         WIND.SPD.CHECK = WIND.SPD) ~ ID + DATE + ToD,
                                   data = obs.in,            
                                   function(x) sum( !is.na(x) ) > 1)
  # now check to see if each date has 4 pieces of data
  obs.in.ag.check.Date <- aggregate(cbind(WIND.DIR.CHECK = WIND.DIR.CHECK,
                                          WIND.SPD.CHECK = WIND.SPD.CHECK) ~ ID + DATE,
                                    data = obs.in.ag.check.ToD,            
                                    function(x) sum(x) == 4)
  obs.in.ag.check.Date$WIND.DIR.SPD.CHECK <- obs.in.ag.check.Date$WIND.SPD.CHECK &
    obs.in.ag.check.Date$WIND.DIR.CHECK
  
  # get the components
  obs.in <- cbind(obs.in,
                  GetWindComponentsFromWindSpeedDirection(speed = obs.in$WIND.SPD,
                                                          dir = obs.in$WIND.DIR))
  # get the statistics associated with that period  
  obs.in.ag.stat <- aggregate(cbind(v_e = v_e,
                                    v_n = v_n)~ ID + DATE,
                              data = obs.in,            
                              FUN = mean, na.rm = TRUE)
  # now get the wind direction
  temp <- GetWindSpeedDirectionFromComponents(data = obs.in.ag.stat)
  obs.in.ag.stat <- cbind(obs.in.ag.stat,
                          "WIND.DIR" = temp$dir,
                          "WIND.SPD" = temp$speed)
  
  # finally apply our TRUE / FALSE to them
  # create the output dataframe
  obs.out <- data.frame("ID" = obs.in.ag.stat$ID,
                        "DATE" = obs.in.ag.stat$DATE,
                        "WIND.DIR" = NA,
                        "WIND.SPD" = NA)
  obs.out$WIND.SPD[obs.in.ag.check.Date$WIND.DIR.SPD.CHECK] <- obs.in.ag.stat$WIND.SPD[obs.in.ag.check.Date$WIND.DIR.SPD.CHECK]
  obs.out$WIND.DIR[obs.in.ag.check.Date$WIND.DIR.SPD.CHECK] <- obs.in.ag.stat$WIND.DIR[obs.in.ag.check.Date$WIND.DIR.SPD.CHECK]
  
  return(obs.out)
}