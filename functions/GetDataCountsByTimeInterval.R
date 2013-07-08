GetDataCountsByTimeInterval <- function(obs.in,
                                        interval = "overall"){  
  switch(interval,
         overall = GetDataCountsOverall(obs.in),
         year = GetDataCountsByYear(obs.in),
         month = GetDataCountsByMonth(obs.in),
         yearmonth = GetDataCountsByYearMonth(obs.in),
         hour = GetDataCountsByHour(obs.in))
}


# get the total count of non-NA obs.in at each station, overall
GetDataCountsOverall <- function(obs.in){
  data.counts <- data.frame("WIND.DIR.COUNT" = sum( !is.na(obs.in$WIND.DIR)))
  data.counts$WIND.SPD.COUNT = sum( !is.na(obs.in$WIND.SPD))
  data.counts$TEMP.COUNT = sum( !is.na(obs.in$TEMP))
  data.counts$ATM.PRES.COUNT = sum( !is.na(obs.in$ATM.PRES))  
  return(data.counts)
}

# get the total count of non-NA data at each station, each year
GetDataCountsByYear <- function(obs.in){  
  data.counts <- aggregate(cbind(WIND.DIR.COUNT = WIND.DIR,
                                 WIND.SPD.COUNT = WIND.SPD,
                                 TEMP.COUNT = TEMP,
                                 ATM.PRES.COUNT = ATM.PRES) ~ YR,                           
                           data=obs.in,
                           na.action = na.pass,
                           function(x) sum( !is.na(x) ))
  return(data.counts)
}

# get the total count of non-NA data at each station, each month
GetDataCountsByMonth <- function(obs.in){
  data.counts <- aggregate(cbind(WIND.DIR.COUNT = WIND.DIR,
                                 WIND.SPD.COUNT = WIND.SPD,
                                 TEMP.COUNT = TEMP,
                                 ATM.PRES.COUNT = ATM.PRES) ~ M,
                           data=obs.in,                           
                           na.action = na.pass,
                           function(x) sum( !is.na(x) ))
  return(data.counts)}

# get the total count of non-NA data at each station, each month of each year
GetDataCountsByYearMonth <- function(obs.in){
  data.counts <- aggregate(cbind(WIND.DIR.COUNT = WIND.DIR,
                                 WIND.SPD.COUNT = WIND.SPD,
                                 TEMP.COUNT = TEMP,
                                 ATM.PRES.COUNT = ATM.PRES) ~ YR + M,
                           data=obs.in,
                           na.action = na.pass,
                           function(x) sum( !is.na(x) ))
  return(data.counts)}

# get the total count of non-NA data at each station, each hour
GetDataCountsByHour <- function(obs.in){
  data.counts <- aggregate(cbind(WIND.DIR.COUNT = WIND.DIR,
                                 WIND.SPD.COUNT = WIND.SPD,
                                 TEMP.COUNT = TEMP,
                                 ATM.PRES.COUNT = ATM.PRES) ~ HR,
                           data=obs.in,
                           na.action = na.pass,
                           function(x) sum( !is.na(x) ))
  return(data.counts)}
