GetDataCountsByTimeInterval <- function(obs.all,
                          interval = "overall"){
switch(interval,
       overall = GetDataCountsOverall(obs.all),
       year = GetDataCountsByYear(obs.all),
       month = GetDataCountsByMonth(obs.all),
       yearmonth = GetDataCountsByYearMonth(obs.all),
       hour = GetDataCountsByHour(obs.all))
}


# get the total count of non-NA data at each station, overall
GetDataCountsOverall <- function(obs.all){
  data.counts <- aggregate(cbind(WIND.DIR.COUNT = WIND.DIR,
                                    WIND.SPD.COUNT = WIND.SPD,
                                    TEMP.COUNT = TEMP,
                                    ATM.PRES.COUNT = ATM.PRES) ~ USAF,
                              data=obs.all,
                              function(x) sum( !is.na(x) ))
  return(data.counts)}

# get the total count of non-NA data at each station, each year
GetDataCountsByYear <- function(obs.all){
  data.counts <- aggregate(cbind(WIND.DIR.COUNT = WIND.DIR,
                                   WIND.SPD.COUNT = WIND.SPD,
                                   TEMP.COUNT = TEMP,
                                   ATM.PRES.COUNT = ATM.PRES) ~ USAF + YR,
                             data=obs.all,
                             function(x) sum( !is.na(x) ))
  return(data.counts)}

# get the total count of non-NA data at each station, each month
GetDataCountsByMonth <- function(obs.all){
  data.counts <- aggregate(cbind(WIND.DIR.COUNT = WIND.DIR,
                                    WIND.SPD.COUNT = WIND.SPD,
                                    TEMP.COUNT = TEMP,
                                    ATM.PRES.COUNT = ATM.PRES) ~ USAF + M,
                              data=obs.all,
                              function(x) sum( !is.na(x) ))
  return(data.counts)}

# get the total count of non-NA data at each station, each month of each year
GetDataCountsByYearMonth <- function(obs.all){
  data.counts <- aggregate(cbind(WIND.DIR.COUNT = WIND.DIR,
                                  WIND.SPD.COUNT = WIND.SPD,
                                  TEMP.COUNT = TEMP,
                                  ATM.PRES.COUNT = ATM.PRES) ~ USAF + YR + M,
                            data=obs.all,
                            function(x) sum( !is.na(x) ))
  return(data.counts)}

# get the total count of non-NA data at each station, each hour
GetDataCountsByHour <- function(obs.all){
  data.counts <- aggregate(cbind(WIND.DIR.COUNT = WIND.DIR,
                                       WIND.SPD.COUNT = WIND.SPD,
                                       TEMP.COUNT = TEMP,
                                       ATM.PRES.COUNT = ATM.PRES) ~ USAF + HR,
                                 data=obs.all,
                                 function(x) sum( !is.na(x) ))
  return(data.counts)}
