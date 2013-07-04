
adjustTimeStamp <- function(obs.all,
                            minutes.round.up = 50,
                            minutes.round.down = 10){
  # fix the hour that we associate with data
  obs.all$HR[obs.all$MIN > minutes.round.up] <- obs.all$HR[obs.all$MIN > minutes.round.up] + 1
  # fix the minutes
  obs.all$MIN[obs.all$MIN > minutes.round.up] <- 0
  obs.all$MIN[obs.all$MIN < minutes.round.down] <- 0
  # fix the date
  obs.all$DATE[obs.all$HR == 24] <- obs.all$DATE[obs.all$HR == 24] + 1
  # fix the other bits of time stamp
  obs.all$Y <- format(obs.all$DATE,
                      "%Y",
                      tz = "UTC")
  obs.all$M <- format(obs.all$DATE,
                      "%m",
                      tz = "UTC")  
  obs.all$D <- format(obs.all$DATE,
                      "%d",
                      tz = "UTC")  
  obs.all$YM <- format(obs.all$DATE,
                       "%Y-%m",
                       tz = "UTC")  
}