
## LOAD DATA FROM EACH STATION ----
# identify unique stations
unique.USAFID <- unique(station.files$USAFID)
# find the files belonging to each station
file.count <- 0
for (USAFID in unique.USAFID){
  files <- list.files("data/csv",
                      pattern = paste("^",USAFID,"",sep=""))
  cat("Station ", USAFID,":", files,"\n")
  for (file.id in files){
    # read the files
    obs.in <- read.csv(file = paste("data/csv/", file.id,sep = ""))
    obs.in <- obs.in[order(obs.in$M, obs.in$D, obs.in$HR), ]
    obs.in$DATE <- as.Date(paste(obs.in$YR, 
                                     obs.in$M, 
                                     obs.in$D, 
                                     sep = "-"),
                               format = "%Y-%m-%d")
    # get the meta data from the files
    obs.in$LAT <- rep(target.list$LAT[target.list$USAF == USAFID],
                          length(obs.in$DATE))
    obs.in$LONG <- rep(target.list$LON[target.list$USAF == USAFID],
                           length(obs.in$DATE))
    obs.in$ELEV <- rep(target.list$LON[target.list$USAF == USAFID],
                           length(obs.in$DATE))
    obs.in$NAME <- rep(target.list$NAME[target.list$USAF == USAFID],
                           length(obs.in$DATE))
    if (file.count == 0){
      obs.all <- obs.in
    } else {
      obs.all <- rbind(obs.all,obs.in)
    }
    file.count <- file.count +1
  }
}
# replace missing values with NA
obs.all[(obs.all$LAT >= 99.999),"LAT"] = NA
obs.all[(obs.all$LONG >= 999.999),"LONG"] = NA
obs.all[(obs.all$ELEV == 9999),"ELEV"] = NA
obs.all[(obs.all$WIND.DIR >= 999),"WIND.DIR"] = NA
obs.all[(obs.all$WIND.SPD >= 999),"WIND.SPD"] = NA
obs.all[(obs.all$TEMP >= 999),"TEMP"] = NA
obs.all[(obs.all$DEW.POINT >= 999),"DEW.POINT"] = NA
obs.all[(obs.all$ATM.PRES >= 2000),"ATM.PRES"] = NA

# add the year and month
obs.all$YM <- format(obs.all$DATE,"%Y-%m")

# tidy up
rm(obs.in,file.count)
