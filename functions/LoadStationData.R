
## LOAD DATA FROM EACH STATION ----
LoadStationData <- function(station.files = station.files,
                            data.dir = getwd,
                            debug.level = 0){  
  # find the files that we identified
  file.count <- 0
  for (rowi in 1:NROW(station.files)){
    cat(paste("Loading file ", station.files$Filename[rowi],
              " (", rowi, " of ", NROW(station.files) ,").\n",
              sep = ""))
    # look for the files that include this USAF identifier
    file.id <- list.files(path = file.path(data.dir,
                                           "data",
                                           "csv"),
                          pattern = paste("^",station.files$Filename[rowi],"",sep=""),
                          full.names = FALSE)
    # read the files
    obs.in <- read.csv(file = file.path(data.dir,
                                        "data",
                                        "csv",
                                        file.id[1]))
    obs.in <- obs.in[order(obs.in$M,
                           obs.in$D,
                           obs.in$HR), ]
    obs.in$DATE <- as.Date(paste(obs.in$YR, 
                                 obs.in$M, 
                                 obs.in$D, 
                                 sep = "-"),
                           format = "%Y-%m-%d",
                           tz = "UTC")
    # add the year and month
    obs.in$YM <- format(obs.in$DATE,
                        "%Y-%m",
                        tz = "UTC")
    
    if (file.count == 0){
      obs.all <- obs.in
    } else {
      obs.all <- rbind(obs.all,obs.in)
    }
    file.count <- file.count +1
  }
  # now have all of the data in one data frame
  # replace missing values with NA
  obs.all[(obs.all$WIND.DIR >= 999),"WIND.DIR"] = NA
  obs.all[(obs.all$WIND.SPD >= 999),"WIND.SPD"] = NA
  obs.all[(obs.all$TEMP >= 999),"TEMP"] = NA
  obs.all[(obs.all$DEW.POINT >= 999),"DEW.POINT"] = NA
  obs.all[(obs.all$ATM.PRES >= 2000),"ATM.PRES"] = NA
  
  return(obs.all)
}