

## IMPORT NOAA DATA ----
system("gunzip -r data/raw", intern = FALSE, ignore.stderr = TRUE)
files <- list.files("data/raw")
column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                   7,5,5,5,4,3,1,1,4, 1,5,1,1,1,6,
                   1,1,1,5,1,5,1,5,1)
station.files <- as.data.frame(matrix( NA, length(files),6))
names(station.files) <- c("USAFID", "NAME", "YR", "LAT","LONG", "ELEV")
for (i in 1:length(files)) {
  data <- read.fwf(paste("data/raw/",files[i],
                         sep = ""), column.widths)
  data <- data[,c(2:8,10:11,13,16,19,29,31, 33)]
  names(data) <- c("USAFID","WBAN","YR","M",
                   "D", "HR", "MIN", "LAT", "LONG", "ELEV",
                   "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT",
                   "ATM.PRES")
  data$LAT <- data$LAT/1000
  data$LONG <- data$LONG/1000
  data$WIND.SPD <- data$WIND.SPD/10
  data$TEMP <- data$TEMP/10
  data$DEW.POINT <- data$DEW.POINT/10
  data$ATM.PRES <- data$ATM.PRES/10
  write.csv(data,file=paste("data/csv/",files[i],
                            ".csv", sep = ""), row.names = FALSE)
  station.files$USAFID[i] <- data[1,1]
  station.files$YR[i] <- data[1,3]
  # combine this data with the target list
  station.files$LAT[i] <- target.list$LAT[target.list$USAF == data$USAFID[1]]
  station.files$LONG[i] <- target.list$LON[target.list$USAF == data$USAFID[1]]
  station.files$ELEV[i] <- target.list$ELEV[target.list$USAF == data$USAFID[1]]
  station.files$NAME[i] <- target.list$NAME[target.list$USAF == data$USAFID[1]]
}

# save the station files to file
write.csv(station.files, file = "data/stations.csv", row.names = FALSE)

rm(data)