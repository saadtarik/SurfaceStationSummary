
ConvertStationDataFiles <- function(station.files = NULL,                                    
                                    data.dir = getwd,
                                    debug.level = 0){
  ## IMPORT NOAA DATA FILES ----
  # data directory
  source.file.directory <- file.path(data.dir,"data","raw")
  destination.file.directory <- file.path(data.dir,"data","csv")
  
  # unzip all of the files
  system(paste("gunzip -r",source.file.directory),
         intern = FALSE,
         ignore.stderr = TRUE)
  
  # define a data frame to put information in
  column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6,
                     7,5,5,5,4,3,1,1,4, 1,5,1,1,1,6,
                     1,1,1,5,1,5,1,5,1)  
  
  # read the files in
  for (fi in 1:length(station.files$Filename)) {
    # read the original data file
    data <- read.fwf(file.path(source.file.directory,
                               station.files$Filename[fi]),
                     column.widths)
    # extract specific columns
    data <- data[,c(2:8,10:11,13,16,19,29,31, 33)]
    # give them names
    names(data) <- c("USAF",
                     "WBAN",
                     "YR",
                     "M",
                     "D",
                     "HR",
                     "MIN",
                     "LAT",
                     "LONG",
                     "ELEV",
                     "WIND.DIR",
                     "WIND.SPD",
                     "TEMP",
                     "DEW.POINT",
                     "ATM.PRES")
    data$LAT <- data$LAT/1000
    data$LONG <- data$LONG/1000
    data$WIND.SPD <- data$WIND.SPD/10
    data$TEMP <- data$TEMP/10
    data$DEW.POINT <- data$DEW.POINT/10
    data$ATM.PRES <- data$ATM.PRES/10
    
    # write this data to file
    write.csv(data,
              file=file.path(destination.file.directory,
                             paste(station.files$Filename[fi],".csv", sep = "")),
              row.names = FALSE)
  }
    
  return(station.files)
}