## REQUEST STATION DATA ----
RequestStationDataFiles <- function(station.list = NULL,
                                    year.range = c(NULL,NULL),
                                    data.dir = getwd,
                                    debug.level = 0){  
  # initialize a data frame to keep track
  outputs <- as.data.frame(matrix(NA,
                                  dim(station.list)[1],
                                  2))
  names(outputs) <- c("FILE", "STATUS")
  # create a flat data file as output
  file.list <- data.frame("USAF" = NA,
                          "WBAN" = NA,
                          "ID" = NA,
                          "NAME" = NA,
                          "LAT" = NA,
                          "LONG" = NA,
                          "ELEV" = NA,
                          "YR" = NA,
                          "Filename" = NA)
  
  # directory we will send our files to
  destination.file.path <- file.path(data.dir,
                                     "data",
                                     "raw")
  
  # download data from each station for each year ----
  NOAA.FTP <- "ftp://ftp3.ncdc.noaa.gov/pub/data/noaa"
  NOAA.con = getCurlHandle(ftp.use.epsv = FALSE,
                           maxconnects=1,
                           fresh.connect=0,
                           timeout = 60,
                           useragent = "R")
  # work through the years
  for (y in seq(as.numeric(format(min(year.range),"%Y")),
                as.numeric(format(max(year.range),"%Y")))) {
    # get some information about the files
    source.file.path <- paste(NOAA.FTP,"/",y,"/",sep="")
    cat("\nLooking in directory ", source.file.path, "\n")      
    
    # get a file listing
    source.listing <- unlist(strsplit(getURL(source.file.path,
                                             curl = NOAA.con,
                                             verbose=TRUE,
                                             dirlistonly = TRUE),
                                      "\n"))
    cat("...got file listing\n")          
    # work through the stations
    for (s in 1:dim(station.list)[1]) {
      # get the name of the remote file for this station.
      cat("\nLooking for station ", as.character(station.list$USAF[s]), "\n")            
      # This will include .gz in the name
      source.file.gz <- grep(as.character(paste(station.list$USAF[s],
                                                "-",
                                                station.list$WBAN[s],
                                                sep = "")),
                             source.listing,
                             fixed = TRUE,
                             value = TRUE)
      destination.file.gz <- source.file.gz      
      if (length(source.file.gz) != 0){
        # Remove the .gz so that we can search      
        source.file <- sub(".gz","",source.file.gz)
        destination.file <- source.file
        
        # tell the user something
        cat("...downloading file ", source.file.gz, "\n")      
        
        # check to see if the zipped file exists,
        # or if the unzipped file is there
        destination.file.exist <- any(file.exists(file.path(destination.file.path,
                                                            destination.file),
                                                  file.path(destination.file.path,
                                                            destination.file.gz)))
        
        if(!destination.file.exist){        
          # now try to actually get the file, returning the error
          # http://stackoverflow.com/questions/14426359/downloading-large-files-with-r-rcurl-efficiently
          try(download.file(url = paste(source.file.path,
                                        source.file.gz,
                                        sep = ""),
                            destfile = file.path(destination.file.path,
                                                 destination.file.gz)))
          # check and see how well that worked
          download.failed <- TRUE
          if (file.exists(file.path(destination.file.path,
                             destination.file.gz))){
            if (file.info(file.path(destination.file.path,
                                     destination.file.gz))$size > 0){
              # expand the zip
              gunzip(filename = (file.path(destination.file.path,
                                           destination.file.gz)))
              download.failed <- FALSE
            }
          }
          if (file.exists(file.path(destination.file.path,
                                   destination.file))){
            if (file.info(file.path(destination.file.path,
                                    destination.file))$size > 0){
              download.failed <- FALSE
            }
          }
                    
          if(download.failed){
            try(file.remove(file.path(destination.file.path,
                                  destination.file.gz)))
            try(file.remove(file.path(destination.file.path,
                                  destination.file)))
            cat("...download failed, partial files removed.")
            destination.file.exist <- FALSE
          } else{
            cat("...download succesful.")
            destination.file.exist <- TRUE
          }
        }
        
        # check again
        if(destination.file.exist){        
          # append the data to the output
          file.list <- rbind(file.list,
                             cbind("USAF" = station.list$USAF[s],
                                   "WBAN" = station.list$WBAN[s],
                                   "ID" = station.list$ID[s],
                                   "NAME" = station.list$NAME[s],
                                   "LAT" = station.list$LAT[s],
                                   "LONG" = station.list$LONG[s],
                                   "ELEV" = station.list$ELEV[s],
                                   "YR" = y,
                                   "Filename" = destination.file))  
        }      
      } else{
        # tell the user something
        cat("...didn't find a file for station ", station.list$ID[s], "\n")
      }
    }
  }
  cat("Finished fetching files\n")
  # generate a file list
  file.list <- file.list[-1,]
  return(file.list)
}


