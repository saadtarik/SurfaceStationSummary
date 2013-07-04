
# define a function to tell me more about wget errors
wget.errors <- function(errorCode) {  
  ## 'errorCode' is the value returned by wget  
  desc <- ifelse(errorCode==0, "No problems occurred",
                 ifelse(errorCode==1, "Generic error code.",
                        ifelse(errorCode==2, "Parse error",
                               ifelse(errorCode==3, "File I/O error",
                                      ifelse(errorCode==4, "Network failure",
                                             ifelse(errorCode==5, "SSL verification failure",
                                                    ifelse(errorCode==6, "Username/password authentication failure",
                                                           ifelse(errorCode==7, "Protocol error",
                                                                  ifelse(errorCode==8, "Server issued an error response",
                                                                         NA )))))))))
}

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
                          "NAME" = NA,
                          "LAT" = NA,
                          "LONG" = NA,
                          "ELEV" = NA,
                          "YR" = NA,
                          "Filename" = NA)
  
  # download data from each station for each year ----
  # work through the years
  for (y in as.numeric(format(min(year.range),"%Y")):
         as.numeric(format(max(year.range),"%Y"))) {
    # work through the stations
    for (s in 1:dim(station.list)[1]) {
      # get the name of the file we want
      outputs[s, 1] <- paste(sprintf("%06d", station.list$USAF[s]),
                             "-",
                             sprintf("%05d", station.list$WBAN[s]),
                             "-", 
                             y,
                             sep = "")
      
      # get some information about the files
      destination.file.path <- file.path(data.dir,
                                         "data",
                                         "raw")
      destination.file <- outputs[s, 1]
      destination.file.gz <- paste(destination.file,
                                   "gz",
                                   sep=".")
      
      # create the command we will will use
      wget <- paste("wget --directory-prefix ",
                    destination.file.path,
                    " ftp://ftp3.ncdc.noaa.gov/pub/data/noaa/",
                    y, "/", destination.file.gz,
                    sep = "")
      
      # tell the user something
      cat("\nLooking for file ", destination.file, "\n")      
      if (debug.level > 0) {
        cat("wget command: ", wget, "\n")
      }
      
      # use a loop to try a few times
      # start a counter to see how many times we tried
      rcount = 0
      repeat{
        # check to see if the zipped file exists,
        # or if the unzipped file is there
        destination.file.exist <- any(file.exists(file.path(destination.file.path,
                                                        destination.file),
                                                  file.path(destination.file.path,
                                                        destination.file.gz)))
        
        if(!destination.file.exist){
          cat("... download attempt ", rcount <- rcount+1, "")
          # now try to actually get the file, returning the error
          outputs[s, 2] <- try(system(wget, 
                                      intern = FALSE,
                                      ignore.stderr = TRUE))   
          cat(wget.errors(outputs[s, 2]))
          if (any(outputs[s,2],c(2,5,6,7))){          
            break
          } else {
            # and append the data to the output
            file.list <- rbind(file.list,
                               cbind("USAF" = station.list$USAF[s],
                                     "WBAN" = station.list$WBAN[s],
                                     "NAME" = station.list$NAME[s],
                                     "LAT" = station.list$LAT[s],
                                     "LONG" = station.list$LONG[s],
                                     "ELEV" = station.list$ELEV[s],
                                     "YR" = y,
                                     "Filename" = destination.file))
          }
        } else {
          cat("file already available locally :)")
          # and append the data to the output
          file.list <- rbind(file.list,
                             cbind("USAF" = station.list$USAF[s],
                             "WBAN" = station.list$WBAN[s],
                             "NAME" = station.list$NAME[s],
                             "LAT" = station.list$LAT[s],
                             "LONG" = station.list$LONG[s],
                             "ELEV" = station.list$ELEV[s],
                             "YR" = y,
                             "Filename" = destination.file))
          break          
        }      
        if (rcount >5){
          break
        }
      }
    }
  }
  return(file.list[-1,])
}


