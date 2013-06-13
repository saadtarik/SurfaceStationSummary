
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
outputs <- as.data.frame(matrix(NA, dim(target.list)[1],2))
names(outputs) <- c("FILE", "STATUS")
for (y in as.numeric(format(target.date.min,"%Y")):
       as.numeric(format(target.date.max,"%Y"))) {
  y.target.list <- target.list[target.list$BEGIN <= y & target.list$END >=y, ]
  for (s in 1:dim(y.target.list)[1]) {
    outputs[s, 1] <- paste(sprintf("%06d", y.target.list[s,1]),
                           "-",
                           sprintf("%05d", y.target.list[s,2]),
                           "-", 
                           y,
                           sep = "")
    wget <- paste("wget --directory-prefix data/raw ftp://ftp3.ncdc.noaa.gov/pub/data/noaa/",
                  y, "/", outputs[s, 1],".gz", sep = "")
    # try to get the file
    cat("\nLooking for file ", outputs[s, 1], "\n")
    rcount = 0
    repeat{
      # check to see if the zipped file exists,
      # or if the unzipped file is there
      if((is.na(file.info(paste("data/raw/",outputs[s, 1],".gz",sep=""))$size)) & 
           (is.na(file.info(paste("data/raw/",outputs[s, 1],sep=""))$size))){
        cat("... download attempt ", rcount <- rcount+1, "")
        outputs[s, 2] <- try(system(wget, intern = FALSE,
                                    ignore.stderr = TRUE))   
        cat(wget.errors(outputs[s, 2]))
        if (any(outputs[s,2],c(2,5,6,7))){          
          break
        }
      } else {
        if ((file.info(paste("data/raw/",outputs[s, 1], ".gz",sep=""))$size>0) |
              (file.info(paste("data/raw/",outputs[s, 1],sep=""))$size>0)){
          cat("file already available locally\n")
          break
        }       
      }      
      if (rcount >5){
        break
      }
    }
  }
}
head(outputs)