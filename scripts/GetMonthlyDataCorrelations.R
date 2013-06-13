# get correlations between each site, for each month

# initialise the outputs
varnames <- c("M", "Month",
              "site1.USAFID", "site1.NAME", 
              "site1.LAT", "site1.LONG",
              "site2.USAFID", "site2.NAME", 
              "site2.LAT", "site2.LONG",
              "TEMP.MEAN.r2",
              "WIND.SPD.MEAN.r2")
correlations.monthly <- data.frame(matrix(ncol = length(varnames),
                                          nrow = 12 *
                                            length(station.highrate) 
                                          * (length(station.highrate)-1)))
names(correlations.monthly) <- varnames

# get  vector of month names
Months <- format(as.Date(paste("2000-",seq(1,12,1),"-01",sep="")),"%B")

# get the data, station-by-station for each calendar month (brute force it)
irow <- 0
for (M in 1:12){
  for (s1i in 1:length(station.highrate)){
    # output to screen
    cat("Comparing data from station ",station.highrate[s1i],
        " (#",s1i," of ",length(station.highrate),")...\n",
        sep="")
    
    # get the data for this site, for this month
    s1 <- obs.daily[[s1i]][(as.numeric(format(obs.daily[[s1i]]$DATE,"%m")) == M),]
    
    for (s2i in 1:s1i){
      if (s1i == s2i){
        
      }else{
        # work out which row this will be in the outputs
        irow <- irow +1
        
        # output to screen
        cat("... with data from station ",station.highrate[s2i],
            " (#",s2i," of ",length(station.highrate),",",
            " row ", irow,
            ")\n",
            sep="")      
        
        # get the data for this site, for this month
        s2 <- obs.daily[[s2i]][(as.numeric(format(obs.daily[[s2i]]$DATE,"%m")) == M),]
        
        # now compare the data for the same time stamp
        s1s2 <- merge(s1,s2,by = "DATE")  
        
        # finally get the data
        correlations.monthly$TEMP.MEAN.r2[irow] = summary(lm(s1s2$TEMP.MEAN.x ~ 
                                                               s1s2$TEMP.MEAN.y))$r.squared
        correlations.monthly$WIND.SPD.MEAN.r2[irow] = summary(lm(s1s2$WIND.SPD.MEAN.x ~ 
                                                                   s1s2$WIND.SPD.MEAN.y))$r.squared
        
        # gather the outputs      
        correlations.monthly$M[irow] <- M
        correlations.monthly$site1.USAFID[irow] <- station.highrate[s1i]
        correlations.monthly$site1.NAME[irow] <- s1$NAME[1]
        correlations.monthly$site1.LAT[irow] <- s1$LAT[1]
        correlations.monthly$site1.LONG[irow] <- s1$LONG[1]
        correlations.monthly$site2.USAFID[irow] <- station.highrate[s2i]
        correlations.monthly$site2.NAME[irow] <- s2$NAME[1]
        correlations.monthly$site2.LAT[irow] <- s2$LAT[1]
        correlations.monthly$site2.LONG[irow] <- s2$LONG[1]     
      }
    }
  }
cat("...done\n")
}

# get the month as a string
correlations.monthly$Month <- droplevels(factor(correlations.monthly$M,
                                                levels = seq(1,12,1),
                                                labels = Months,
                                                exclude = NA,
                                                ordered = TRUE))

# trim that data frame
correlations.monthly<- correlations.monthly[!is.na(correlations.monthly$site1.USAFID),]
# save the data ----
save(correlations.monthly, 
     file = file.path(data.dir,"data","Correlations.Rdata"))
