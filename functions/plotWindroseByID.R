plotWindroseByID<- function(obs,
                            spd = "WIND.SPD",
                            dir = "WIND.DIR",
                            opts = "",                            
                            data.dir){
  
  for (ID in unique(obs$ID)){
    cat("Working on", ID, "\n")
    # get the data
    obs.subs <- obs[obs$ID == ID,]
    # filter for NA
    dnu <- (is.na(obs.subs[,spd]) | is.na(obs.subs[,dir]))
    obs.subs <- obs.subs[!dnu,]
    
    # make the plot
    if (NROW(obs.subs) >1){
      cat("...plotting wind rose.\n")
      plot.windrose <- plotWindrose(data = obs.subs,
                                    spd = spd,
                                    dir = dir,
                                    opts = opts)  
      print(plot.windrose)
      } else {
      cat("...skipping this station; insufficient data.\n")
    }
  }
}