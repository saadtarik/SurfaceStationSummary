# WindRose.R
require(ggplot2)
require(RColorBrewer)

plotWindrose <- function(data,
                         spd = "speed",
                         dir = "direction",
                         spdres = 2,
                         dirres = 30,
                         spdmin = 2,
                         spdmax = 20,
                         palette = "YlGnBu",
                         countmax = NA,
                         opts = NA,
                         debug = 0){
  # check inputs ----
  if (debug>0){
    cat("Speed = ", spd, "\n")
    cat("Direction = ", dir, "\n")
  }
  # Tidy up input data ----
  dnu <- (is.na(data[,spd]) | is.na(data[,dir]))
  data<- data[!dnu,]
  
  # figure out the wind speed bins ----
  n.colors.in.range <- length(seq(spdmin,spdmax,spdres))-1
  speedcuts.colors = colorRampPalette(brewer.pal(min(max(3,
                                                         n.colors.in.range),
                                                     min(9,
                                                         n.colors.in.range)),                                               
                                                 palette))(n.colors.in.range)
  if (max(data[,spd],na.rm = TRUE) > spdmax){    
    speedcuts.breaks <- c(seq(spdmin,spdmax,by = spdres),
                          max(data[,spd],na.rm = TRUE))
    speedcuts.labels <- c(paste(c(seq(spdmin,spdmax-spdres,by = spdres)),
                                '-',
                                c(seq(spdmin+spdres,spdmax,by = spdres))),
                          paste(spdmax,
                                "-",
                                max(data[,spd],na.rm = TRUE)))
    speedcuts.colors <- c(speedcuts.colors,
                          "grey50")
  } else{
    speedcuts.breaks <- c(seq(spdmin,spdmax,by = spdres))
    speedcuts.labels <- paste(c(seq(spdmin,spdmax-spdres,by = spdres)),
                              '-',
                              c(seq(spdmin+spdres,spdmax,by = spdres)))
    
  }
  if (debug > 0){
    cat(speedcuts.colors, "\n")
  }
  # Bin wind speed data ----
  data$spd.binned <- cut(data[,spd],
                         breaks = speedcuts.breaks,
                         labels = speedcuts.labels,
                         ordered_result = TRUE)  
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
  }
  data$dir.binned <- cut(data[,dir],
                         breaks = dir.breaks,
                         ordered_result = TRUE)
  levels(data$dir.binned) <- dir.labels
  
  # Run debug if required ----
  if (debug>0){    
    cat("levels(dir.binned) = ",levels(data$dir.binned),"\n")   
    cat("names(data) = ", names(data), "\n")   
    if (debug >1){
      cat(spd.binned,"\n")  
      cat(dir.binned,"\n")    
    }
  }  
  
  # create the plot ----
  plot.windrose <- ggplot(data = data,
                          aes(x = dir.binned,
                              fill = spd.binned)) +
    geom_bar()
  if (!is.na(opts)){
    plot.windrose <- eval(parse(text = paste("plot.windrose +", opts)))
  }
  plot.windrose <- plot.windrose +
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = speedcuts.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank())
  
  # adjust axes if required
  if (!is.na(countmax)){
    plot.windrose <- plot.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(plot.windrose)  
  
  # return the handle to the wind rose
  return(plot.windrose)
}