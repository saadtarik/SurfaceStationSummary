# Reshape the data ----
climate.monthly.melt <- cast(melt.list(climate.monthly,
                                       id = c("USAFID","M")),
                             USAFID + M ~ variable)

climate.monthly.melt$ID <- paste(climate.monthly.melt$USAFID,
                                 " (", climate.monthly.melt$NAME, ")",
                                 sep="")

# note that we now have to fix some variables as well
climate.monthly.melt$TEMP.MIN <- as.numeric(as.character(climate.monthly.melt$TEMP.MIN))
climate.monthly.melt$TEMP.MEAN <- as.numeric(as.character(climate.monthly.melt$TEMP.MEAN))
climate.monthly.melt$TEMP.MAX <- as.numeric(as.character(climate.monthly.melt$TEMP.MAX))

climate.monthly.melt$WIND.SPD.MIN <- as.numeric(as.character(climate.monthly.melt$WIND.SPD.MIN))
climate.monthly.melt$WIND.SPD.MEAN <- as.numeric(as.character(climate.monthly.melt$WIND.SPD.MEAN))
climate.monthly.melt$WIND.SPD.MAX <- as.numeric(as.character(climate.monthly.melt$WIND.SPD.MAX))

# do some plotting
d <- ggplot(data = climate.monthly.melt,
            aes(x = M)) +    
  facet_wrap(~ID, ncol = 3) + 
  geom_ribbon(aes(ymax = TEMP.MAX,
                  ymin = TEMP.MIN,
                  fill = "temp")) +
  geom_line(aes(y = TEMP.MEAN)) + 
  geom_ribbon(aes(ymax = WIND.SPD.MAX,
                  ymin = WIND.SPD.MIN,
                  fill = "wind")) +
  geom_line(aes(y = WIND.SPD.MEAN)) +  
  coord_cartesian(xlim = c(1, 12)) + 
  coord_cartesian(ylim = c(-5, 35)) +     
  labs(x = "Month",
       y = "Value") +    
  scale_fill_manual(name = "Values",
                    labels = c("temp" ="Temperature",
                               'wind' = "Wind speed"),
                    values = c("temp" = "pink",
                               "wind" = "lightblue")) +
  scale_x_continuous(breaks=c(1,3,5,7,9,11),
                     labels=c('J','M','M','J','S','N')) + 
  theme_bw(base_size = 10, base_family = "") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "horizontal",        
        legend.background = element_rect(fill = "white",
                                         colour = "black"))  
print(d)

ggsave(file.path(data.dir,"figures",
                 paste("ClimateDiagrams.png",sep="")),
       plot = last_plot(),
       scale = 1, width = 7,
       height = 8, units = c("in"),
       dpi = 300, limitsize = TRUE)