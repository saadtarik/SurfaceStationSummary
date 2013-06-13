
# plot the temperature correlations
correlations.monthly$TEMP.MEAN.r2.levels<-cut(correlations.monthly$TEMP.MEAN.r2, 
                                              breaks=c(0.5,0.75,0.9,0.95,1), 
                                              labels = paste(">",c(0.5,0.75,0.9,0.95)))
d <-  ggplot(data = correlations.monthly) +
  geom_polygon(data = map.in.f,               
               aes(x = long,
                   y = lat,
                   group=group),
               fill="grey80") +    
  geom_segment(aes(x = site1.LONG,
                   xend = site2.LONG,
                   y = site1.LAT,
                   yend = site2.LAT,
                   color = TEMP.MEAN.r2.levels)) +   
  scale_colour_brewer(palette="YlOrRd",
                      name = "Correlation R2") + 
  labs(x = "Longitude",
       y = "Latitude") + 
  coord_map() +
  facet_wrap(~Month, ncol = 4) +
  theme_bw(base_size = 10, base_family = "") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "horizontal",        
        legend.background = element_rect(fill = "white",
                                         colour = "black")) 
print(d)

ggsave(file.path(data.dir,"figures","MapTemperatureCorrelations.png"),
       scale = 1, width = 7,
       height = 8, units = c("in"),
       dpi = 300, limitsize = TRUE)

# facet_wrap(~MONTH, ncol = 4) +

# plot the wind speed correlations
correlations.monthly$WIND.SPEED.MEAN.r2.levels<-cut(correlations.monthly$WIND.SPD.MEAN.r2, 
                                                    breaks=c(0.5,0.75,0.9,0.95,1), 
                                                    labels = paste(">",c(0.5,0.75,0.9,0.95)))
d <-  ggplot(data = correlations.monthly) +
  geom_polygon(data = map.in.f,               
               aes(x = long,
                   y = lat,
                   group=group),
               fill="grey80") +    
  geom_segment(aes(x = site1.LONG,
                   xend = site2.LONG,
                   y = site1.LAT,
                   yend = site2.LAT,
                   color = WIND.SPEED.MEAN.r2.levels)) + 
  scale_colour_brewer(palette="YlOrRd",
                      name = "Correlation R2") +   
  facet_wrap(~Month, ncol = 4) +
  labs(x = "Longitude",
       y = "Latitude") + 
  coord_map() +
  theme_bw(base_size = 10, base_family = "") +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
        legend.direction = "horizontal",        
        legend.background = element_rect(fill = "white",
                                         colour = "black")) 
print(d)

ggsave(file.path(data.dir,"figures","MapWindSpeedCorrelations.png"),
       scale = 1, width = 7,
       height = 8, units = c("in"),
       dpi = 300, limitsize = TRUE)