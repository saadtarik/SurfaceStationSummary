# Temperature count versus location ----
# change the size of the marker on the map to show the number of data points
# add the stations
chart1 <- base.map+ 
  geom_point(data = all.data,
             aes(x = LONG, 
                 y = LAT),
             shape = "+",
             alpha = 0.5) +
  geom_point(data = all.data,
             aes(x = LONG, 
                 y = LAT,
                 size = TEMP.COUNT),
             colour = "blue",
             alpha = 0.5) +
  annotate("text",
           x = max(all.data$LONG),
           y = max(all.data$LAT),
           label = paste("From ", 
                         format(target.date.min,"%b %d, %Y"),
                         "\nto ",
                         format(target.date.max,"%b %d, %Y"),
                         sep=""),
           size = 2,
           hjust = 1)+        
  scale_size(range = c(0,8)) +  
  labs(x = "Longitude",
       y = "Latitude",
       size = "Number of\ntemperature\nobservations")+
  theme_bw(base_size = 10, base_family = "") + 
  theme(legend.justification=c(0,0),
        legend.position=c(0, 0),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.background = element_rect(colour = "black"))
print(chart1)

ggsave(file.path(data.dir,"figures","MapTempNonNACount.png"),
       plot = chart1,
       scale = 1, width = 3,
       height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)