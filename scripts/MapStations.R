# Map stations on base map ----
station.map <- base.map +
  geom_point(data = target.list,
             aes(x = LON,
                 y = LAT),
             shape = "+") +
  geom_text(data = target.list,
            aes(x = LON,
                y = LAT,
                label = USAF),
            size = 1,
            hjust = 0,
            vjust = 0.5,
            angle = 30,
            position = position_jitter(width=0, height=0.1)) + 
  labs(x = "Longitude",
       y = "Latitude") +
  theme_bw(base_size = 10, base_family = "")
print(station.map)

ggsave(file.path(data.dir,"figures","MapStationsByUSAFID.png"),
       plot = station.map,
       scale = 1, width = 3,
       height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)

# see also solution that uses google to get the map
# http://stackoverflow.com/questions/13426470/justification-of-multiple-legends-in-ggmap-ggplot2