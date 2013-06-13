## CreateBaseMap.R

if (use.ggmap == FALSE){
  ## Create base map using shape file ----
  map.in <- readShapeSpatial("data/gis/bgd_div_adm1_py/bgd_div_adm1_py.shp")
  map.in.f <- fortify(map.in,region="Upabnd2010")
  base.map<- ggplot(data=map.in.f,
                    aes(long,
                        lat)) +
    geom_polygon(fill="grey80",
                 aes(group=group)) +
    scale_fill_brewer("Regions") +
    labs(x = "Longitude",
         y = "Latitude") + 
    coord_map()
  print(base.map)
  
  ggsave(file.path(data.dir,"figures","MapBase.png"),
         plot = base.map,
         scale = 1, width = 3,
         height = 4, units = c("in"),
         dpi = 300, limitsize = TRUE)
  
  
  # http://stackoverflow.com/questions/10918102/r-adding-legend-to-ggmap-ggplot2-while-using-annotate?rq=1 
} else {
  ## ggmap version
  base.map = get_map(location = c(target.long.min,
                                  target.lat.min,
                                  target.long.max,
                                  target.lat.max),
                     source = "google",
                     maptype = "terrain")
  base.map = ggmap(base.map)
  base.map  
  
  ggsave(file.path(data.dir,"figures","MapBase.png"),
         plot = base.map,
         scale = 1, width = 3,
         height = 4, units = c("in"),
         dpi = 300, limitsize = TRUE) 
}