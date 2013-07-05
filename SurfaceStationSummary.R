# script to plot data from NOAA stations, based on http://blue.for.msu.edu/lab-notes/NOAA_0.1-1/NOAA-ws-data.pdf
rm(list = ls())
import.noaa.data <- TRUE

## USER INPUTS ----
# set code directory
code.dir = "~/Documents/projects/active/Rcode/SurfaceStationSummary"
# Set data directory
data.dir = "~/Documents/projects/active/Rcode/SurfaceStationSummary/Test/Maine"
# define configuration file directory and name
config.dir <- code.dir
config.file = "MaineSurfaceStations.R"

# END OF USER INPUTS

## GET STARTED ----
source(file.path(config.dir,config.file))

## LIBRARIES ----
source(file.path(code.dir,"scripts","LoadPackages.R"))

## DIRECTORIES ----
source(file.path(code.dir,"scripts","PrepDirectories.R"))
setwd(data.dir)

## SOURCE FUNCTIONS WE WILL USE ----
source(file.path(code.dir,"scripts","SourceFunctions.R"))

## TIDY UP THE GRAPHICS ----
theme_set(theme_gray(base_size = 10))
theme_update(axis.text.x = element_text(size=rel(0.8)),
             axis.text.y = element_text(size=rel(0.8)))

## Go looking for stations ----

# see if we are importing data or not
if (import.noaa.data == TRUE){  
  stations.all <- FindStations(country.code = country.code,
                               year.range = year.range,             
                               lat.range = lat.range,
                               long.range = long.range,
                               data.dir = data.dir)
  # save the station files to file
  write.csv(stations.all, 
            file = file.path(data.dir,
                             "data",
                             "stationlist.csv"),
            row.names = FALSE)
  # request files for these stations from the NOAA database
  station.files <- RequestStationDataFiles(station.list = stations.all,
                                           year.range = year.range,
                                           data.dir = data.dir,
                                           debug = 0)  
  # save the station files to file
  write.csv(station.files, 
            file = file.path(data.dir,
                             "data",
                             "stationfiles.csv"),
            row.names = FALSE)
  # convert the data to csv    
  ConvertStationDataFiles(station.files = station.files,                          
                          data.dir = data.dir,
                          debug = 0)
  # load the data from those stations
  obs.all <- LoadStationData(stations.all = stations.all,
                             station.files = station.files,
                             data.dir = data.dir)
  save(stations.all,
       station.files,
       obs.all,
       file = file.path(data.dir,
                        "data",
                        "AllObservations.Rdata"))  
  
  # Get images of each station
  ImageStations(stations = stations.all,
                data.dir = data.dir)
}else{
  load(file = file.path(data.dir,
                        "data",
                        "AllObservations.Rdata"))
}

# create a LaTeX table for reference
WriteStationInformation(stations.all,
                        data.dir,
                        country.code)

## Create a base map that we will use to overlay plots on ----
base.map <- CreateBaseMap(lat.range = lat.range,
                          long.range = long.range)
ggsave(file.path(data.dir,"maps","MapBase.png"),
       plot = base.map,
       scale = 1, width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE) 

## Map the stations ----
station.map <- MapStations(base.map = base.map,
                           stations = stations.all,
                           labels = TRUE)
ggsave(file.path(data.dir,"maps","MapStationsByUSAF.png"),
       plot = station.map,
       scale = 1, 
       width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

# Create and plot information about all data ----
# start with counts per year
PlotDataCountsByTimeInterval(GetDataCountsByTimeInterval(obs.all, 
                                                         interval = "year"),
                             interval = "year")
ggsave(file.path(data.dir,"figures","PlotDataCountByYear.png"),
       scale = 1, 
       width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

PlotDataCountsByTimeInterval(GetDataCountsByTimeInterval(obs.all, 
                                                         interval = "hour"),
                             interval = "hour")
ggsave(file.path(data.dir,"figures","PlotDataCountByHour.png"),
       scale = 1, 
       width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

# map the counts by year
MapDataCountsByTimeInterval(obs.all,
                            stations.all,
                            interval = "year",
                            lat.range = lat.range,
                            long.range = long.range)
ggsave(file.path(data.dir,"maps","MapDataCountByYear.png"),
       scale = 1, 
       width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

# show statistics about the stations
monthly.mean.mean <- merge(stations.all,
                           getCalendarMonthStatistics(getDailyStatistics(obs.all,
                                                                         stat = "mean"), 
                                                      stat = "mean"),                     
                           by = "USAF")

monthly.means.map <- ggmap(get_map(location = c(min(long.range),
                                                min(lat.range),
                                                max(long.range),
                                                max(lat.range)),
                                   source = "osm"),
                           base_layer = ggplot(aes(x = LONG,
                                                   y = LAT),
                                               data = monthly.mean.mean)) + 
  geom_point(aes(size = WIND.SPD),
             alpha = 0.5) +
  facet_wrap(~B) +
  scale_size_area(name = "Wind Speed") +
  labs(x = "Longitude",
       y = "Latitude") + 
  coord_map()
print(monthly.means.map)

ggsave(file.path(data.dir,"maps","MapMonthlyMeanWindSpeed.png"),
     plot = monthly.means.map,
     scale = 1, 
     width = 9,
     height = 6, units = c("in"),
     dpi = 300, limitsize = TRUE)

monthly.max.max <- merge(stations.all,
                         getCalendarMonthStatistics(getDailyStatistics(obs.all,
                                                                       stat = "max"), 
                                                    stat = "max"),                     
                         by = "USAF")

monthly.max.map <- ggmap(get_map(location = c(min(long.range),
                                              min(lat.range),
                                              max(long.range),
                                              max(lat.range)),
                                 source = "osm"),
                         base_layer = ggplot(aes(x = LONG,
                                                 y = LAT),
                                             data = monthly.max.max)) + 
  geom_point(aes(colour = WIND.SPD),
             alpha = 0.5) +
  facet_wrap(~B) +
  scale_color_continuous(name = "Wind Speed",
                         high = "red",
                         low = "blue",
                         limits = c(0,30)) +
  labs(x = "Longitude",
       y = "Latitude") + 
  coord_map()
print(monthly.max.map)

ggsave(file.path(data.dir,"maps","MapMonthlyMaximumWindSpeed.PNG"),
       plot = monthly.max.map,
       scale = 1, 
       width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

# create ribbons for each station showing ranges ----
monthly.min.min <- merge(stations.all,
                         getCalendarMonthStatistics(getDailyStatistics(obs.all,
                                                                       stat = "min"), 
                                                    stat = "min"),                     
                         by.x = "USAF",
                         by.y = "USAF")
monthly.mean.min <- merge(stations.all,
                          getCalendarMonthStatistics(getDailyStatistics(obs.all,
                                                                        stat = "min"), 
                                                     stat = "mean"),                     
                          by.x = "USAF",
                          by.y = "USAF")
monthly.mean.max <- merge(stations.all,
                          getCalendarMonthStatistics(getDailyStatistics(obs.all,
                                                                        stat = "max"), 
                                                     stat = "mean"),                     
                          by.x = "USAF",
                          by.y = "USAF")
monthly.ranges <- merge(merge(merge(monthly.max.max,
                                    monthly.mean.max,
                                    by = c("USAF","WBAN","B","NAME","CTRY","STATE","CALL","LAT","LONG","ELEV","BEGIN","END"),
                                    suffixes = c(".max.max",".mean.max")),
                              merge(monthly.mean.min,
                                    monthly.min.min,
                                    by = c("USAF","WBAN","B","NAME","CTRY","STATE","CALL","LAT","LONG","ELEV","BEGIN","END"),
                                    suffixes = c(".mean.min",".min.min")),
                              by = c("USAF","WBAN","B","NAME","CTRY","STATE","CALL","LAT","LONG","ELEV","BEGIN","END")),
                        monthly.mean.mean,
                        by = c("USAF","WBAN","B","NAME","CTRY","STATE","CALL","LAT","LONG","ELEV","BEGIN","END"),
                        suffixes = c("",".mean.mean"))

# plot this
monthly.ranges$ID <- paste(monthly.ranges$NAME, " (",monthly.ranges$USAF,")", sep = "")
monthly.ranges$Bsubstr  <- substr(monthly.ranges$B,1,1)
plot.temp.ranges <- ggplot(data = monthly.ranges,
                           aes(x = B,
                               ymin = `TEMP.min.min`,
                               lower = `TEMP.mean.min`,
                               middle = `TEMP`,
                               upper = `TEMP.mean.max`,
                               ymax = `TEMP.max.max`)) +
  geom_boxplot(stat = "identity") +
  facet_wrap(~ID) +
  scale_x_discrete("Month",
                   labels=substr(levels(monthly.ranges$B),1,1))
print(plot.temp.ranges)
ggsave(file.path(data.dir,"figures","PlotMonthlyTemperatureRange.png"),
       plot = plot.temp.ranges,
       scale = 1, 
       width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

# wind speed
plot.wind.speed.ranges <- ggplot(data = monthly.ranges,
                                 aes(x = B,
                                                           ymin = `WIND.SPD.min.min`,
                                                           lower = `WIND.SPD.mean.min`,
                                                           middle = `WIND.SPD`,
                                                           upper = `WIND.SPD.mean.max`,
                                                           ymax = `WIND.SPD.max.max`)) +
  geom_boxplot(stat = "identity") +
  facet_wrap(~ID) +
  scale_x_discrete("Month",
                   labels=substr(levels(monthly.ranges$B),1,1)) +
  labs(y = "Wind Speed")
  print(plot.wind.speed.ranges)
ggsave(file.path(data.dir,"figures","PlotMonthlyWindSpeedRange.png"),
       plot = plot.wind.speed.ranges,
       scale = 1, 
       width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)




# Create daily data (we'll need this for monthly values, later) 
#source(file.path(code.dir,"scripts","GetDailyDataStatistics.R"))
# create monthly data
#source(file.path(code.dir,"scripts","GetMonthlyDataStatistics.R"))
#source(file.path(code.dir,"scripts","PlotMonthlyDataStatistics.R"))
#source(file.path(code.dir,"scripts","GetMonthlyDataCorrelations.R"))
#source(file.path(code.dir,"scripts","MapMonthlyDataCorrelations.R"))

# Create climatologies
#source(file.path(code.dir,"scripts","GetMonthlyClimatology.R"))
#source(file.path(code.dir,"scripts","PlotMonthlyClimatology.R"))