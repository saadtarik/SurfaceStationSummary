# script to plot data from NOAA stations, based on http://blue.for.msu.edu/lab-notes/NOAA_0.1-1/NOAA-ws-data.pdf
rm(list = ls())
import.noaa.data <- FALSE

## USER INPUTS ----
# set code directory
code.dir = "~/Documents/projects/Rcode/SurfaceStationSummary"
# Set data directory
data.dir = "~/Documents/projects/Rcode/SurfaceStationSummary/Test/Maine"
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
ggsave(file.path(data.dir,"figures","MapBase.png"),
       plot = base.map,
       scale = 1, width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE) 

## Map the stations ----
station.map <- MapStations(base.map = base.map,
                           stations = stations.all,
                           labels = TRUE)
ggsave(file.path(data.dir,"figures","MapStationsByUSAF.png"),
       plot = station.map,
       scale = 1, 
       width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

## Get images of each station ----
ImageStations(stations = stations.all,
              data.dir = data.dir)

# Create and plot information about all data ----
# start with counts per year
PlotDataCountsByTimeInterval(GetDataCountsByTimeInterval(obs.all, 
                                                         interval = "year"),
                             interval = "year")

PlotDataCountsByTimeInterval(GetDataCountsByTimeInterval(obs.all, 
                                                         interval = "hour"),
                             interval = "hour")
# map the counts
MapDataCountsByTimeInterval(obs.all,
                            stations.all,
                            interval = "overall",
                            lat.range,
                            long.range)

if (0){
  # this function needs ggmap to be rewritten...
  MapDataCountsByTimeInterval(obs.all,
                            stations.all,
                            interval = "year",
                            lat.range,
                            long.range)
} else {
  counts <- merge(stations.all,
                  melt(GetDataCountsByTimeInterval(obs.all, 
                                              interval = "year"),
                       id.vars = c("USAF","YR")),
                  by.x = "USAF",
                  by.y = "USAF")
  
  count.map <- ggmap(get_map(location = c(min(long.range),
                                          min(lat.range),
                                          max(long.range),
                                          max(lat.range)),
                             source = "osm"),
                     base_layer = ggplot(aes(x = LONG,
                                             y = LAT),
                                         data = counts)) + 
    geom_point(aes(size = value),
               alpha = 0.5) +
    facet_grid(variable~YR) +
    scale_size_area(name = "Number of \ndata points") +
    labs(x = "Longitude",
         y = "Latitude") + 
    coord_map()
  print(count.map)
}

ggsave(file.path(data.dir,"figures","MapStationsCount.png"),
       plot = count.map,
       scale = 1, 
       width = 9,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

# show statistics about the stations
monthly.means <- merge(stations.all,
                getCalendarMonthStatistics(getDailyStatistics(obs.all,
                                                                   stat = "mean"), 
                                                 stat = "mean"),                     
                by.x = "USAF",
                by.y = "USAF")

monthly.means.map <- ggmap(get_map(location = c(min(long.range),
                                        min(lat.range),
                                        max(long.range),
                                        max(lat.range)),
                           source = "osm"),
                   base_layer = ggplot(aes(x = LONG,
                                           y = LAT),
                                       data = monthly.means)) + 
  geom_point(aes(size = WIND.SPD),
             alpha = 0.5) +
  facet_wrap(~B) +
  scale_size_area(name = "Wind Speed") +
  labs(x = "Longitude",
       y = "Latitude") + 
  coord_map()
print(monthly.means.map)

monthly.max <- merge(stations.all,
                       getCalendarMonthStatistics(getDailyStatistics(obs.all,
                                                                     stat = "max"), 
                                                  stat = "max"),                     
                       by.x = "USAF",
                       by.y = "USAF")

monthly.max.map <- ggmap(get_map(location = c(min(long.range),
                                                min(lat.range),
                                                max(long.range),
                                                max(lat.range)),
                                   source = "osm"),
                           base_layer = ggplot(aes(x = LONG,
                                                   y = LAT),
                                               data = monthly.max)) + 
  geom_point(aes(size = WIND.SPD),
             alpha = 0.5) +
  facet_wrap(~B) +
  scale_size_area(name = "Wind Speed") +
  labs(x = "Longitude",
       y = "Latitude") + 
  coord_map()
print(monthly.max.map)


# Create daily data (we'll need this for monthly values, later) 
source(file.path(code.dir,"scripts","GetDailyDataStatistics.R"))
# create monthly data
source(file.path(code.dir,"scripts","GetMonthlyDataStatistics.R"))
source(file.path(code.dir,"scripts","PlotMonthlyDataStatistics.R"))
source(file.path(code.dir,"scripts","GetMonthlyDataCorrelations.R"))
source(file.path(code.dir,"scripts","MapMonthlyDataCorrelations.R"))

# Create climatologies
source(file.path(code.dir,"scripts","GetMonthlyClimatology.R"))
source(file.path(code.dir,"scripts","PlotMonthlyClimatology.R"))