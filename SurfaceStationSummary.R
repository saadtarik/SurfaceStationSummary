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

## SOURCE FUNCTIONS WE WILL USE ----
source(file.path(code.dir,"scripts","SourceFunctions.R"))

## TIDY UP THE GRAPHICS ----
theme_set(theme_gray(base_size = 8))
theme_update(axis.text.x = element_text(size=rel(0.8)),
             axis.text.y = element_text(size=rel(0.8)),
             legend.text=element_text(size=rel(0.8)),
             legend.title=element_text(size=rel(1)))

##DIRECTORIES ----
# Create a unique ID for this project
projectID <- paste(country.code,
                   "LAT",lat.range[1],"to",lat.range[2],
                   "LONG",long.range[1],"to",long.range[2],
                   "FROM",year.range[1],"to",year.range[2],
                   sep="")
projectID<-gsub(pattern = "[.]",replacement = "pt",x = projectID)
prepDirectories(data.dir,
                projectID)
setwd(data.dir)

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
  obs.all <- LoadStationData(station.files = station.files,
                             data.dir = data.dir)
  # get the number of years of data
  stations.all <- merge(stations.all,
                        aggregate(cbind(NYRS = YR)~ID,
                                  data = obs.all,
                                  function(x) sum(!is.na(unique(x)))),
                        by = "ID")
  # create an annotation
  stations.all$NOTE= paste(stations.all$NAME, 
                           " (USAF ", stations.all$USAF, ", WBAN ",
                           stations.all$WBAN, ")\n",
                           stations.all$LONG, " W, ",
                           stations.all$LAT, " N, elevation ",
                           stations.all$ELEV, " m\n",
                           "From ",  min(year.range), 
                           " to ", max(year.range),
                           sep ="")
  # save the data
  save(stations.all,
       station.files,
       obs.all,
       file = file.path(data.dir,
                        projectID,
                        paste(projectID,"Rdata",sep =".")))  
  
}else{
  load(file = file.path(data.dir,
                        projectID,
                        paste(projectID,"Rdata",sep =".")))
}
# keep stations where we have at least n.years of observations:
stations <- stations.all[stations.all$NYRS >year.min.count, ]
obs <- obs.all[obs.all$ID %in% stations$ID, ]
files <- station.files[station.files$ID %in% stations$ID, ]

## create a LaTeX table for reference ----
WriteStationInformation(stations,
                        projectID,
                        data.dir,
                        country.code)

# Work through the data, station by station ----
for (ID in stations$ID){
  # create directorys for the data
  dir.create(file.path(data.dir,projectID,"figures",ID),recursive = TRUE)
}

for (ID in stations$ID){  
  # subset the data
  obs.subs = obs[obs$ID == ID,]  
  obs.subs$B = factor(obs.subs$M,
                      labels = format(as.Date(paste("2012",1:12,"01",sep = "-")),
                                      "%B",
                                      tz = "GMT"),
                      ordered = TRUE)
  
  ## show the time series of data for this data set ----
  obs.subs$Timestamp <- as.POSIXct(paste(obs.subs$DATE," ",
                                         sprintf("%02d", obs.subs$HR),":",
                                         sprintf("%02d", obs.subs$MIN),
                                         sep = ""),
                                   tz ="GMT",
                                   format = "%Y-%m-%d %H:%M")
  # save the data
  save(obs.subs,
       file = file.path(data.dir,projectID,"figures",ID,"Observations.RData"))
  
  obs.subs.melt <- melt(obs.subs,
                        id.vars = c("Timestamp"),
                        measure.vars = c("WIND.SPD","WIND.DIR","TEMP","ATM.PRES"))
  # catch some problems that can occur if the entire time series is empty
  if(all(is.na(obs.subs.melt$value[obs.subs.melt$variable == "WIND.SPD"]))){
    obs.subs.melt$value[obs.subs.melt$variable == "WIND.SPD"] <- -999.99
  }
  if(all(is.na(obs.subs.melt$value[obs.subs.melt$variable == "WIND.DIR"]))){
    obs.subs.melt$value[obs.subs.melt$variable == "WIND.DIR"] <- -999.99
  }  
  if(all(is.na(obs.subs.melt$value[obs.subs.melt$variable == "TEMP"]))){
    obs.subs.melt$value[obs.subs.melt$variable == "TEMP"] <- -999.99
  }
  if(all(is.na(obs.subs.melt$value[obs.subs.melt$variable == "ATM.PRES"]))){
    obs.subs.melt$value[obs.subs.melt$variable == "ATM.PRES"] <- -999.99
  }
  
  try(ggsave(file.path(data.dir,projectID,"figures",ID,"AllDataTimeSeries.png"),
             plot = ggplot(data = obs.subs.melt, 
                           aes(x = Timestamp)) +
               geom_point(aes(y = value,
                              color = variable),
                          size = 0.5) +
               facet_wrap(~variable,
                          ncol = 1,
                          scales = "free_y") +
               guides(color=FALSE),
             scale = 1, 
             width = 6, height = 4, units = c("in"),
             dpi = 300, limitsize = TRUE))
  
  ## show wind roses for this data set----
  
  # make one per station  
  try(ggsave(file.path(data.dir,projectID,"figures",ID,"WindRose.png"),
             plot = plotWindrose(data = obs.subs,
                                 spd = "WIND.SPD",
                                 dir = "WIND.DIR"),
             scale = 1, width = 5,
             height = 3, units = c("in"),
             dpi = 300, limitsize = TRUE))
  
  # now make one for each month  
  try(ggsave(file.path(data.dir,projectID,"figures",ID,"WindRoseByMonth.png"),
             plot = plotWindrose(data = obs.subs,
                                 spd = "WIND.SPD",
                                 dir = "WIND.DIR",
                                 opts = "facet_wrap(~B)"),
             scale = 1, width = 6,
             height = 4, units = c("in"),
             dpi = 300, limitsize = TRUE))
}

## Create a base map that we will use to overlay plots on ----
base.map <- CreateBaseMap(lat.range = lat.range,
                          long.range = long.range)
ggsave(file.path(data.dir,projectID,"maps","MapBase.png"),
       plot = base.map,
       scale = 1, width = 6,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE) 

## Map the stations ----
try(ggsave(file.path(data.dir,projectID,"maps","MapOfStationsByID.png"),
           plot = MapStations(lat.range = lat.range,
                              long.range = long.range,
                              base.map = base.map,
                              stations = stations,
                              labels = TRUE),
           scale = 1, 
           width = 6,
           height = 6, units = c("in"),
           dpi = 300, limitsize = TRUE))

for (ID in stations$ID){
  dir.create(file.path(data.dir,projectID,"figures",ID),recursive = TRUE)  
  # create an image of the station
  try(ggsave(file.path(data.dir,projectID,"figures",ID,"StationImage.png"),
             plot = showStation(stations[stations$ID == ID,],
                                precision = 0.0005),
             scale = 1, 
             width = 3,
             height = 3, units = c("in"),
             dpi = 300, limitsize = TRUE))
  
  try(ggsave(file.path(data.dir,projectID,"figures",ID,"StationLocation.png"),
             plot = MapStations(lat.range = lat.range,
                                long.range = long.range,
                                base.map = base.map,
                                stations = stations[stations$ID == ID,],
                                labels = TRUE),
             scale = 1, 
             width = 3,
             height = 3, units = c("in"),
             dpi = 300, limitsize = TRUE))
}

# Create and plot information about data coverage ----
# start with counts per year
counts.year <- GetDataCountsByTimeInterval(obs, 
                                           interval = "year")

try(ggsave(file.path(data.dir,projectID,"figures","PlotDataCountByYear.png"),
           plot = PlotDataCountsByTimeInterval(counts.year,
                                               interval = "year"),
           scale = 1, 
           width = 6,
           height = 6, units = c("in"),
           dpi = 300, limitsize = TRUE))

counts.hour <- GetDataCountsByTimeInterval(obs, 
                                           interval = "hour")
try(ggsave(file.path(data.dir,projectID,"figures","PlotDataCountByHour.png"),
           plot = PlotDataCountsByTimeInterval(counts.hour,
                                               interval = "hour"),
           scale = 1, 
           width = 6,
           height = 6, units = c("in"),
           dpi = 300, limitsize = TRUE))

# map the counts by year
ggsave(file.path(data.dir,projectID,"maps","MapOfDataCountByYear.png"),
       plot = MapDataCountsByTimeInterval(obs,
                                          stations,
                                          interval = "year",
                                          lat.range = lat.range,
                                          long.range = long.range),
       scale = 1, 
       width = 6,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

# produce plots for each station 
for (ID in stations$ID){  
  try(ggsave(file.path(data.dir,projectID,"figures",ID,"DataCountByYear.png"),
             plot = PlotDataCountsByTimeInterval(counts = counts.year[counts.year$ID == ID,],
                                                 interval = "year"),
             scale = 1, 
             width = 3,
             height = 3, units = c("in"),
             dpi = 300, limitsize = TRUE))
}

# CLIMATOLOGY ----
# get the climatology for this data set
climatology <- getClimatologies(obs,
                                stations,
                                year.min.count)
# Map statistics about the stations
# mean wind speed
climatology.map <- base.map %+% climatology + aes(x = LONG, y = LAT)
climatology.monthly.mean.wind.spd.map <- climatology.map +
  geom_point(aes(color = WIND.SPD.mean.mean),
             alpha = 0.5) +
  facet_wrap(~B) +
  scale_colour_gradientn(name = "Wind Speed",
                         limits = c(0,15),
                         colours = brewer.pal(7,"YlOrRd"),
                         breaks = c(0,3,6,9,12,15)) +
  labs(x = "Longitude",
       y = "Latitude") + 
  coord_map()
print(climatology.monthly.mean.wind.spd.map)

ggsave(file.path(data.dir,projectID,"maps","MapMonthlyMeanWindSpeed.png"),
       plot = climatology.monthly.mean.wind.spd.map,
       scale = 1, 
       width = 6,
       height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)

# maximum wind speed
climatology.monthly.max.wind.spd.map <- climatology.map +
  geom_point(aes(color = WIND.SPD.max.max),
             alpha = 0.5) +
  facet_wrap(~B) +
  scale_colour_gradientn(name = "Wind Speed",
                         limits = c(0,15),
                         colours = brewer.pal(7,"YlOrRd"),
                         breaks = c(0,3,6,9,12,15)) +
  labs(x = "Longitude",
       y = "Latitude") + 
  coord_map()
print(climatology.monthly.max.wind.spd.map)

ggsave(file.path(data.dir,projectID,"maps","MapMonthlyMaximumWindSpeed.PNG"),
       plot = climatology.monthly.max.wind.spd.map,
       scale = 1, 
       width = 6,
       height = 4, units = c("in"),
       dpi = 300, limitsize = TRUE)

## PLOT AND SAVE INDIVIDUAL CLIMATOLOGIES ----
# temperature
for (ID in stations$ID){
  dir.create(file.path(data.dir,projectID,"figures",ID))  
  # get the climatology for this ID
  climatology.subs <- climatology[climatology$ID == ID,]
  # save this data  
  save(climatology.subs,
       file = file.path(data.dir,projectID,"figures",ID,"Climatology.RData"))
  # temperature climatology
  try(ggsave(file.path(data.dir,projectID,"figures",ID,"TemperatureClimatology.png"),
             plot = plotClimatologies(climatology.subs,
                                      param = "TEMP"),
             scale = 1, 
             width = 3,
             height = 3, units = c("in"),
             dpi = 300, limitsize = TRUE))
  # wind speed climatology
  try(ggsave(file.path(data.dir,projectID,"figures",ID,"WindSpdClimatology.png"),
             plot = plotClimatologies(climatology.subs,
                                      param = "WIND.SPD"),
             scale = 1, 
             width = 3,
             height = 3, units = c("in"),
             dpi = 300, limitsize = TRUE))
}

## PLOT ALL CLIMATOLOGIES TOGETHER
# temperature
ggsave(file.path(data.dir,projectID,"figures","PlotOfTemperatureClimatology.png"),
       plot = plotTEMPClimatology(climatology),
       scale = 1, 
       width = 6,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)
# wind speed
ggsave(file.path(data.dir,projectID,"figures","PlotOfWindSpeedClimatology.png"),
       plot = plotWINDSPDClimatology(climatology),
       scale = 1, 
       width = 6,
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