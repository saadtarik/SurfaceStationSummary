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
  stations <- FindStations(country.code = country.code,
                               year.range = year.range,             
                               lat.range = lat.range,
                               long.range = long.range,
                               data.dir = data.dir)
  # save the station files to file
  write.csv(stations, 
            file = file.path(data.dir,
                             "data",
                             "stationlist.csv"),
            row.names = FALSE)
  # request files for these stations from the NOAA database
  station.files <- RequestStationDataFiles(station.list = stations,
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
  # save the data
  save(stations,
       station.files,
       file = file.path(data.dir,
                        projectID,
                        paste(projectID,"Rdata",sep =".")))    
}else{
  load(file = file.path(data.dir,
                        projectID,
                        paste(projectID,"Rdata",sep =".")))
}

# BASE MAP ----
# Create a base map that we will use to overlay plots on
base.map <- CreateBaseMap(lat.range = lat.range,
                          long.range = long.range)
ggsave(file.path(data.dir,projectID,"maps","MapBase.png"),
       plot = base.map,
       scale = 1, width = 6,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE) 

#  WORK THROUGH STATIONS ----
for (ID in stations$ID){
  if (any(station.files$ID == ID,na.rm = TRUE)){
  # load the data from those stations
  obs <- LoadStationData(station.files = station.files[station.files$ID == ID,],
                         data.dir = data.dir)  
  # get the number of years of data from this station
  stations$NYRS[stations$ID == ID] <- length(unique(obs$YR))
  
  # work on this data if we have a decent amount
  if (stations$NYRS[stations$ID == ID] >= year.min.count){
    # create directorys for the data
    dir.create(file.path(data.dir,projectID,"figures",ID),recursive = TRUE) 
    
    ## station data
    station <- stations[stations$ID == ID,]    
    
    # create an annotation
    station$NOTE= paste(station$NAME, 
                        " (USAF ", station$USAF, ", WBAN ", station$WBAN, ")\n",
                        station$LONG, " W, ", station$LAT, " N, elevation ",
                        station$ELEV, " m\n",
                        "From ",  min(year.range), " to ", max(year.range),
                        sep ="")    
    
    # MAPS ----
    # retrieve an aerial image of the station
    try(ggsave(file.path(data.dir,projectID,"figures",ID,"StationImage.png"),
               plot = showStation(station,
                                  precision = 0.0005),
               scale = 1, 
               width = 3,
               height = 3, units = c("in"),
               dpi = 300, limitsize = TRUE))
    # create a map showing the location of the station
    try(ggsave(file.path(data.dir,projectID,"figures",ID,"StationLocation.png"),
               plot = MapStations(lat.range = lat.range,
                                  long.range = long.range,
                                  base.map = base.map,
                                  stations = station,
                                  labels = TRUE),
               scale = 1, 
               width = 3,
               height = 3, units = c("in"),
               dpi = 300, limitsize = TRUE))
    
    # DATA ----    
    ## show the time series of data for this data set ----
    obs$Timestamp <- as.POSIXct(paste(obs$DATE," ",
                                      sprintf("%02d", obs$HR),":",
                                      sprintf("%02d", obs$MIN),
                                      sep = ""),
                                tz ="GMT",
                                format = "%Y-%m-%d %H:%M")
    # add a month
    obs$B = factor(obs$M,
                   labels = format(as.Date(paste("2012",1:12,"01",sep = "-")),
                                   "%B",
                                   tz = "GMT"),
                   ordered = TRUE)    
    
    # TIME SERIES OF OBSERVATIONS ----
    obs.melt <- melt(obs,
                     id.vars = c("Timestamp"),
                     measure.vars = c("WIND.SPD","WIND.DIR","TEMP","ATM.PRES"))
    # catch some problems that can occur if the entire time series is empty
    if(all(is.na(obs.melt$value[obs.melt$variable == "WIND.SPD"]))){
      obs.melt$value[obs.melt$variable == "WIND.SPD"] <- -999.99
    }
    if(all(is.na(obs.melt$value[obs.melt$variable == "WIND.DIR"]))){
      obs.melt$value[obs.melt$variable == "WIND.DIR"] <- -999.99
    }  
    if(all(is.na(obs.melt$value[obs.melt$variable == "TEMP"]))){
      obs.melt$value[obs.melt$variable == "TEMP"] <- -999.99
    }
    if(all(is.na(obs.melt$value[obs.melt$variable == "ATM.PRES"]))){
      obs.melt$value[obs.melt$variable == "ATM.PRES"] <- -999.99
    }
    
    try(ggsave(file.path(data.dir,projectID,"figures",ID,"AllDataTimeSeries.png"),
               plot = ggplot(data = obs.melt, 
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
    
    ## DATA AVAILABILTY ----
    counts.year <- GetDataCountsByYear(obs)
    try(ggsave(file.path(data.dir,projectID,"figures",ID,"DataCountByYear.png"),
               plot = PlotDataCountsByTimeInterval(counts = counts.year,
                                                   interval = "year"),
               scale = 1, 
               width = 3,
               height = 3, units = c("in"),
               dpi = 300, limitsize = TRUE))
    
    ## WIND ROSES ----
    # overall
    try(ggsave(file.path(data.dir,projectID,"figures",ID,"WindRose.png"),
               plot = plotWindrose(data = obs,
                                   spd = "WIND.SPD",
                                   dir = "WIND.DIR"),
               scale = 1, width = 5,
               height = 3, units = c("in"),
               dpi = 300, limitsize = TRUE))
    
    # one for each month  
    try(ggsave(file.path(data.dir,projectID,"figures",ID,"WindRoseByMonth.png"),
               plot = plotWindrose(data = obs,
                                   spd = "WIND.SPD",
                                   dir = "WIND.DIR",
                                   opts = "facet_wrap(~B)"),
               scale = 1, width = 6,
               height = 4, units = c("in"),
               dpi = 300, limitsize = TRUE))
    
    ## GENERATE CLIMATOLOGY ----    
    climatology <- getClimatologies(obs = obs,                                    
                                    year.min.count)         
    # PLOT CLIMATOLOGY ----
    # temperature climatology
    try(ggsave(file.path(data.dir,projectID,"figures",ID,"TemperatureClimatology.png"),
               plot = plotClimatologies(climatology,
                                        param = "TEMP"),
               scale = 1, 
               width = 3,
               height = 3, units = c("in"),
               dpi = 300, limitsize = TRUE))
    # wind speed climatology
    try(ggsave(file.path(data.dir,projectID,"figures",ID,"WindSpdClimatology.png"),
               plot = plotClimatologies(climatology,
                                        param = "WIND.SPD"),
               scale = 1, 
               width = 3,
               height = 3, units = c("in"),
               dpi = 300, limitsize = TRUE))
    
    ## SAVE DATA ----
    # observations
    save(obs,
         file = file.path(data.dir,projectID,"figures",ID,"Observations.RData"))
    # data counts
    save(counts.year,
         file = file.path(data.dir,projectID,"figures",ID,"DataCounts.RData"))    
    # climatology
    save(climatology,
         file = file.path(data.dir,projectID,"figures",ID,"Climatology.RData"))
    
    ## REMOVE TEMPORARY STUFF ----
    rm(station,obs, obs.melt, counts.year, climatology)
    
  } # end of loop that only runs if we have good data
  } else 
  {
    # get the number of years of data from this station
    stations$NYRS[stations$ID == ID] = 0    
  }
}

## CREATE SUMMARY DATA ----

# keep stations where we have at least n.years of observations:
stations.sub <- stations[stations$NYRS >=year.min.count, ]

## create a LaTeX table for reference ----
WriteStationInformation(stations.sub,
                        projectID,
                        data.dir,
                        country.code)

## Map the stations ----
try(ggsave(file.path(data.dir,projectID,"maps","MapOfStationsByID.png"),
           plot = MapStations(lat.range = lat.range,
                              long.range = long.range,
                              base.map = base.map,
                              stations = stations.sub,
                              labels = TRUE),
           scale = 1, 
           width = 6,
           height = 6, units = c("in"),
           dpi = 300, limitsize = TRUE))

# Create and plot information about data coverage ----
# load all of the data count files
for (ID in stations.sub$ID){
  load(file=file.path(data.dir,projectID,"figures",ID,"DataCounts.RData"))
  counts.year$ID <- ID
  if (ID == stations.sub$ID[1]){
    counts.year.all <- counts.year
  } else {
    counts.year.all <- rbind(counts.year.all,
                             counts.year)
  }   
}

try(ggsave(file.path(data.dir,projectID,"figures","PlotOfDataCountByYear.png"),
           plot = PlotDataCountsByYearByID(counts.year.all),
           scale = 1, 
           width = 6,
           height = 6, units = c("in"),
           dpi = 300, limitsize = TRUE))

# map the counts by year
counts.map <- base.map %+% merge(stations.sub, 
                   counts.year.all,  
                   by = "ID") +
  aes(x = LONG, 
      y = LAT)
ggsave(file.path(data.dir,projectID,"maps","MapOfDataCountByYear.png"),
       plot = counts.map +
         geom_point(aes(size=WIND.SPD.COUNT),
                    alpha = 0.5) +
         facet_wrap(~YR) +
         scale_size_continuous(name = "Wind Speed\nData Points") +
         labs(x = "Longitude",
              y = "Latitude") + 
         coord_map(), 
       scale = 1, 
       width = 6,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)

# CLIMATOLOGY ----
for (ID in stations.sub$ID){
  load(file=file.path(data.dir,projectID,"figures",ID,"Climatology.RData"))
  climatology$ID <- ID
  if (ID == stations.sub$ID[1]){
    climatology.all <- climatology
  } else {
    climatology.all <- rbind(climatology.all,
                             climatology)
  }
}
# bin the data
climatology.all$WIND.SPD.mean.mean.binned <- cut(climatology.all$WIND.SPD.mean.mean,
                                             breaks = c(seq(0,20,2)),
                                             labels = paste(seq(0,18,2),
                                                            "-",
                                                            seq(2,20,2)))
climatology.all$WIND.SPD.max.max.binned <- cut(climatology.all$WIND.SPD.max.max,
                                             breaks = c(seq(0,20,5),30,40,50),
                                             labels = c(paste(seq(0,15,5),
                                                              "-",
                                                              seq(5,20,5)),
                                                        "20 - 30","30 - 40","40 - 50"))

# Map statistics about the stations ----
# create the map we'll need
climatology.map <- base.map %+% 
  merge(stations.sub,
        climatology.all,
        by = "ID") +
  aes(x = LONG, y = LAT)
# mean wind speed
climatology.monthly.mean.wind.spd.map <- climatology.map +
  geom_point(aes(color = WIND.SPD.mean.mean.binned)) +
  facet_wrap(~B) +
  scale_colour_manual(name = "Wind Speed\n(Mean daily-mean)",
                      labels = paste(seq(0,18,2),
                                       "-",
                                       seq(2,20,2)),
                      values = colorRampPalette(brewer.pal(7,"YlOrRd"))(10)) +
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
  geom_point(aes(color = WIND.SPD.max.max.binned)) +
  facet_wrap(~B) +
  scale_colour_manual(name = "Wind Speed\n(maximum)",
                      labels = c(paste(seq(0,15,5),
                                       "-",
                                       seq(5,20,5)),
                                 "20 - 30","30 - 40","40-50"),
                      values = c(colorRampPalette(brewer.pal(7,"YlOrRd"))(4),
                                 colorRampPalette(brewer.pal(3,"RdPu"))(3))) +  
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

## PLOT ALL CLIMATOLOGIES TOGETHER
# temperature
plot.temperature.climatology <- plotTEMPClimatology(climatology.all) + facet_wrap(~ID)
print(plot.temperature.climatology)
ggsave(file.path(data.dir,projectID,"figures","PlotOfTemperatureClimatology.png"),
       plot = plot.temperature.climatology,
       scale = 1, 
       width = 6,
       height = 6, units = c("in"),
       dpi = 300, limitsize = TRUE)
# wind speed
plot.wind.speed.climatology <- plotWINDSPDClimatology(climatology.all) + facet_wrap(~ID)
print(plot.wind.speed.climatology)
ggsave(file.path(data.dir,projectID,"figures","PlotOfWindSpeedClimatology.png"),
       plot = plot.wind.speed.climatology,
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