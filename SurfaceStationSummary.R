# script to plot data from NOAA stations, based on http://blue.for.msu.edu/lab-notes/NOAA_0.1-1/NOAA-ws-data.pdf
rm(list = ls())
import.noaa.data <- FALSE

## USER INPUTS ----
# set code directory
code.dir = "~/Documents/projects/Rcode/SurfaceStationSummary"
# Set data directory
data.dir = "~/Documents/projects/WindResourceAssessment/Bangladesh/SurfaceStationSummary"
# define configuration file directory and name
config.dir <- code.dir
config.file = "BangladeshSurfaceStations.R"

# END OF USER INPUTS

## GET STARTED ----
setwd(data.dir)
source(file.path(config.dir,config.file))

## LIBRARIES ----
source(file.path(code.dir,"scripts","LoadPackages.R"))

## DIRECTORIES ----
source(file.path(code.dir,"scripts","PrepDirectories.R"))

## Go looking for stations ----
source(file.path(code.dir,"scripts","FindStations.R"))
if (import.noaa.data == TRUE){  
  source(file.path(code.dir,"scripts","RequestStationDataFiles.R"))
  source(file.path(code.dir,"scripts","ConvertStationDataFiles.R"))
}else{
  station.files <- read.csv(file = file.path(working.dir,
                                             "data",
                                             "stations.csv"), 
                            header = TRUE)
}
# load the data from those stations
source(file.path(code.dir,"scripts","LoadStationData.R"))
source(file.path(code.dir,"scripts","WriteStationInformation.R"))

## Create a base map that we will use to overlay plots on ----
source(file.path(code.dir,"scripts","CreateBaseMap.R"))
source(file.path(code.dir,"scripts","MapStations.R"))

# Create and plot information about all data ----
source(file.path(code.dir,"scripts","GetAllDataStatistics.R"))
source(file.path(code.dir,"scripts","PlotAllDataStatistics.R"))
source(file.path(code.dir,"scripts","MapAllDataStatistics.R"))

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