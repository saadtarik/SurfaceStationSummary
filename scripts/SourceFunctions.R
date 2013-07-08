# script to source all of the functions we will use

# prep directories
source(file.path(code.dir,"functions","PrepDirectories.R"))

# wind roses ---- 
source(file.path(code.dir,"functions","WindRose.R"))

# find stations ----
source(file.path(code.dir,"functions","FindStations.R"))
source(file.path(code.dir,"functions","RequestStationDataFiles.R"))
source(file.path(code.dir,"functions","ConvertStationDataFiles.R"))
source(file.path(code.dir,"functions","LoadStationData.R"))
source(file.path(code.dir,"functions","WriteStationInformation.R"))

# create a base map
source(file.path(code.dir,"functions","CreateBaseMap.R"))

# map stations
source(file.path(code.dir,"functions","MapStations.R"))
source(file.path(code.dir,"functions","ImageStations.R"))

# get statistics from each station
source(file.path(code.dir,"functions","plotWindroseByID.R"))
source(file.path(code.dir,"functions","GetDataCountsByTimeInterval.R"))
source(file.path(code.dir,"functions","PlotDataCountsByTimeInterval.R"))
source(file.path(code.dir,"functions","MapDataCountsByTimeInterval.R"))
source(file.path(code.dir,"functions","GetDataStatsByTimeInterval.R"))
source(file.path(code.dir,"functions","Climatologies.R"))