library("reshape")

require("rgdal") # requires sp, will use proj.4 if installed
require("sp")
require("gpclib")
require("maptools")
require("mapproj")
require("ggplot2")
require("plyr")
require("xtable")
gpclibPermit() # required for fortify method