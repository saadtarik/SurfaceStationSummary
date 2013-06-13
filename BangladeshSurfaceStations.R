## DATA TO FIND
# country code
target.country.code <- "BW"
# Bounding box. Latitudes and longitudes outside this range will be set to NA. 
# This assumes that the country code is sufficient to identify the location of a 
# station correctly.
target.long.max <- 94
target.long.min <- 86
target.lat.min <- 20
target.lat.max <- 27
# years we want data for
target.date.min <- as.Date("1981-01-01")
target.date.max <- as.Date("2010-12-31")

# Set working directory for this analysis----
working.dir = '~/Documents/projects/WindResourceAssessment/Bangladesh/ClimateMaps'
