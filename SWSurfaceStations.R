## DATA TO FIND
# country code
country.code <- "SW"
# Bounding box. Latitudes and longitudes outside this range will be set to NA. 
# This assumes that the country code is sufficient to identify the location of a 
# station correctly.
long.range <- c(6, 11)
lat.range <- c(45.5, 48)
# years we want data for
year.range <- c(as.Date("2010-01-01"),
                as.Date("2012-12-31"))

# Set working directory for this analysis----
working.dir = '~/Documents/projects/Rcode/Test/SW/ClimateMaps'
