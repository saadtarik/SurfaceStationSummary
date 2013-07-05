## DATA TO FIND
# country code
country.code <- "US"
# Bounding box. Latitudes and longitudes outside this range will be set to NA. 
# This assumes that the country code is sufficient to identify the location of a 
# station correctly.
long.range <- c(-71.5, -67.5)
lat.range <- c(42.5, 44.5)
# years we want data for
year.range <- c(as.Date("2010-01-01"),
                as.Date("2012-12-31"))

# Set working directory for this analysis----
working.dir = '~/Documents/projects/active/Rcode/Test/Maine/ClimateMaps'
