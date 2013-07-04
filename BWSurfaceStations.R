## DATA TO FIND
# country code
country.code <- "BW"
# Bounding box. Latitudes and longitudes outside this range will be set to NA. 
# This assumes that the country code is sufficient to identify the location of a 
# station correctly.
lon.range <- c(NULL, NULL)
lat.range <- c(NULL, NULL)
# years we want data for
year.range <- c(as.Date("1981-01-01"),as.Date("2001-12-31"))

# Set working directory for this analysis----
working.dir = '~/Documents/projects/Rcode/Test/BW'
