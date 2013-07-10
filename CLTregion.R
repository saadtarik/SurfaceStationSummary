## DATA TO FIND
# country code
country.code <- "US"
# Bounding box. Latitudes and longitudes outside this range will be set to NA. 
# This assumes that the country code is sufficient to identify the location of a 
# station correctly.
# station is at 36.904639, -75.71272
long.range <- c(-76.7, -74.7)
lat.range <- c(35.9, 37.9)
# years we want data for
year.range <- c(as.Date("2009-01-01"),
                as.Date("2012-12-31"))
year.min.count = 2
