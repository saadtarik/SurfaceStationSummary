file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv"
repeat {    
  try(download.file(file, "data/ish-history.csv",quiet = TRUE))
  if (file.info("data/ish-history.csv")$size > 0){
    break
  }
}
st <- read.csv("data/ish-history.csv")

## Define station data ----
# check the station list
dim(st)
names(st)
names(st)[c(3, 10)] <- c("NAME", "ELEV")
st <- st[, -5]
# identify stations that are in bangladesh
st <- st[st$CTRY == target.country.code, ]

# fix something that means that names get treated as factors
st$NAME <- as.character(st$NAME)

# get more metadata
st$LAT <- st$LAT/1000
st$LON <- st$LON/1000
st$ELEV <- st$ELEV/10
st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
st$END <- as.numeric(substr(st$END, 1, 4))

# find stations that are within the bounding box and year ranges
target.list <- st[(st$BEGIN <= as.numeric(format(target.date.max,'%Y')) &
                     st$END >= as.numeric(format(target.date.min,'%Y')) & 
                     ((st$LAT >= target.lat.min) & (st$LAT <= target.lat.max))  &
                     ((st$LON >= target.long.min) & (st$LON <= target.long.max))  &
                     !is.na(st$BEGIN)), ]
rm(st)