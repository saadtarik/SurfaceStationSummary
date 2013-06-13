## GET DATA FROM EACH STATION ----
# get the total count of non-NA data
all.counts <- aggregate(cbind(TEMP.COUNT=TEMP,
                              WIND.SPD.COUNT=WIND.SPD,
                              ATM.PRES.COUNT=ATM.PRES)~USAFID,
                        data=obs.all,
                        function(x) sum( !is.na(x) ))

all.means <- aggregate(cbind(TEMP.MEAN=TEMP,
                             WIND.SPD.MEAN=WIND.SPD,
                             ATM.PRES.MEAN=ATM.PRES)~USAFID,
                       data=obs.all,
                       function(x) mean(x, na.rm=TRUE))

# find the station locations
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
all.locations <- aggregate(cbind(LAT = LAT,
                                 LONG = LONG,
                                 ELEV = ELEV)~USAFID,
                           data=obs.all,
                           function(x) Mode(x))
# merge this data
all.data<-merge(merge(all.locations,
                      all.counts, 
                      by = "USAFID"),
                all.means,                            
                by = "USAFID")