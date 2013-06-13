# script PlotAllDataStatistics.R 

# Temperature count versus station ID ----
d <- ggplot(data = all.data,
            aes(x = factor(USAFID),
                y = TEMP.COUNT)) +  
  geom_bar(stat = "identity") +  
  labs(y = "Number of non-NA temperature data",
       x = "Station identifier (USAFID)") +
  coord_flip() +
  theme_bw(base_size = 10, base_family = "") +
  annotate("text",
           y = 0.8* max(all.data$TEMP.COUNT),
           x = floor(0.8*NROW(unique.USAFID)),
           label = paste("Data are from ",
                         format(target.date.min,"%b %d, %Y"),"\n",
                         "to",
                         format(target.date.max,"%b %d, %Y")),
           size = 3)
print(d)

ggsave(file.path(data.dir,"figures","BarTempNonNACount.png"),
       scale = 1, width = 6,
       height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# get stations where there is a lot of data ----
station.highrate <- all.data$USAFID[all.data$TEMP.COUNT >
                                      (0.1 *max(all.data$TEMP.COUNT,na.rm=TRUE))]

obs.all.highrate <- obs.all[obs.all$USAFID %in% station.highrate,]
obs.all.highrate$ID <- paste(obs.all.highrate$USAFID,
                             " (", obs.all.highrate$NAME, ")",
                             sep="")

# show frequency with which data are obtained ----

d <- ggplot(data = obs.all.highrate,
            aes(x = HR)) +
  geom_bar(stat="bin",binwidth=1) +
  facet_wrap(~ID,ncol = 3) +
  labs(x = "Hour of Day",
       y = "Count") +
  xlim(0,24) +
  theme_bw(base_size = 10, base_family = "")
print(d)

ggsave(file.path(data.dir,"figures","BarTempCountByHour.png"),
       scale = 1, width = 7,
       height = 3.5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# show temperature data versus date ----
d <- ggplot(data = obs.all.highrate,
            aes(x = DATE,
                y = TEMP)) +
  geom_point(shape = ".") +
  facet_wrap(~ID,ncol = 3) +
  labs(x = "Year",
       y = "Temperature") +
  theme_bw(base_size = 10, base_family = "")
print(d)

ggsave(file.path(data.dir,"figures","ScatterTempObs.png"),
       scale = 1, width = 7,
       height = 3.5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# show wind speed data versus date ----
d <- ggplot(data = obs.all.highrate,
            aes(x = DATE,
                y = WIND.SPD)) +
  geom_point(shape = ".") +
  facet_wrap(~ID,ncol = 3) +
  labs(x = "Year",
       y = "Wind Speed (m/s)") +
  theme_bw(base_size = 10, base_family = "")
print(d)

ggsave(file.path(data.dir,"figures","ScatterWindSpdObs.png"),
       scale = 1, width = 7,
       height = 3.5, units = c("in"),
       dpi = 300, limitsize = TRUE)