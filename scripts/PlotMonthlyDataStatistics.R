# Reshape the data ----
obs.monthly.melt <- cast(melt.list(obs.monthly,
                              id = c("USAFID","DATE")),
                         USAFID + DATE ~ variable)

obs.monthly.melt$ID <- paste(obs.monthly.melt$USAFID,
                             " (", obs.monthly.melt$NAME, ")",
                             sep="")

# note that we now have to fix some variables as well
obs.monthly.melt$TEMP.MIN <- as.numeric(as.character(obs.monthly.melt$TEMP.MIN))
obs.monthly.melt$TEMP.MEAN <- as.numeric(as.character(obs.monthly.melt$TEMP.MEAN))
obs.monthly.melt$TEMP.MAX <- as.numeric(as.character(obs.monthly.melt$TEMP.MAX))

obs.monthly.melt$WIND.SPD.MIN <- as.numeric(as.character(obs.monthly.melt$WIND.SPD.MIN))
obs.monthly.melt$WIND.SPD.MEAN <- as.numeric(as.character(obs.monthly.melt$WIND.SPD.MEAN))
obs.monthly.melt$WIND.SPD.MAX <- as.numeric(as.character(obs.monthly.melt$WIND.SPD.MAX))

# Plot the monthly temperature range ----
d <- ggplot(data = obs.monthly.melt,
            aes(x = DATE,
                y = TEMP.MEAN)) +
  geom_ribbon(aes(ymin = TEMP.MIN,
                  ymax = TEMP.MAX),
              color = "pink") +
  geom_line() + 
  facet_wrap(~ID, ncol = 3) +
  labs(x = "Date",
       y = "Monthly temperature range") +
  theme_bw(base_size = 10, base_family = "")
print(d)

ggsave(file.path(data.dir,"figures","ScatterTempRangeMonth.png"),
       plot = last_plot(),
       scale = 1, width = 7,
       height = 3.5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# Plot the monthly wind speed range ----
d <- ggplot(data = obs.monthly.melt,
            aes(x = DATE,
                y = WIND.SPD.MEAN)) +
  geom_ribbon(aes(ymin = WIND.SPD.MIN,
                  ymax = WIND.SPD.MAX),
              color = "pink") +
  geom_line() + 
  facet_wrap(~ID, ncol = 3) +
  labs(x = "Date",
       y = "Monthly wind speed range") +
  theme_bw(base_size = 10, base_family = "")
print(d)

ggsave(file.path(data.dir,"figures","ScatterWindSpdRangeMonth.png"),
       scale = 1, width = 7,
       height = 3.5, units = c("in"),
       dpi = 300, limitsize = TRUE)