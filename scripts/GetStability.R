

stability <- data.frame("e.sat.30" = rep(NA,length(obs.all.df$AT.30)))

# interpolate the aire temperature to the intermediate levels
stability$AT.60.interp <- apply(cbind(obs.all.df$AT.50),
                                obs.all.df$AT.70)),
1,
function(x) round(approx(c(50,70),x,60, 
                         method = "linear")$y,
                  digits = 2))
# Saturation vapor pressures ----
e.sat <- function(T){
  if (T > 0){
    A <- 7.5
    B <- 237.3
  }else{
    A <- 9.5
    B <- 265.5
  }
  e.sat <- 6.11 * 10 ^ ((T*A)/(T+B))
}

stability$e.sat.30 <- apply(as.data.frame(obs.all$AT.30), 1, e.sat)
stability$e.sat.40 <- apply(as.data.frame(obs.all$AT.40), 1, e.sat)
stability$e.sat.50 <- apply(as.data.frame(obs.all$AT.50), 1, e.sat)
stability$e.sat.60 <- apply(as.data.frame(stability$AT.60.interp), 1, e.sat)
stability$e.sat.70 <- apply(as.data.frame(obs.all$AT.70), 1, e.sat)
stability$e.sat.100 <- apply(as.data.frame(obs.all$AT.100), 1, e.sat)


# relative humidity ----
stability$phi.33 <- as.data.frame(obs.all$RH.33)
stability$phi.50 <- as.data.frame(obs.all$RH.50)
stability$phi.90 <- as.data.frame(obs.all$RH.90)
# interpolate the RH to the intermediate levels
stability$phi.40.interp <- apply(cbind(stability$phi.33, stability$phi.50),
                                 1,
                                 function(x) round(approx(c(33,50),x,40, 
                                                          method = "linear")$y,
                                                   digits = 2))
stability$phi.60.interp <- apply(cbind(stability$phi.50, stability$phi.90),
                                 1,
                                 function(x) round(approx(c(50,90),x,60, 
                                                          method = "linear")$y,
                                                   digits = 2))
stability$phi.70.interp <- apply(cbind(stability$phi.50, stability$phi.90),
                                 1,
                                 function(x) round(approx(c(50,90),x,70, 
                                                          method = "linear")$y,
                                                   digits = 2))
# assume the 0 m level is saturated so we can get the 30 m level value
stability$phi.30.extrap <- apply(cbind(stability$phi.33, 
                                       rep(100,length(stability$phi.33))),
                                 1,
                                 function(x) round(approx(c(33,0),x,30,
                                                          method = "linear")$y,
                                                   digits = 2))
# assume the 100 m level is the same as the 90 m level
stability$phi.100.extrap <- stability$phi.90

# Vapor pressure ----
stability$e.30 <- (stability$phi.30 / 100) * stability$e.sat.30
stability$e.40 <- (stability$phi.40.interp / 100) * stability$e.sat.40
stability$e.50 <- (stability$phi.50 / 100) * stability$e.sat.50
stability$e.60 <- (stability$phi.60.interp / 100) * stability$e.sat.60
stability$e.70 <- (stability$phi.70.interp / 100) * stability$e.sat.70
stability$e.90 <- (stability$phi.90 / 100) * stability$e.sat.90

# Specific humidity ----
# requires local pressure, but the active sensor changes as one or other drops out.
# are they the same sensor? If so, what height are they at?

d1 <- ggplot(data = obs.all.df,
             aes(x = Date)) + 
  geom_point(aes(y = AP.90)) +
  geom_point(aes(y = AP.20)) +
  labs(x = "Date",
       y = "P [mBar]",
       title = "Barometric pressure") +
  theme_bw(base_size = 10, base_family = "")

d2 <- ggplot(data = obs.all.df,
             aes(x = Date,
                 y = AP.90-AP.20)) + 
  geom_point() +
  labs(x = "Date",
       y = "P(z = 90) - P(z = 20) [mBar]",
       title = "Pressure difference") + 
  theme_bw(base_size = 10, base_family = "")

png(filename = file.path("figures","AirPressure.png"),
    res = 300,
    width = 6,
    height = 4,
    unit = "in")
multiplot(d1, d2, cols=1)
dev.off()

