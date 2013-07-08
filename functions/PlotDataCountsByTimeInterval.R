PlotDataCountsByTimeInterval <- function(counts,
                                         interval = "overall"){
  switch(interval,
         overall = PlotDataCountsOverall(obs.all),
         year = PlotDataCountsByYear(counts),
         month = PlotDataCountsByMonth(counts),
         hour = PlotDataCountsByHour(counts))         
}

PlotDataCountsOverall <- function(counts){
  is.single.station <- (NROW(unique(counts$ID)) == 1)  
  melt.counts <- melt(counts, id.vars = c("ID"))
  plot.data.counts <- ggplot(data = melt.counts,
                             aes(y = value,
                                 fill = variable)) +
    geom_bar(stat = "identity",
             position="dodge")
  if (!is.single.station){
    plot.data.counts <- plot.data.counts + facet_wrap(~ID)
  } 
  plot.data.counts <- plot.data.counts + 
    scale_fill_brewer(name = "Variable",
                      labels = c("Wind direction",
                                 "Wind speed",
                                 "Temperature",
                                 "Barometric\n pressure")) + 
    labs(x = "",
         y = "Number of valid data")
  print(plot.data.counts)
  
  return(plot.data.counts)
}

PlotDataCountsByYear <- function(counts){
  is.single.station <- (NROW(unique(counts$ID)) == 1)    
  melt.counts <- melt(counts, id.vars = c("ID","YR"))
  plot.data.counts <- ggplot(data = melt.counts,
                             aes(x = YR,
                                 y = value,
                                 fill = variable)) +
    geom_bar(stat = "identity",
             position="dodge")
  if (!is.single.station){
    plot.data.counts <- plot.data.counts + facet_wrap(~ID)
  } 
  plot.data.counts <- plot.data.counts + 
    scale_fill_brewer(name = "Variable",
                      labels = c("Wind direction",
                                 "Wind speed",
                                 "Temperature",
                                 "Barometric\n pressure")) + 
    labs(x = "Year",
         y = "Number of valid data") +
    theme(legend.position="top")
  print(plot.data.counts)
  
  return(plot.data.counts)
}

PlotDataCountsByMonth <- function(counts){
  is.single.station <- (NROW(unique(counts$ID)) == 1)      
  melt.counts <- melt(counts, id.vars = c("ID","M"))
  plot.data.counts <- ggplot(data = melt.counts,
                             aes(x = M,
                                 y = value,
                                 fill = variable)) +
    geom_bar(stat = "identity",
             position="dodge")
  if (!is.single.station){
    plot.data.counts <- plot.data.counts + facet_wrap(~ID)
  } 
  plot.data.counts <- plot.data.counts + 
    scale_fill_brewer(name = "Variable",
                      labels = c("Wind direction",
                                 "Wind speed",
                                 "Temperature",
                                 "Barometric\n pressure")) + 
    labs(x = "Month",
         y = "Number of valid data") + 
    theme(legend.position="top")
  print(plot.data.counts)
  
  return(plot.data.counts)
}

PlotDataCountsByHour <- function(counts){
  is.single.station <- (NROW(unique(counts$ID)) == 1)      
  melt.counts <- melt(counts, id.vars = c("ID","HR"))
  plot.data.counts <- ggplot(data = melt.counts,
                             aes(x = HR,
                                 y = value,
                                 fill = variable)) +
    geom_bar(stat = "identity",
             position="dodge")
  if (!is.single.station){
    plot.data.counts <- plot.data.counts + facet_wrap(~ID)
  } 
  plot.data.counts <- plot.data.counts +     
    scale_fill_brewer(name = "Variable",
                      labels = c("Wind direction",
                                 "Wind speed",
                                 "Temperature",
                                 "Barometric\n pressure")) + 
    labs(x = "Hour of the Day (GMT)",
         y = "Number of valid data") +
    theme(legend.position="top")
  print(plot.data.counts)
  
  return(plot.data.counts)
}