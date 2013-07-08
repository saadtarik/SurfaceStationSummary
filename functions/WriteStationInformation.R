WriteStationInformation <- function(stations.all,
                                    projectID,
                                    data.dir,
                                    country.code){
  ## find the unique station identifiers  
  st.out <- data.frame("USAF" = stations.all$USAF)
  st.out$WBAN <- stations.all$WBAN
  st.out$Name <- stations.all$NAME
  st.out$Latitude <- stations.all$LAT
  st.out$Longitude <- stations.all$LON
  st.out$Elevation <- stations.all$ELEV
  st.out$Begin <- stations.all$BEGIN
  st.out$End <- stations.all$END
  
  ## Write information on stations to latex
  tab<-xtable(st.out, 
              label = paste("tab:",
                            country.code,
                            "_Stations",
                            sep=""),
              caption= paste("Stations with a ",
                             country.code, 
                             " country code", 
                             sep=""), 
              align=c("r","r","r","r","r","r","r","r","r"),
              digits = c(0,0,0,0,2,2,0,0,0))
  print(tab,
        file=file.path(data.dir,
                       projectID,
                       "tex",
                       paste("Stations.tex",
                             sep="")),
        append=F,
        tabular.environment="longtable",
        table.placement = "h",
        caption.placement="top",
        include.rownames = FALSE,
        floating=F,
        hline.after=NULL,
        add.to.row=list(pos=list(-1,0, nrow(tab)),
                        command=c(
                          '\\toprule\n',
                          '\\midrule\n',
                          '\\bottomrule\n')))
}