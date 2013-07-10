WriteStationInformationTable <- function(stations,
                                         projectID,
                                         data.dir,
                                         country.code){
  ## find the unique station identifiers  
  st.out <- data.frame("USAF" = stations$USAF)
  st.out$WBAN <- stations$WBAN
  st.out$Name <- stations$NAME
  st.out$Latitude <- stations$LAT
  st.out$Longitude <- stations$LON
  st.out$Elevation <- stations$ELEV
  st.out$Begin <- stations$BEGIN
  st.out$End <- stations$END
  
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
                       paste("StationsTable.tex",
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

WriteStationInformationList <- function(stations,
                                        projectID,
                                        data.dir,
                                        country.code){
  
  # create a connection to an output file
  tex.stationinfo.list.file.to <- file.path(data.dir,
                                            projectID,
                                            "tex",
                                            paste("StationsList.tex",
                                                  sep=""))
  file.remove(tex.stationinfo.list.file.to)  
  zz <- file(tex.stationinfo.list.file.to, "at")    
  for (ID in stations$ID){
    writeLines(text = paste("\\subimport{../figures/",ID,"/}{StationInfo.tex}",sep = ""),
              con = zz)
  }    
  close(zz)
}

CreateStationSummaryPage <- function(stations,
                                     year.min.count,
                                     data.dir,
                                     projectID,
                                     code.dir){
  # read the station information template file from the repository
  tex.stationinfo.from = file.path(code.dir,"TexTemplates","StationInfo.tex")  
  for (ID in stations$ID){
    station <- stations[stations$ID == ID,]    
    
    tex.stationinfo.file.to = file.path(data.dir,projectID,"figures",ID,"StationInfo.tex")    
    tex.stationinfo.file.in <- readChar(con = tex.stationinfo.from,
                                        nchars = file.info(tex.stationinfo.from)$size)
    # edit this file to replace some of the stuff in the file
    tex.stationinfo.file.out <- gsub(pattern = "#NAME",
                                     replacement = station$NAME,
                                     x = tex.stationinfo.file.in)
    tex.stationinfo.file.out <- gsub(pattern = "#WBAN",
                                     replacement = station$WBAN,
                                     x = tex.stationinfo.file.out)
    tex.stationinfo.file.out <- gsub(pattern = "#USAF",
                                     replacement = station$USAF,
                                     x = tex.stationinfo.file.out)
    tex.stationinfo.file.out <- gsub(pattern = "#LONG",
                                     replacement = station$LONG,
                                     x = tex.stationinfo.file.out)
    tex.stationinfo.file.out <- gsub(pattern = "#LAT",
                                     replacement = station$LAT,
                                     x = tex.stationinfo.file.out)
    tex.stationinfo.file.out <- gsub(pattern = "#ELEV",
                                     replacement = station$ELEV,
                                     x = tex.stationinfo.file.out)    
    tex.stationinfo.file.out <- gsub(pattern = "#NYEARS",
                                     replacement = year.min.count,
                                     x = tex.stationinfo.file.out)  
    tex.stationinfo.file.out <- gsub(pattern = "#BEGIN",
                                     replacement = ifelse(is.na(station$BEGIN),"unkown",station$BEGIN),
                                     x = tex.stationinfo.file.out)
    tex.stationinfo.file.out <- gsub(pattern = "#END",
                                     replacement = ifelse(is.na(station$END),"unkown",station$END),
                                     x = tex.stationinfo.file.out)
    # write it out    
    writeLines(text = tex.stationinfo.file.out,
              con = tex.stationinfo.file.to)    
  }
}