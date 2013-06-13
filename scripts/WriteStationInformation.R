## find the unique station identifiers
st.out <- data.frame(cbind(USAFID =target.list$USAF))
st.out$WBAN <- target.list$WBAN
st.out$Name <- target.list$NAME
st.out$Latitude <- target.list$LAT
st.out$Longitude <- target.list$LON
st.out$Elevation <- target.list$ELEV
st.out$Begin <- target.list$BEGIN
st.out$End <- target.list$END

## Write information on stations to latex
tab<-xtable(st.out, 
            label = paste("tab:",
                          target.country.code,
                          "_Stations",
                          sep=""),
            caption= paste("Stations with a ",
                           target.country.code, 
                           " country code", 
                           sep=""), 
            align=c("r","r","r","r","r","r","r","r","r"),
            digits = c(0,0,0,0,2,2,0,0,0))
print(tab,
      file=file.path(data.dir,
                     "tex",
                     paste(target.country.code,
                           "_Stations.tex",
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

# tidy up
rm(st.out)