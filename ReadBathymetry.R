#Read waterbody bathymetry data 
#i.e. tables of cumulative %area vs depth for use in O2 indicator

ReadBathymetry<-function(){
  df<-read.table("data/WB_bathymetry.txt", fileEncoding = "UTF-8", sep="\t", 
                 stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")
  df$WB<-paste0(df$WaterbodyID," ",df$WB_name)
  return(df)
}
