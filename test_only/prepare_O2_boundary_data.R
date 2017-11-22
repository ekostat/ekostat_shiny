
library(tidyverse)

dfb1 <- data.frame(WB_name="Byfjorden", WaterbodyID="SE582000-115270", area_pct = 1:100, depth = c(1:40/4,10+1:20/2,20+1:30/3,30+1:10))
dfb2 <- data.frame(WB_name="Gullmarn centralbassäng", WaterbodyID="SE581700-113000", area_pct = 1:100, depth = c(1:40*2,80+1:20,100+1:30,130+1:10))
WB_bathymetry <- bind_rows(dfb1,dfb2)

BndHypoxicArea<-data.frame(WB_name="",WaterbodyID="",RefCond=0,H.G=0,G.M=0,M.P=0,P.B=0,Worst=0,stringsAsFactors=FALSE)
BndHypoxicArea[1,]=c("Byfjorden","SE582000-115270",100,68,64,60,40,0)
BndHypoxicArea[2,]=c("Gullmarn centralbassäng","SE581700-113000",100,82,53,24,16,0)

write.table(WB_bathymetry,file="data/WB_bathymetry.txt",row.names=F,na="",sep=";")
write.table(BndHypoxicArea,file="data/BoundsHypoxicArea.txt",row.names=F,na="",sep=";")



# test<-read.table("data/BoundsHypoxicArea.txt", fileEncoding = "UTF-8", sep=";", 
#                stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")