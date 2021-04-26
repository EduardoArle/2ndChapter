library(rgdal);library(raster)

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Mammals/DAMA"
setwd(wd_shp)

sps_list <- gsub(".shp","",list.files(pattern = ".shp"))

t <- readOGR(sps_list[1],dsn=wd_shp)

plot(t)

t2 <- readOGR(sps_list[2],dsn=wd_shp)

t2$Landmass
t2@data

plot(t2)
plot(t2[1,],add=T,col="red")
plot(t2[2,],add=T,col="blue")
plot(t2[3,],add=T,col="orange")
plot(t2[4,],add=T,col="magenta")
plot(t2[5,],add=T,col="green")
plot(t2[6,],add=T,col="purple")

