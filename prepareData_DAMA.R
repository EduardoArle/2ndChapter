library(rgdal);library(raster)

wd_mapping <- "C:/Users/ca13kute/Documents/Soup/World_simple_map"

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Mammals/DAMA"
setwd(wd_shp)

#load world map and frame
setwd(wd_mapping)
world <- readRDS("Simple_world")
frame <- readRDS("World_frame")

sps_list <- gsub(".shp","",list.files(pattern = ".shp"))

t <- readOGR(sps_list[1],dsn=wd_shp)

setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Mammals/Examples of the data")

write.csv(t,paste0(sps_list[1],".csv"),row.names = F)

plot(t)

plot(world,col="gray70",border=NA)
plot(frame,add=T)
tb <- spTransform(t,CRS(proj4string(world)))
plot(tb,add=T,col="red",border=NA)

t2 <- readOGR(sps_list[2],dsn=wd_shp)

write.csv(t2,paste0(sps_list[2],".csv"),row.names = F)

plot(world,col="gray70",border=NA)
plot(frame,add=T)
t2b <- spTransform(t2,CRS(proj4string(world)))
plot(t2b,add=T,col="red",border=NA)

t2$Landmass
t2@data

plot(t2)
plot(t2[1,],add=T,col="red")
plot(t2[2,],add=T,col="blue")
plot(t2[3,],add=T,col="orange")
plot(t2[4,],add=T,col="magenta")
plot(t2[5,],add=T,col="green")
plot(t2[6,],add=T,col="purple")

