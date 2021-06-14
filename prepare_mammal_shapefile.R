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
