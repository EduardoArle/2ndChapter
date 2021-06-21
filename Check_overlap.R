library(rgdal);library(raster)

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Regions_shapefile"
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GloNAF_modified_shp"

#load shp
shp <- readOGR("GloNAF_modified",dsn = wd_shp)

#visual check
plot(shp,col = rgb(0,0,0,0.2),border=NA)

