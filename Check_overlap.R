library(rgdal);library(raster)

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants/Bentities2_23_Oct_2015.qgs"

#load shp
shp <- readOGR("Bentities2_23_Oct_2015.qgs",dsn = wd_shp)

#visual check
plot(shp,col = rgb(0,0,0,0.2),border=NA)
