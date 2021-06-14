library(rgdal);library(raster)

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Regions_shapefile"

#load shp
shp <- readOGR("Regions_reptiles_amphibians_359",dsn = wd_shp)

#visual check
plot(shp,col = rgb(0,0,0,0.2),border=NA)
