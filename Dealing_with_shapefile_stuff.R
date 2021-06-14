library(rgdal);library(raster);library(rgeos)

wd_clip <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Specific_shp_issues"

nf_lab <- readOGR("Newfoundland_Labrador",dsn=wd_clip)

clipping <- readOGR("Clipping", dsn=wd_clip)


newfoundland <- gIntersection(nf_lab,clipping)
labrador <- gDifference(nf_lab,clipping)

newfoundland$BENTITY2_N <- "Newfoundland"
newfoundland$POINT <- "NA"

labrador$BENTITY2_N <- "Labrador"
labrador$POINT <- "NA"


writeOGR(newfoundland,layer="Newfoundland",dsn=wd_clip,drive="ESRI Shapefile")
writeOGR(labrador,layer="Labrador",dsn=wd_clip,drive="ESRI Shapefile")


plot(labrador)
plot(newfoundland)

plot(nf_lab)
plot(clipping)

?gDifference
