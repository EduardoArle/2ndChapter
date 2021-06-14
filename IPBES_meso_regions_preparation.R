library(raster);library(rgdal);library(rgeos)

wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/ipbes_regions_subregions_shape_1.1"
wd_out <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/ipbes_regions_mesoregions_shape"

#load IPBES shapefie
ipbes <- readOGR("IPBES_Regions_Subregions2",dsn=wd_IPBES)

#create new column for "meso region"
ipbes$Meso_Region <- NA

#populate the column with "meso regions"
ipbes$Meso_Region[which(ipbes$Sub_Region == "Antarctica")] <-
  "Antarctica"
  
ipbes$Meso_Region[which(ipbes$Sub_Region == "Caribbean")] <-
  "North America"
  
ipbes$Meso_Region[which(ipbes$Sub_Region == "Central Africa")] <-
  "Africa"

ipbes$Meso_Region[which(ipbes$Sub_Region == "Central and Western Europe")] <-
  "Central and Western Europe" 

ipbes$Meso_Region[which(ipbes$Sub_Region == "Western Asia")] <-
  "Asia"  

ipbes$Meso_Region[which(ipbes$Sub_Region == "South America")] <-
  "South America"  

ipbes$Meso_Region[which(ipbes$Sub_Region == "Eastern Europe")] <-
  "Eastern Europe"  

ipbes$Meso_Region[which(ipbes$Sub_Region == "Oceania")] <-
  "Oceania"

ipbes$Meso_Region[which(ipbes$Sub_Region == "Central Africa")] <-
  "Africa"

ipbes$Meso_Region[which(ipbes$Sub_Region == "West Africa")] <-
  "Africa"

ipbes$Meso_Region[which(ipbes$Sub_Region == "Mesoamerica")] <-
  "North America"

ipbes$Meso_Region[which(ipbes$Sub_Region == "South-East Asia")] <-
  "Asia" 

ipbes$Meso_Region[which(ipbes$Sub_Region ==  "North America")] <-
  "North America"

ipbes$Meso_Region[which(ipbes$Sub_Region == "East Africa and adjacent islands")] <-
  "Africa" 

ipbes$Meso_Region[which(ipbes$Sub_Region == "North Africa")] <-
  "Africa"

ipbes$Meso_Region[which(ipbes$Sub_Region == "Central Asia" )] <-
  "Asia"

ipbes$Meso_Region[which(ipbes$Sub_Region == "South Asia" )] <-
  "Asia"

ipbes$Meso_Region[which(ipbes$Sub_Region == "North-East Asia" )] <-
  "Asia"

ipbes$Meso_Region[which(ipbes$Sub_Region == "Southern Africa")] <-
  "Africa"

#simplify polygons
ipbes2 <- gSimplify(ipbes,tol=0.1,topologyPreserve = T)

#put data back making a spatialPolygonDataFrame
ipbes3 <- SpatialPolygonsDataFrame(ipbes2,ipbes@data)

ipbes3 <- ipbes

#plot to check if it works and save the figure

plot(ipbes3, border=NA)
     
plot(ipbes3[which(ipbes3$Meso_Region == "North America"),],
     col = "red", add = T, border = NA)

plot(ipbes3[which(ipbes3$Meso_Region == "Africa"),],
     col = "gold", add = T, border = NA)

plot(ipbes3[which(ipbes3$Meso_Region == "South America"),],
     col = "darkgreen", add = T, border = NA)

plot(ipbes3[which(ipbes3$Meso_Region == "Central and Western Europe"),],
     col = "blue", add = T, border = NA)

plot(ipbes3[which(ipbes3$Meso_Region == "Eastern Europe"),],
     col = "orange", add = T, border = NA)

plot(ipbes3[which(ipbes3$Meso_Region == "Oceania"),],
     col = "purple", add = T, border = NA)

plot(ipbes3[which(ipbes3$Meso_Region == "Asia"),],
     col = "green", add = T, border = NA)

plot(ipbes3[which(ipbes3$Meso_Region == "Antarctica"),],
     col = "gray70", add = T, border = NA)

#calculate area or each region
ipbes3$Area_km2 <- NA

for(i in 1:nrow(ipbes3))
{
  ipbes3$Area_km2[i] <- area(ipbes3[i,])/1000000
  print(i)
}

#save new shapefile
writeOGR(ipbes3, dsn = wd_out, driver = "ESRI Shapefile", 
         layer = "IPBES_mesoregions")




unique(ipbes$Meso_Region)
