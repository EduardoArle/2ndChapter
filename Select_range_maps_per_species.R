library(rgdal);library(raster)

#repeat for each group clumped in the same SHP

#GYMNOPHIONA

wd_ranges <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Range maps/GYMNOPHIONA"
wd_out <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Range maps/GYMNOPHIONA_species"
  
ranges <- readOGR("GYMNOPHIONA",dsn = wd_ranges)

sps_list <- unique(ranges$binomial)

for(i in 1:length(sps_list))
{
  sps_range <- ranges[which(ranges$binomial == sps_list[i]),]
  writeOGR(sps_range,layer = sps_list[i], driver = "ESRI Shapefile",
           dsn = wd_out)
  print(i)
}

#CAUDATA

wd_ranges <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Range maps/CAUDATA"
wd_out <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Range maps/CAUDATA_species"

ranges <- readOGR("CAUDATA",dsn = wd_ranges)

sps_list <- unique(ranges$binomial)

for(i in 1:length(sps_list))
{
  sps_range <- ranges[which(ranges$binomial == sps_list[i]),]
  writeOGR(sps_range,layer = sps_list[i], driver = "ESRI Shapefile",
           dsn = wd_out)
  print(i)
}

#ANURA

wd_ranges <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Range maps/ANURA"
wd_out <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Range maps/ANURA_species"

ranges <- readOGR("ANURA",dsn = wd_ranges)

sps_list <- unique(ranges$binomial)

for(i in 1:length(sps_list))
{
  sps_range <- ranges[which(ranges$binomial == sps_list[i]),]
  writeOGR(sps_range,layer = sps_list[i], driver = "ESRI Shapefile",
           dsn = wd_out)
  print(i)
}

plot(world,border=NA,col="gray70")
plot(sps_range,add=T,col="red",border=NA)

library(rworldmap)
world <- getMap(res="low")
