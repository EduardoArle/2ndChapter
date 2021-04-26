library(rgdal);library(raster)

#paths
wd_results <- "C:/Users/ca13kute/Documents/2nd_Chapter/Results/GloNAF"
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GloNAF_modified_shp"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/ipbes_regions_subregions_shape_1.1"

#load resuts table
setwd(wd_results)

table <- read.csv("GloNAF.csv")

#load GloNAF shapefile
shp <- readOGR("GloNAF_modified",dsn=wd_shp)

plot(shp)

#load IPBES shapefie
ipbes <- readOGR("IPBES_Regions_Subregions2",dsn=wd_IPBES)

head(ipbes@data)
