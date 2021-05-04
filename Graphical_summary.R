library(rgdal);library(raster)

#paths
wd_results <- "C:/Users/ca13kute/Documents/2nd_Chapter/Results/GloNAF"
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GloNAF_modified_shp"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/ipbes_regions_mesoregions_shape"

#load resuts table
setwd(wd_results)

table <- read.csv("GloNAF.csv")

#load GloNAF shapefile
shp <- readOGR("GloNAF_modified",dsn=wd_shp)

plot(shp)

#load IPBES shapefie
ipbes <- readOGR("IPBES_mesoregions",dsn=wd_IPBES)

#get mesoregion of each checklist region
region_centre <- table
coordinates(region_centre) <- ~LON+LAT

proj4string(region_centre) <- CRS(paste(crs(ipbes))) #indicate projection

meso <- over(region_centre,ipbes)

#include meso_region info in the table

table$Meso_region <- meso$Mes_Rgn

#save table and solve the regions that where not assessed to any meso region
#manually

setwd(wd_results)

write.csv(table,"GloNAF_mesoRegion.csv")

############################ Scrap ##########################

#identify the GloNAF regions that where not assessed to any IPBES region

nas <- which(is.na(meso$Mes_Rgn))
na_meso <- table[is.na(meso$Mes_Rgn),]

#solve the regions that were not over any IPBES regions (probably smalls islands)

small <- ipbes[which(ipbes$Are_km2 <= 1000),] #select small regions
small2 <- gBuffer(small, width = 4, byid = T) #make a buffer

meso2 <- over(region_centre[is.na(meso$Mes_Rgn),],small2)

meso_t <- meso
meso_t[is.na(meso_t$Mes_Rgn),] <- meso2

table(is.na(meso2$Mes_Rgn))

plot(small2,add=F)

t <- region_centre[nas,]
plot(t,add=T,col="red",cex=.4,pch=19)

par(mar=c(0,0,0,0))
plot(ipbes)

head(ipbes@data)

aruba <- ipbes[1,]
brazil <- ipbes[which(ipbes$Area == "Brazil"),]
plot(ipbes[-nas],)

area(aruba)/1000000

plot(aruba,add=T,col="red")
plot(brazil,add=T,col="red")


plot(region_centre[nas,],add=F)
sort(region_centre[nas,]$region)
