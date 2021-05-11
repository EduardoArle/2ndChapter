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

write.csv(table,"GloNAF_mesoRegion_with_gaps.csv",row.names = F)

############################ Scrap ##########################

#visualisation for manual assessment of regions

plot(world,col="grey80",border=NA)

plot(region_centre[which(region_centre$OBJIDsic == 84),],add = T, col = "red",
     pch = 19, cex = 0.5) #channel islands

plot(region_centre[which(region_centre$OBJIDsic == 91),],add = T, col = "red",
     pch = 19, cex = 0.5) #cocos islands islands

plot(region_centre[which(region_centre$OBJIDsic == 130),],add = T, col = "red",
     pch = 19, cex = 0.5) #española

plot(region_centre[which(region_centre$OBJIDsic == 151),],add = T, col = "red",
     pch = 19, cex = 0.5) #French Frigate Shoals

plot(region_centre[which(region_centre$OBJIDsic == 158),],add = T, col = "darkgreen",
     pch = 19, cex = 0.5) #Genovesa

plot(region_centre[which(region_centre$OBJIDsic == 202),],add = T, col = "darkgreen",
     pch = 19, cex = 0.5) #Isabela

plot(region_centre[which(region_centre$OBJIDsic == 211),],add = T, col = "darkgreen",
     pch = 19, cex = 0.5) #Juan Fernandez

plot(region_centre[which(region_centre$OBJIDsic == 227),],add = T, col = "orange",
     pch = 19, cex = 0.5) #Kermadec Islands

plot(region_centre[which(region_centre$OBJIDsic == 804),],add = T, col = "green",
     pch = 19, cex = 0.5) #cook islands

plot(region_centre[which(region_centre$OBJIDsic == 547),],add = T, col = "green",
     pch = 19, cex = 0.5) #beacon island

plot(region_centre[which(region_centre$OBJIDsic == 548),],add = T, col = "green",
     pch = 19, cex = 0.5) #Coral Sea Islands Territory

plot(region_centre[which(region_centre$OBJIDsic == 552),],add = T, col = "green",
     pch = 19, cex = 0.5) #Helms Island

plot(region_centre[which(region_centre$OBJIDsic == 553),],add = T, col = "red",
     pch = 19, cex = 0.5) #Hummock Island

plot(region_centre[which(region_centre$OBJIDsic == 356),],add = T, col = "red",
     pch = 19, cex = 0.5) #Pearl & Hermes

plot(region_centre[which(region_centre$OBJIDsic == 555),],add = T, col = "green",
     pch = 19, cex = 0.5) #Long Island, Houtman Abrolhos

plot(region_centre[which(region_centre$OBJIDsic == 577),],add = T, col = "orange",
     pch = 19, cex = 0.5) #Pigeon Island

plot(region_centre[which(region_centre$OBJIDsic == 608),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Seagull Island

plot(region_centre[which(region_centre$OBJIDsic == 619),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Esperance Plains

plot(region_centre[which(region_centre$OBJIDsic == 651),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Bahamas

plot(region_centre[which(region_centre$OBJIDsic == 671),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Trindade e Martim Vaz

plot(region_centre[which(region_centre$OBJIDsic == 712),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Aisen

plot(region_centre[which(region_centre$OBJIDsic == 728),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Capitán Prat

plot(region_centre[which(region_centre$OBJIDsic == 760),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Última Esperanza

plot(region_centre[which(region_centre$OBJIDsic == 816),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Comoros

plot(region_centre[which(region_centre$OBJIDsic == 822),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Cayman Islands

plot(region_centre[which(region_centre$OBJIDsic == 847),],add = T, col = "blue",
     pch = 19, cex = 0.5) #Crozet Islands

plot(region_centre[which(region_centre$OBJIDsic == 853),],add = T, col = "green",
     pch = 19, cex = 0.5) #Diego Garcia

plot(region_centre[which(region_centre$OBJIDsic == 874),],add = T, col = "red",
     pch = 19, cex = 0.5) #Gonave

plot(region_centre[which(region_centre$OBJIDsic == 895),],add = T, col = "red",
     pch = 19, cex = 0.5) #Lakshadweep

plot(region_centre[which(region_centre$OBJIDsic == 903),],add = T, col = "green",
     pch = 19, cex = 0.5) #Puducherry

plot(region_centre[which(region_centre$OBJIDsic == 930),],add = T, col = "green",
     pch = 19, cex = 0.5) #Canton, Phoenix Islands

plot(region_centre[which(region_centre$OBJIDsic == 936),],add = T, col = "orange",
     pch = 19, cex = 0.5) #Tabueran

plot(region_centre[which(region_centre$OBJIDsic == 987),],add = T, col = "orange",
     pch = 19, cex = 0.5) #Ailinginae Atoll

plot(region_centre[which(region_centre$OBJIDsic == 988),],add = T, col = "red",
     pch = 19, cex = 0.5) #Eniwetok, Mashall Islands

plot(region_centre[which(region_centre$OBJIDsic == 1075),],add = T, col = "red",
     pch = 19, cex = 0.5) #Desertas

plot(region_centre[which(region_centre$OBJIDsic == 1078),],add = T, col = "red",
     pch = 19, cex = 0.5) #Salvage Islands

plot(region_centre[which(region_centre$OBJIDsic == 1260),],add = T, col = "red",
     pch = 19, cex = 0.5) #Tonga

plot(region_centre[which(region_centre$OBJIDsic == 1284),],add = T, col = "red",
     pch = 19, cex = 0.5) #Johnston Island

plot(region_centre[which(region_centre$OBJIDsic == 1340),],add = T, col = "red",
     pch = 19, cex = 0.5) #Marion Island

plot(region_centre[which(region_centre$OBJIDsic == 1344),],add = T, col = "green",
     pch = 19, cex = 0.5) #Prince Edward Island, South Africa

plot(region_centre[which(region_centre$OBJIDsic == 1024),],add = T, col = "green",
     pch = 19, cex = 0.5) #New Caledonia

plot(region_centre[which(region_centre$OBJIDsic == 546),],add = T, col = "orange",
     pch = 19, cex = 0.5) #Ashmore Reef

y <- over(region_centre[which(region_centre$OBJIDsic == 124),],ipbes)

setwd(wd_results)

test <- read.csv("GloNAF_mesoRegion.csv")

unique(is.na(test$Meso_region))

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
