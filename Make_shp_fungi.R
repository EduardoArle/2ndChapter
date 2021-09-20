#list packages
library(rgdal);library(raster);library(maptools);library(rgeos);
library(rworldmap)

#list wds
wd_shp_ants <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants/Bentity2_shapefile_fullres/Bentity2_shapefile_fullres"
wd_GRIIS_shp <- "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp" 
wd_shp_australia <- "C:/Users/ca13kute/Documents/2nd_Chapter/Fungi/Shp_stuff/AUS_adm"
wd_shp_russia <- "C:/Users/ca13kute/Documents/2nd_Chapter/Fungi/Shp_stuff/RUS_adm"

#read checkist table
setwd(wd_data_fungi)
table <- read.csv("Alien_fungi.csv", encoding="UTF-8")

#load ants shp
shp_ants <- readOGR("Bentity2_shapefile_fullres", dsn = wd_shp_ants)

#load GRIIS shp
shp_griis <- readOGR("GRIIS_ISO3", dsn = wd_GRIIS_shp)

#load Australia shp
shp_australia <- readOGR("AUS_adm1", dsn = wd_shp_australia)

#oad Russia shp
shp_russia <- readOGR("RUS_adm1", dsn = wd_shp_russia)


#list regions in the ants shapefile
ant_regs <- sort(unique(shp_ants$BENTITY2_N))

#list regions in the birds shapefile
bird_regs <- sort(unique(shp_birds$GAVIARg))

#list regions in the Australia shapefile
australia_regs <- sort(unique(shp_australia$NAME_1))

#list regions in the Russia shapefile
russia_regs <- sort(unique(shp_russia$NAME_1))

#load world map
world <- getMap()


#make a country list and check one by one if it makes sense to have subnational
#level as region

regions <- sort(unique(table$locality))

#### create new column in the checklist table to include the region names

table$FungiRegion <- NA

#manually compile the shp solving case by case

i=1  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(1))

shp_fungi <- a

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=2 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=3 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=4 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=5 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=6 # AUSTRALIA IS ONE OF THE SIX COUNTRIES INDICATED IN THE ARTICLE AS BEING
#FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "AUSTRALIA", AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE SUBNATIONAL LEVEL


i=7 #### region taken from Australia shapefile

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% australia_regs

reg_i <- shp_australia[which(shp_australia$NAME_1 == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=8 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=9 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=10 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=11 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=12 #One entry in "Bonin Islands", will be ignored


i=13 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=14  # BRAZIL IS ONE OF THE SIX COUNTRIES INDICATED IN THE ARTICLE AS BEING
#FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "BRAZIL", AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE SUBNATIONAL LEVEL


i=15 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=16 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=17 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=18 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=19 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=20 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=21  # CANADA IS ONE OF THE SIX COUNTRIES INDICATED IN THE ARTICLE AS BEING
#FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "CANADA", AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE SUBNATIONAL LEVEL


i=22 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=23 # CENTRAL EUROPE IS NOT A COUNTRY NOR A REGION IN ONE OF THE SIX COUNTRIES 
#INDICATED IN THE ARTICLE AS BEING FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "CENTRAL EUROPE", AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE NATIONAL / SUBNATIONAL LEVEL


i=24 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=25  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=26 # CHINA IS ONE OF THE SIX COUNTRIES INDICATED IN THE ARTICLE AS BEING
#FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "CHINA", AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE SUBNATIONAL LEVEL


i=27 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=28 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=29 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("Congo",griis_regs)

reg_i <- shp_griis[grep("Congo",shp_griis$Region),]

reg_i
reg_i <- reg_i[1,]
reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=30 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=31 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=32 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("Ivo",ant_regs)

reg_i <- shp_ants[grep("Ivo",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=33 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=34 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=35 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=36 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=37 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=38 #region already represented by 29th position. Name issue.

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- "Congo, the Democratic Republic of the"

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=39 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=40 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=41 # EASTERN AUSTRALIA IS  NOT A COUNTRY NOR A REGION IN ONE OF THE SIX 
#COUNTRIES INDICATED IN THE ARTICLE AS BEING FURTHER DIVIDED INTO SMALLER ADM 
#REGIONS.
#DECISION: IGNORE ENTRIES AS "EASTERN AUSTRALIA", AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE NATIONAL / SUBNATIONAL LEVEL


i=42 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=43  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("Espirito",ant_regs)

reg_i <- shp_ants[grep("Espirito",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=44  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=45  # EUROPE IS NOT A COUNTRY NOR A REGION IN ONE OF THE SIX COUNTRIES 
#INDICATED IN THE ARTICLE AS BEING FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "EUROPE", AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE NATIONAL / SUBNATIONAL LEVEL


i=46 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("Falkland",ant_regs)

reg_i <- shp_ants[grep("Falkland",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=47   #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=48   #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=49  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=50 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=51  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=52  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=53  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=54  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=55  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=56 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=57 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=58 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=59 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=60 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=61 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=62 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=63 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=64  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=65 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=66 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=67 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=68 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=69  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=70 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=71  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=72  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=73 #### region taken from Russia shapefile

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% russia_regs

reg_i <- shp_russia[which(shp_russia$NAME_1 == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=74 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=75 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=76 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=77 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("Macedonia",ant_regs)

reg_i <- shp_ants[grep("Macedonia",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=78 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=79 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=80 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=81 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=82 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=83 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=84  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=85 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=86 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=87  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=88 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=89  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=90  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=91  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=92  #### region taken from Russia shapefile

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% russia_regs

reg_i <- shp_russia[which(shp_russia$NAME_1 == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=93 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=94 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=95 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=96 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=97 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=98 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=99 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=100 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=101 #### region taken from Australia shapefile

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% australia_regs

reg_i <- shp_australia[which(shp_australia$NAME_1 == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=102 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=103  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


104 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=105 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=106 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=107 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=108 # NORTH AFRICA IS NOT A COUNTRY NOR A REGION IN ONE OF THE SIX COUNTRIES 
#INDICATED IN THE ARTICLE AS BEING FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "NORTH AFRICA", AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE NATIONAL / SUBNATIONAL LEVEL


i=109 # NORTH AMERICA IS NOT A COUNTRY NOR A REGION IN ONE OF THE SIX COUNTRIES 
#INDICATED IN THE ARTICLE AS BEING FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "NORTH AMERICA, AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE NATIONAL / SUBNATIONAL LEVEL


i=110 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=111 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("Mariana",ant_regs)

reg_i <- shp_ants[grep("Mariana",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=112 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=113 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=114 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=115 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=116 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=117 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=118 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=119 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=120 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=121 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=122 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=123 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=124 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=125 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=126 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=127 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=128 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=129 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("Congo",ant_regs)

reg_i <- shp_ants[grep("Congo",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=130  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("union",griis_regs)

reg_i <- shp_griis[grep("union",shp_griis$Region),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=131 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=132 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=133 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=134 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=135 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=136  # RUSSIA IS ONE OF THE SIX COUNTRIES INDICATED IN THE ARTICLE AS BEING
#FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "RUSSIA", AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE SUBNATIONAL LEVEL


i=137 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=138 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=139 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=140 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=141 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=142 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=143 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=144 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=145 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=146 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=147 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=148 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=149 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=150 # SOUTH AMERICA IS NOT A COUNTRY NOR A REGION IN ONE OF THE SIX COUNTRIES 
#INDICATED IN THE ARTICLE AS BEING FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "SOUTH AMERICA, AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE NATIONAL / SUBNATIONAL LEVEL


i=151 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=152 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=153 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=154 # SOUTHERN SOUTH AMERICA IS NOT A COUNTRY NOR A REGION IN ONE OF THE SIX COUNTRIES 
#INDICATED IN THE ARTICLE AS BEING FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "SOUTHERN SOUTH AMERICA, AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE NATIONAL / SUBNATIONAL LEVEL


i=155 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=156#### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=157 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=158 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=159 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=160 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=161 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("Tanzania",ant_regs)

reg_i <- shp_ants[grep("Tanzania",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=162 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=163 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=164 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=165 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=166 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=167 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=168 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=169 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=170 # UNITED STATES IS ONE OF THE SIX COUNTRIES INDICATED IN THE ARTICLE AS BEING
#FURTHER DIVIDED INTO SMALLER ADM REGIONS.
#DECISION: IGNORE ENTRIES AS "UNITED STATES, AND CHECK WHETHER THESE SPS 
#HAVE BEEN REPRESENTED IN THE SUBNATIONAL LEVEL


i=171 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=172 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=173 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=174 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=175 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=176  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=177 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=178 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=179 #### region taken from Australia shapefile

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

grep("Western Australia",australia_regs)

reg_i <- shp_australia[grep("Western Australia",shp_australia$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=180 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=181 #already in the shapefile as "west australia"

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

table$FungiRegion[which(table$locality == regions[i])] <- "West Australia"

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=182 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=183 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=184 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=185 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


i=186 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$locality == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$FungiRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_fungi)+1))

shp_fungi <- spRbind(shp_fungi,a)

shp_fungi

shp_fungi$FungiRegion

#plot(shp_fungi)

#include region name in table

table$FungiRegion[which(table$locality == regions[i])] <- regions[i]

table$FungiRegion[which(table$locality == regions[i])] 

t <- table[which(!is.na(table$FungiRegion)),]


#check overlap

plot(shp_fungi,col = rgb(0,0,0,0.2),border=NA)

nrow(table)
nrow(t)

missing <- table[which(is.na(table$FungiRegion)),]
################################################


################################################################

wd <- "C:/Users/ca13kute/Documents/2nd_Chapter/Fungi/Shapefile"

writeOGR(shp_fungi,layer = "Shapefile_fungi",drive = "ESRI Shapefile",
         dsn = wd)

shp_fungi <- readOGR("Shapefile_fungi",dsn = wd, 
                       use_iconv=TRUE, encoding="UTF-8")

names(shp_fungi) <- "FungiRegion"

setwd(wd)

#write.csv(table,"Table.csv")

table <- read.csv("Table.csv")




