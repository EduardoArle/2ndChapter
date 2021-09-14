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



nrow(table)
nrow(t)


################################################


