#list packages
library(rgdal);library(raster);library(maptools);library(rgeos)

#list wds
wd_shp_ants <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants/Bentity2_shapefile_fullres/Bentity2_shapefile_fullres"
wd_GRIIS_shp <- "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp" 
wd_shp_amph <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Regions_shapefile"
wd_shp_birds <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/Shapefile"
wd_specific_issues <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/Specific_shp_issues"
wd_data_spiders <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders"
wd_indonesia_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/IDN_adm"
wd_brazil_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/BRA_adm"
wd_nz_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/NZL_adm"
wd_rus_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/RUS_adm"
wd_zaf <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/ZAF_adm"
wd_pyf <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/PYF_adm"
wd_png <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/PNG_adm"

#read checkist table
setwd(wd_data_spiders)
table <- read.csv("alien spiders Wolfgang Nentwig.csv")

#load ants shp
shp_ants <- readOGR("Bentity2_shapefile_fullres", dsn = wd_shp_ants)

#load GRIIS shp
shp_griis <- readOGR("GRIIS_ISO3", dsn = wd_GRIIS_shp)

#load amphibian shapefile
shp_amph <- readOGR("Regions_reptiles_amphibians", dsn = wd_shp_amph)

#load bird shapefile
shp_birds <- readOGR("Shapefile_birds", dsn = wd_shp_birds)

#load my chile shp
shp_chile <- readOGR("Chile",dsn = wd_specific_issues)

#load New Zealand shp
shp_nz <- readOGR("NZL_adm1",dsn = wd_nz_shp)

#load indonesia shp
shp_indo <- readOGR("IDN_adm1",dsn = wd_indonesia_shp)

#load russia shp
shp_rus <- readOGR("RUS_adm1",dsn = wd_rus_shp)

#load South African shapefile
shp_zaf <- readOGR("ZAF_adm1", dsn = wd_zaf)

#load French Polynesia shapefile
shp_pyf <- readOGR("PYF_adm1", dsn = wd_pyf)

#load Papua New Guinea regions
shp_png <- readOGR("PNG_adm1", dsn = wd_png)

#list regions in the ants shapefile
ant_regs <- sort(unique(shp_ants$BENTITY2_N))

#list regions in the birds shapefile
bird_regs <- sort(unique(shp_birds$GAVIARg))

#list regions in the griis shapefile
griis_regs <- sort(unique(shp_griis$Region))

#list amphibian regions
amph_regs <- sort(unique(shp_amph$BENTITY2_N))


#make a country list and check one by one if it makes sense to have subnational
#level as region

regions <- sort(unique(table$X.8))

#### create new column in the checklist table to include the region names

table$SpiderRegion <- NA


#manually compile the shp solving case by case

i=1 #### empty rows, ignore

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

table2 <- table[-which(table$X.8 == ""),]


i=2 ### detailed spatial information missing, check later

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab


i=3 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(1))

shp_spiders <- a

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]


i=4 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]


i=5 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]


i=6 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]


i=7 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]


i=8 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]


i=9 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]


i=10 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]


i=11 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

##### Make sure it is BRAZILIAN AMAZONAS instead of Colombian one #####

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep(regions[i],ant_regs)

reg_i <- shp_ants[grep(regions[i],shp_ants$BENTITY2_N),]
reg_i <- reg_i[1,]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]


i=12 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=13 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=14 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=15 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% griis_regs
grep("Barbu",griis_regs)

reg_i <- shp_griis[grep("Barbu",shp_griis$Region),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=16 #region in Chile (special shapefile)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_chile[which(shp_chile$SpdrRgn == "Chile North"),]

#verify if feature is already in the shapefile

"Chile North" %in% shp_spiders$SpiderRegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0)

a$SpiderRegion = "Chile North"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Chile North"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=17 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

#### Include Argentina Distrito Federal data into Buenos Aires province

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Buenos",ant_regs)

reg_i <- shp_ants[grep("Buenos",shp_ants$BENTITY2_N),]

#verify if feature is already in the shapefile

"Buenos Aires" %in% shp_spiders$SpiderRegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = "Buenos Aires"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Buenos Aires"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=18 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=19 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=20 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

# Manually separate ascension from St Helena and Tristan da Cunha

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% griis_regs
grep(regions[i],griis_regs)

reg_i <- shp_griis[grep(regions[i],shp_griis$Region),]

reg_i
plot(reg_i)

#cut the island

b <- as(extent(-14.5, -14.25, -8, -7.8), 'SpatialPolygons')
reg_i <- crop(reg_i,b)

plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=21 #region in Chile (special shapefile)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_chile[which(shp_chile$SpdrRgn == "Chile North"),]

#verify if feature is already in the shapefile

"Chile North" %in% shp_spiders$SpiderRegion

#if yes

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Chile North"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=22 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=23  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=24 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep(regions[i],ant_regs)

reg_i <- shp_ants[grep(regions[i],shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=25  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=26  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=27  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=28  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=29  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp


###  Extra space in region's name

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab


#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Baja California Sur"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=30 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Balear",ant_regs)

reg_i <- shp_ants[grep("Balear",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=31 #### region taken from Indonesia shapefile

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% shp_indo$NAME_1

reg_i <- shp_indo[which(shp_indo$NAME_1 == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=32 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=33 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=34  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=35  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=36  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=37  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=38  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=39  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=40 #region in Chile (special shapefile)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_chile[which(shp_chile$SpdrRgn == "Chile Central"),]

#verify if feature is already in the shapefile

"Chile Central" %in% shp_spiders$SpiderRegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0)

a$SpiderRegion = "Chile Central"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Chile Central"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=41 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

#separate Bioko Island from Mainland Equatorial Guinea

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% griis_regs
grep("Equatorial",griis_regs)

reg_i <- shp_griis[grep("Equatorial",shp_griis$Region),]

reg_i
plot(reg_i)

b <- as(extent(8.2, 9.2, 3, 3.8), 'SpatialPolygons')
reg_i <- crop(reg_i,b)
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=42 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=43 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=44 #### Borneo here refers to the Malaysian part only (I suppose...)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% griis_regs
grep("Malaysia",griis_regs)

reg_i <- shp_griis[grep("Malaysia",shp_griis$Region),]

b <- as(extent(107, 121, -2, 10), 'SpatialPolygons')
reg_i <- crop(reg_i,b)

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

row.names(a)
row.names(shp_spiders)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=45 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Bosnia",ant_regs)

reg_i <- shp_ants[grep("Bosnia",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=46 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=47 #### region in Brazil - use separated shapefile because the ants one has 
# somehow fused the Distrito Federal into Minas and Goiás. Mais ils font n'importe quoi!!!

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

shp_brazil <- readOGR("BRA_adm1",dsn=wd_brazil_shp,
                      use_iconv=TRUE, encoding="UTF-8")

reg_i <- shp_brazil[grep("Distrito Federal",shp_brazil$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=48  #### "Brazil Northeast" quite vague... Check later if those species
#are represented in lower levels

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab


i=49 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=50 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs

#verify if feature is already in the shapefile

regions[i] %in% shp_spiders$SpiderRegion

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=51  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=52 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=53 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=54 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=55 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Canary",ant_regs)

reg_i <- shp_ants[grep("Canary",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=56 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Canary is."

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=57 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Cape Verde",ant_regs)

reg_i <- shp_ants[grep("Cape Verde",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=58 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Caroline",ant_regs)

reg_i <- shp_ants[grep("Caroline",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=59 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=60 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

##### Russia is divided in a very unorthodox way 
##### Deal with it manually...

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#load European Russia shp

eur_rus <- readOGR("European Russia",dsn = wd_specific_issues)

reg_i <- eur_rus[which(eur_rus$SpdrRgn == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=61 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=62 #### use New Zealand shapefile

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% shp_nz$NAME_1
grep("Chatham",shp_nz$NAME_1)

reg_i <- shp_nz[grep("Chatham",shp_nz$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=63 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=64 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=65 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

####  Decision included the the province of Sichuan

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[grep("Sichuan",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = "Sichuan"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Sichuan"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=66 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=67 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=68 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=69 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=70 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=71 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

### ERROR IN THE NAME

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab


#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Comoros"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=72 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=73 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=74 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Cook",ant_regs)

reg_i <- shp_ants[grep("Cook",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=75  #region in Chile (special shapefile)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_chile[which(shp_chile$SpdrRgn == "Chile Central"),]

#verify if feature is already in the shapefile

"Chile Central" %in% shp_spiders$SpiderRegion

#if yes

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Chile Central"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=76 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Cordoba",ant_regs)

reg_i <- shp_ants[grep("Cordoba",shp_ants$BENTITY2_N),]
reg_i <- reg_i[2,]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=77 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=78 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Corsica",ant_regs)

reg_i <- shp_ants[grep("Corsica",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=79 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=80 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=81 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=82  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=83  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=84  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=85  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=86  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=87  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=88 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=89 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=90 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

##### Russia is divided in a very unorthodox way 
##### Deal with it manually...

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#load European Russia shp

eur_rus <- readOGR("European Russia",dsn = wd_specific_issues)

reg_i <- eur_rus[which(eur_rus$SpdrRgn == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=91 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Easter",ant_regs)

reg_i <- shp_ants[grep("Easter",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=92  #### region taken from amphibians shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% amph_regs

reg_i <- shp_amph[which(shp_amph$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=93  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=94  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=95  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=96  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=97  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=98  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=99  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=100  #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


#### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=101 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=102 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=103 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=104 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=105 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Faro",ant_regs)

reg_i <- shp_ants[grep("Faro",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=106 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=107#### region taken from amphibians shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% amph_regs

reg_i <- shp_amph[which(shp_amph$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=108 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=109 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=110 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Gabon",ant_regs)

reg_i <- shp_ants[grep("Gabon",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=111 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Galap",ant_regs)

reg_i <- shp_ants[grep("Galap",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=112 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Gambia",ant_regs)

reg_i <- shp_ants[grep("Gambia",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=113 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=114 #### region taken from amphibians shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% amph_regs

reg_i <- shp_amph[which(shp_amph$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=115 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Georgia",ant_regs)

reg_i <- shp_ants[grep("Georgia",shp_ants$BENTITY2_N),]
reg_i <- reg_i[1,]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = "Georgia US"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Georgia US"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=116 #### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=117 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=118 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp


#Decision to include Gilbert Islands in Kiribati

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Kiribati",ant_regs)

reg_i <- shp_ants[grep("Kiribati",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = "Kiribati"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Kiribati"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=119 #### region in Brazil - use separated shapefile because the ants one has 
# somehow fused the Distrito Federal into Minas and Goiás. Mais ils font n'importe quoi!!!

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

shp_brazil <- readOGR("BRA_adm1",dsn=wd_brazil_shp,
                      use_iconv=TRUE, encoding="UTF-8")

reg_i <- shp_brazil[which(shp_brazil$NAME_1 == "Goiás"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=120 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
"United Kingdom" %in% ant_regs

reg_i <- shp_ants[grep("United Kingdom",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=121 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=122 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=123 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp


#### GEORGIA COUNTRY 

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Georgia",ant_regs)

reg_i <- shp_ants[grep("Georgia",shp_ants$BENTITY2_N),]
reg_i <- reg_i[2,]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=124 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=125 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=126 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=127 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=128 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=129 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=130 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=131 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=132 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=133 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=134 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=135 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

### repeated region with extra space

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Hainan"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=136 #### region taken from amphibians shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% amph_regs

reg_i <- shp_amph[which(shp_amph$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=137 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=138 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Hawaii",ant_regs)

reg_i <- shp_ants[grep("Hawaii",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=139 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=140 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=141 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=142 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=143 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=144 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

### Honshu is divided in regions in the ant shapefile, join them

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == "Tohoku" |
                        shp_ants$BENTITY2_N == "Kanto" |
                        shp_ants$BENTITY2_N == "Chubu" |
                        shp_ants$BENTITY2_N == "Kinki" |
                        shp_ants$BENTITY2_N == "Chugoku"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=145 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=146 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=147 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=148 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=149 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=150 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=151 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=152 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=153 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=154 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=155 ### region taken from birds shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

toupper(regions[i]) %in% bird_regs
grep("NEW GUINEA",bird_regs)

reg_i <- shp_birds[grep("NEW GUINEA",shp_birds$GAVIARg),]

reg_i
plot(reg_i)
reg_i <- reg_i[1,]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=156 ### Russian Oblast

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_rus[which(shp_rus$NAME_1 == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=157 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=158 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=159 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=160 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=161 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
"Java" %in% ant_regs

reg_i <- shp_ants[grep("Java",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=162 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=163 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=164 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=165 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=166 ### region taken from birds shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

toupper(regions[i]) %in% bird_regs

reg_i <- shp_birds[which(shp_birds$GAVIARg == toupper(regions[i])),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=167 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=168 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=169 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=170 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=171 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=172 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Kermadec",griis_regs)

reg_i <- shp_ants[grep("Kermadec",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=173 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Kyrg",ant_regs)

reg_i <- shp_ants[grep("Kyrg",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=174 ### Kiribati is already in the shapefile

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Kiribati"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=175 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Crete",ant_regs)

reg_i <- shp_ants[grep("Crete",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=176 #### region taken from amphibians shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% amph_regs

reg_i <- shp_amph[which(shp_amph$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=177 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=178 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=179 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=180 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=181 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=182 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=183 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=184 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=185 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=186 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Howe",ant_regs)

reg_i <- shp_ants[grep("Howe",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=187 #region in Chile (special shapefile)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_chile[which(shp_chile$SpdrRgn == "Chile South"),]

#verify if feature is already in the shapefile

"Chile South" %in% shp_spiders$SpiderRegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0)

a$SpiderRegion = "Chile South"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Chile South"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=188 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=189  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=190  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=191  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=192  ### Wrong name for Madagascar

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Madagascar"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=193  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Madeira",ant_regs)

reg_i <- shp_ants[grep("Madeira",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=194  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=195   #region in Chile (special shapefile)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_chile[which(shp_chile$SpdrRgn == "Chile South"),]

#verify if feature is already in the shapefile

"Chile South" %in% shp_spiders$SpiderRegion

#if yes

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Chile South"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=196 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=197 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=198 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=199 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=200 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Maluku",ant_regs)

reg_i <- shp_ants[grep("Maluku",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=201 ###  Include "Manihiki" in the Cook Islands

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Cook Is",ant_regs)

grep("Cook",shp_spiders$SpiderRegion)
shp_spiders$SpiderRegion[66]

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Cook Is."

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=202  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=203  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=204  ### south african isolated islands

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_zaf[grep("isolated",shp_zaf$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=205 ### French Polynesia island groups

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_pyf[grep("Marqu",shp_pyf$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=206 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Marsh",ant_regs)

reg_i <- shp_ants[grep("Marsh",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=207 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=208 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=209 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=210 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=211 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=212 #region in Chile (special shapefile)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_chile[which(shp_chile$SpdrRgn == "Chile Central"),]

#verify if feature is already in the shapefile

"Chile Central" %in% shp_spiders$SpiderRegion

#if yes

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Chile Central"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=213 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=214 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Mexico",ant_regs)

reg_i <- shp_ants[grep("Mexico",shp_ants$BENTITY2_N),]

reg_i$BENTITY2_N
reg_i <- reg_i[1,]
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=215 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=216 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=217 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=218 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=219 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=220 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=221 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=222 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=223 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=224 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=225 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=226 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=227 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=228 #### region taken from amphibians shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% amph_regs

reg_i <- shp_amph[which(shp_amph$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=229 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=230 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=231 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp


### island groups are separated in the ant shp, join them in one feature

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == "Satsunan Islands" |
                        shp_ants$BENTITY2_N == "Okinawa" |
                        shp_ants$BENTITY2_N == "Daito Islands" |
                        shp_ants$BENTITY2_N == "Yaeyama Islands" |
                        shp_ants$BENTITY2_N == "Senkaku Islands"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=232 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% griis_regs
grep("Minor",griis_regs)

reg_i <- shp_griis[grep("Minor",shp_griis$Region),]

reg_i

#clip only Navassa Island

b <- as(extent(-75.05, -74.9, 18.37, 18.44), 'SpatialPolygons')
reg_i <- crop(reg_i,b)

plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=233 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=234 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=235 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=236 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=237 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=238 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=239 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=240 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

grep("Newfound",ant_regs)

reg_i <- shp_ants[grep("Newfound",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Newfoundland"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=241 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=242 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=243 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=244 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=245 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=246 #### use New Zealand shapefile

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% shp_nz$NAME_1

reg_i <- shp_nz[which(shp_nz$NAME_1 == "Northland" |
                      shp_nz$NAME_1 == "Auckland" |
                      shp_nz$NAME_1 == "Waikato" |
                      shp_nz$NAME_1 == "Bay of Plenty" |
                      shp_nz$NAME_1 == "Gisborne" |
                      shp_nz$NAME_1 == "Hawke's Bay" |
                      shp_nz$NAME_1 == "Manawatu-Wanganui" |
                      shp_nz$NAME_1 == "Taranaki" |
                      shp_nz$NAME_1 == "Wellington"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a <- gSimplify(a,tol = 0.01)

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=247 #### use New Zealand shapefile

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% shp_nz$NAME_1

reg_i <- shp_nz[which(shp_nz$NAME_1 == "Marlborough" |
                        shp_nz$NAME_1 == "Nelson" |
                        shp_nz$NAME_1 == "West Coast" |
                        shp_nz$NAME_1 == "Southland" |
                        shp_nz$NAME_1 == "Otago" |
                        shp_nz$NAME_1 == "Canterbury"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a <- gSimplify(a,tol = 0.01)

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=248 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

#Newfoundland is already in the shapefile

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=249 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=250 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

#  Andaman and Nicobar are separated in this database

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Nicobar",ant_regs)

reg_i <- shp_ants[grep("Nicobar",shp_ants$BENTITY2_N),]

#cut the Nicobar islands from the Andamans

b <- as(extent(92.5, 94.1, 6.5, 9.5), 'SpatialPolygons')
reg_i <- crop(reg_i,b)

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=251 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=252 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=253 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Norfolk",ant_regs)

reg_i <- shp_ants[grep("Norfolk",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=254 #### region taken from amphibians shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% amph_regs
grep("West Province",amph_regs)

reg_i <- shp_amph[grep("West Province",shp_amph$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=255 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=256 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=257 ##### Russia is divided in a very unorthodox way 
##### Deal with it manually...

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#load European Russia shp

eur_rus <- readOGR("European Russia",dsn = wd_specific_issues)

reg_i <- eur_rus[which(eur_rus$SpdrRgn == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=258 ### autonomous region of Bougainville (get from Papua New Guinea shp)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_png[which(shp_png$NAME_1 == "Bougainville"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=259 ### region taken from birds shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

toupper(regions[i]) %in% bird_regs

reg_i <- shp_birds[which(shp_birds$GAVIARg == toupper(regions[i])),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=260 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

#####  Separate the Northern Mariana Islands from GUAM

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Mariana",ant_regs)

reg_i <- shp_ants[grep("Mariana",shp_ants$BENTITY2_N),]

#cut the islands

b <- as(extent(144, 147, 13.85, 21.04), 'SpatialPolygons')
reg_i <- crop(reg_i,b)

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=261 #### region taken from amphibians shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% amph_regs

reg_i <- shp_amph[which(shp_amph$BENTITY2_N == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=262 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=263  ##### Russia is divided in a very unorthodox way 
##### Deal with it manually...

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#load European Russia shp

eur_rus <- readOGR("European Russia",dsn = wd_specific_issues)

reg_i <- eur_rus[which(eur_rus$SpdrRgn == regions[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=264 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=265 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=266 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=267 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

# Region name is wrong, fix

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Nunavu",ant_regs)

reg_i <- shp_ants[grep("Nunavu",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = "Nunavut"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Nunavut"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=268 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

#REPEATED REGION  

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=269  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=270  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Ogasa",ant_regs)

reg_i <- shp_ants[grep("Ogasa",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=271  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=272  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=273  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=274  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=275  ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=276 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=277 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=278 ### Main Island Papua New Guinea (get from Papua New Guinea shp)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_png[which(shp_png$NAME_1 != "Bougainville" &
                       shp_png$NAME_1 != "West New Britain" &
                       shp_png$NAME_1 != "East New Britain" &
                       shp_png$NAME_1 != "New Ireland" &
                       shp_png$NAME_1 != "Manus"),]

reg_i
plot(reg_i)

reg_i <- gSimplify(reg_i,tol=0.01)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=279 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=280 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=281 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=282 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=283  #### Peninsula Malaysia (Exclude Borneo)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% griis_regs
grep("Malaysia",griis_regs)

reg_i <- shp_griis[grep("Malaysia",shp_griis$Region),]

b <- as(extent(98, 106, 0, 7.5), 'SpatialPolygons')
reg_i <- crop(reg_i,b)

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

row.names(a)
row.names(shp_spiders)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=284 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=285 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=286 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=287 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

#name issu, Peru again

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Peru"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=288 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=289 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Phoenix",ant_regs)

reg_i <- shp_ants[grep("Phoenix",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=290 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=291 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Pitcairn",ant_regs)

reg_i <- shp_ants[grep("Pitcairn",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=292 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=293 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Puerto",ant_regs)

reg_i <- shp_ants[grep("Puerto",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = "Puerto Rico"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Puerto Rico"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i =294 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=295 ### Russian Oblast

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_rus[grep("Primor",shp_rus$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=296 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs
grep("Edward",ant_regs)

reg_i <- shp_ants[grep("Edward",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=297 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

### Repeated region, name issue

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=298 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=299 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=300 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=301 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% griis_regs
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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=302 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=303 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=304 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=305 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=306 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=307 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=308 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=309 #Sabah is part of the Malaysian part of Borneo

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Borneo"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=310 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=311 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=312 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=313 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=314 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=315 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=316 #region in Chile (special shapefile)

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

reg_i <- shp_chile[which(shp_chile$SpdrRgn == "Chile Central"),]

#verify if feature is already in the shapefile

"Chile Central" %in% shp_spiders$SpiderRegion

#if yes

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Chile Central"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=317 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=318 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=319 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

grep(regions[i],griis_regs)

reg_i <- shp_griis[grep(regions[i],shp_griis$Region),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = "Sao Tome and Principe"

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Sao Tome and Principe"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=320 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

grep("Sard",ant_regs)

reg_i <- shp_ants[grep("Sard",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=321 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=322 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=323 ### decision to include Selvagens Islands in Madeira

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

regions[i] %in% ant_regs


#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- "Madeira"

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=324 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=325 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=326 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=327 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=328 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=329 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=330 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=331 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=332 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=333 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

reg_tab

grep("Sici",ant_regs)

reg_i <- shp_ants[grep("Sici",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=334 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=335 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=336 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

plot(shp_spiders)

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders


i=337 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=338 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


i=339 ### region taken from ants shapefile, attribute table must be modified
########## to include the feature in the shp

regions[i]

reg_tab <- table[which(table$X.8 == regions[i]),]

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

a$SpiderRegion = regions[i]

a <- spChFIDs(a,paste(nrow(shp_spiders)+1))

shp_spiders <- spRbind(shp_spiders,a)

shp_spiders

shp_spiders$SpiderRegion

#plot(shp_spiders,border=NA,col="gray70")

#include region name in table

table2$SpiderRegion[which(table2$X.8 == regions[i])] <- regions[i]

table2$SpiderRegion[which(table2$X.8 == regions[i])] 

t <- table2[which(!is.na(table2$SpiderRegion)),]

shp_back <- shp_spiders 


################################################################

wd <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/Shapefile"

writeOGR(shp_spiders,layer = "Shapefile_spiders",drive = "ESRI Shapefile",
          dsn = wd)

shp_spiders <- readOGR("Shapefile_spiders",dsn = wd, 
                     use_iconv=TRUE, encoding="UTF-8")

names(shp_spiders) <- "SpiderRegion"

setwd(wd)

#write.csv(table2,"Table2.csv")

table2 <- read.csv("Table2.csv")

unique(table2$GAVIARegion)
