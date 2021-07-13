#list packages
library(rgdal);library(raster);library(maptools);library(rgeos)

#list wds
wd_shp_ants <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants/Bentity2_shapefile_fullres/Bentity2_shapefile_fullres"
wd_GRIIS_shp <- "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp" 
wd_antilles <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/ANT_adm"
wd_shp_amph <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Regions_shapefile"
wd_shp_india <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/IND_adm"
wd_specific_issues <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/Specific_shp_issues"
wd_data_GAVIA <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA"

#load ants shp
shp_ants <- readOGR("Bentity2_shapefile_fullres", dsn = wd_shp_ants)

#load GRIIS shp
shp_griis <- readOGR("GRIIS_ISO3", dsn = wd_GRIIS_shp)

#load amphibian shapefile
shp_amph <- readOGR("Regions_reptiles_amphibians", dsn = wd_shp_amph)

#load shp India
shp_india <- readOGR("IND_adm1", dsn = wd_shp_india)

#list regions in the table 
tab_regs <- sort(unique(table$Region))

#list regions in the ants shapefile
ant_regs <- sort(unique(shp_ants$BENTITY2_N))

#list regions in the griis shapefile
griis_regs <- sort(unique(shp_griis$Region))

#list amphibian regions
amph_regs <- sort(unique(shp_amph$BENTITY2_N))

#list indian regions
india_regs <- sort(unique(shp_india$NAME_1))



#load birds table
setwd(wd_data_GAVIA)
table <- read.csv("GAVIA_main_data_table.csv")

#select only introductions listed as "Established" or "Breeding"
table2 <- table[which(table$StatusCat == "Established" |
                        table$StatusCat == "Breeding"),]

#make a country list and check one by one if it makes sense to have subnational
#level as region

countries <- sort(unique(table2$CountryName))


#### create new column in the checklist table to include the region names

table2$GAVIARegion <- NA


#manually compile the shp solving case by case

i=1 #### entries with just location description 
#(solve in them in the end according to the shapefile)

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab$LocationDescription

i=2 

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

reg_i <- shp_ants[which(toupper(shp_ants$BENTITY2_N) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- a

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=3 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% griis_regs
grep("land Islands",griis_regs)
griis_regs[c(1,80)]

reg_i <- shp_griis[which(shp_griis$Region == "Ã…land Islands"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in shapefile

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=4

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

reg_i <- shp_ants[which(toupper(shp_ants$BENTITY2_N) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=5 #Algeria, few entries with region detail, decision to aggregate to 
#country level

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

reg_i <- shp_ants[which(toupper(shp_ants$BENTITY2_N) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]



i=6 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=7

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

reg_i <- shp_ants[which(toupper(shp_ants$BENTITY2_N) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds 

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]


t <- table2[which(!is.na(table2$GAVIARegion)),]


i=8 

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

reg_i <- shp_ants[which(toupper(shp_ants$BENTITY2_N) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds 

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=9  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
grep(countries[i],toupper(griis_regs))
grep("ANGUILLA",toupper(griis_regs))

reg_i <- shp_griis[grep("ANGUILLA",toupper(shp_griis$Region)),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=10  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=11 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

##############  DECISION to join Antigua and Barbuda in one region,
#some species did not carry info on what island they were

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=12 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

##############  DECISION to consider Argentina as one region,
#some species did not carry info on what province they were

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=13 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=14 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=15

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

##############  DECISION to consider each Australian province as one region,
#very few species will be lost

aus <- sort(unique(count_tab$AreaName1))

j=1  #entries with empty province info solve later

aus[j]

aus_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == aus[j]),]

aus_tab

j=2 ### decision to include "Australian Capital Territory" in "New South Wales"

aus[j]

aus_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == aus[j]),]

aus_tab

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                  table2$AreaName1 == aus[j])] <- toupper("New South Wales")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=3

aus[j]

aus_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == aus[j]),]

aus_tab

aus[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == aus[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(aus[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == aus[j])] <- toupper(aus[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=4

aus[j]

aus_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == aus[j]),]

aus_tab

aus[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == aus[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(aus[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == aus[j])] <- toupper(aus[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=5

aus[j]

aus_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == aus[j]),]

aus_tab

aus[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == aus[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(aus[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == aus[j])] <- toupper(aus[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=6

aus[j]

aus_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == aus[j]),]

aus_tab

aus[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == aus[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(aus[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == aus[j])] <- toupper(aus[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=7

aus[j]

aus_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == aus[j]),]

aus_tab

aus[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == aus[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(aus[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == aus[j])] <- toupper(aus[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=8

aus[j]

aus_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == aus[j]),]

aus_tab

aus[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == aus[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(aus[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == aus[j])] <- toupper(aus[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=9

aus[j]

aus_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == aus[j]),]

aus_tab

aus[j] %in% amph_regs

reg_i <- shp_amph[which(shp_amph$BENTITY2_N == aus[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(aus[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == aus[j])] <- toupper(aus[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


i=16 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=17 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=18 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=19 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=20 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=21 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=22 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=23 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=24 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=25 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=26 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=27 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=28 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=29 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=30 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=31 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

##############  DECISION to consider Brazil as one region,
#most species did not carry info on what province they were

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=32 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=33 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
toupper(griis_regs)[grep("VIRGIN",toupper(griis_regs))]

reg_i <- shp_griis[which(toupper(shp_griis$Region) == "VIRGIN ISLANDS (BRITISH)"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=34 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=35 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=36 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=37 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=38 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=39

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

##############  DECISION to consider each Canadian province as one region,
# few species will be lost

can <- sort(unique(count_tab$AreaName1))

j=1  #entries with empty province info solve later

can[j]

can_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == can[j]),]

can_tab

j=2

can[j]

can_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == can[j]),]

can_tab

can[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == can[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(can[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == can[j])] <- toupper(can[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]

j=3

can[j]

can_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == can[j]),]

can_tab

can[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == can[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(can[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == can[j])] <- toupper(can[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=4

can[j]

can_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == can[j]),]

can_tab

can[j] %in% ant_regs
grep("Newfoundland",ant_regs)

reg_i <- shp_ants[grep("Newfoundland",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(can[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == can[j])] <- toupper(can[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=5

can[j]

can_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == can[j]),]

can_tab

can[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == can[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(can[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == can[j])] <- toupper(can[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=6

can[j]

can_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == can[j]),]

can_tab

can[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == can[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(can[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == can[j])] <- toupper(can[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=7

can[j]

can_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == can[j]),]

can_tab

can[j] %in% ant_regs
grep("Quebec",ant_regs)

reg_i <- shp_ants[grep("Quebec",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(can[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == can[j])] <- toupper(can[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=8

can[j]

can_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == can[j]),]

can_tab

can[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == can[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(can[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == can[j])] <- toupper(can[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


i=40 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=41 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=42 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=43 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=44 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

##############  DECISION to consider China as one region,
#some species did not carry info on what province they were

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=45 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=46 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
grep("COCOS",toupper(griis_regs))

reg_i <- shp_griis[grep("COCOS",toupper(shp_griis$Region)),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=47 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=48 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=49 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
griis_regs[grep("CONGO",toupper(griis_regs))]

reg_i <- shp_griis[which(toupper(shp_griis$Region) == "CONGO"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=50 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

countries[i] %in% toupper(griis_regs)
griis_regs[grep("CONGO",toupper(griis_regs))]

reg_i <- shp_griis[which(shp_griis$Region =="Democratic Republic of the Congo"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=51 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=52 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=53 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
grep("IVOIRE",toupper(griis_regs))


reg_i <- shp_griis[grep("IVOIRE",toupper(shp_griis$Region)),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=54 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=55 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=56 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=57 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=58 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=59 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=60 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=61 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=62 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=63 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=64 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=65 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=66 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=67 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=68 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=69 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
grep("FALK",toupper(griis_regs))

reg_i <- shp_griis[grep("FALK",toupper(shp_griis$Region)),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=70 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=71 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
grep("FIJI",toupper(griis_regs))

reg_i <- shp_griis[grep("FIJI",toupper(shp_griis$Region)),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=72 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=73 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=74 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=75 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=76 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=77 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=78 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=79 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=80 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=81 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=82 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=83 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=84 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=85 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=86 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=87 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=88 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=89 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=90 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=91 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=92 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
grep("HONG",toupper(griis_regs))


reg_i <- shp_griis[grep("HONG",toupper(shp_griis$Region)),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=93 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=94 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=95

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

##############  DECISION to consider each Indian province as one region,
#very few species will be lost

ind <- sort(unique(count_tab$AreaName1))

j=1  #entries with empty province info solve later

ind[j]

ind_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ind[j]),]

ind_tab

j=2 

ind[j]

ind_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ind[j]),]

ind_tab

ind[j] %in% india_regs
grep("Andaman",india_regs)

reg_i <- shp_india[grep("Andaman",shp_india$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ind[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ind[j])] <- toupper(ind[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=3 

ind[j]

ind_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ind[j]),]

ind_tab

ind[j] %in% india_regs
grep("Andhra",india_regs)

reg_i <- shp_india[grep("Andhra",shp_india$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ind[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ind[j])] <- toupper(ind[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=4

ind[j]

ind_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ind[j]),]

ind_tab

ind[j] %in% india_regs

reg_i <- shp_india[which(shp_india$NAME_1 == ind[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ind[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ind[j])] <- toupper(ind[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=5

ind[j]

ind_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ind[j]),]

ind_tab

ind[j] %in% india_regs
grep("Lakshadweep",india_regs)

reg_i <- shp_india[grep("Lakshadweep",shp_india$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ind[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ind[j])] <- toupper(ind[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=6

ind[j]

ind_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ind[j]),]

ind_tab

ind[j] %in% india_regs
grep("Maharashtra",india_regs)

reg_i <- shp_india[grep("Maharashtra",shp_india$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ind[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ind[j])] <- toupper(ind[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=7

ind[j]

ind_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ind[j]),]

ind_tab

ind[j] %in% india_regs
grep("Tamil",india_regs)

reg_i <- shp_india[grep("Tamil",shp_india$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ind[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ind[j])] <- toupper(ind[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=8

ind[j]

ind_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ind[j]),]

ind_tab

ind[j] %in% india_regs
grep("Bengal",india_regs)

reg_i <- shp_india[grep("Bengal",shp_india$NAME_1),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ind[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ind[j])] <- toupper(ind[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


i=96 #### region prepared manually from Indonesia DIVA GIS regions
countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

##############  DECISION to consider each Indonesian island or archipelago 
#as one region,
#very few species will be lost

indo <- sort(unique(count_tab$AreaName1))

j=1  #entries with empty province info solve later

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == indo[j]),]

indo_tab

j=2 

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == indo[j]),]

indo_tab

indo[j] %in% ant_regs
grep("Sumatra",ant_regs)

reg_i <- shp_ants[grep("Sumatra",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper("Sumatra")

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("Sumatra")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=3 

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

indo[j] %in% ant_regs
grep("Sumatra",ant_regs)

reg_i <- shp_ants[grep("Sumatra",shp_ants$BENTITY2_N),]

#verify if feature is already in the shapefile

toupper("Sumatra") %in% shp_birds$GAVIARegion

#if not

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("Sumatra")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=4

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

indo[j] %in% ant_regs
grep("Sumatra",ant_regs)

reg_i <- shp_ants[grep("Sumatra",shp_ants$BENTITY2_N),]

#verify if feature is already in the shapefile

toupper("Sumatra") %in% shp_birds$GAVIARegion

#if not

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("Sumatra")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=5

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

#prepare region manually

reg_i <- readOGR("Lesser Sunda Islands (IDN)",dsn = wd_specific_issues)

#if not
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

a <- reg_i
names(a) <- "GAVIARegion"

#make shapefile

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("Lesser Sunda Islands (IDN)")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=6

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

#prepare region manually

reg_i <- readOGR("New Guinea (IDN)",dsn = wd_specific_issues)

#if not
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

a <- reg_i
names(a) <- "GAVIARegion"

#make shapefile

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("New Guinea (IDN)")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=7

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

indo[j] %in% ant_regs
grep("Sumatra",ant_regs)

reg_i <- shp_ants[grep("Sumatra",shp_ants$BENTITY2_N),]

#verify if feature is already in the shapefile

toupper("Sumatra") %in% shp_birds$GAVIARegion

#if yes

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("Sumatra")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=8

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

indo[j] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == indo[j]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(indo[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper(indo[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=9

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

#prepare region manually

reg_i <- readOGR("Kalimantan",dsn = wd_specific_issues)

#if not
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

a <- reg_i
names(a) <- "GAVIARegion"

#make shapefile

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper(indo[j])


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=10

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

#prepare region manually

reg_i <- readOGR("Maluku",dsn = wd_specific_issues)

#if not
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

a <- reg_i
names(a) <- "GAVIARegion"

#make shapefile

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper(indo[j])


j=11

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

reg_i <- readOGR("Lesser Sunda Islands (IDN)",dsn = wd_specific_issues)

#verify if feature is already in the shapefile

toupper("Lesser Sunda Islands (IDN)") %in% shp_birds$GAVIARegion

#if yes

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("Lesser Sunda Islands (IDN)")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=12

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

reg_i <- readOGR("Lesser Sunda Islands (IDN)",dsn = wd_specific_issues)

#verify if feature is already in the shapefile

toupper("Lesser Sunda Islands (IDN)") %in% shp_birds$GAVIARegion

#if yes

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("Lesser Sunda Islands (IDN)")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=13

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

indo[j] %in% ant_regs
grep("Sumatra",ant_regs)

reg_i <- shp_ants[grep("Sumatra",shp_ants$BENTITY2_N),]

#verify if feature is already in the shapefile

toupper("Sumatra") %in% shp_birds$GAVIARegion

#if yes

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("Sumatra")


t <- table2[which(!is.na(table2$GAVIARegion)),]


j=14 #Sabah is in Malaysia! Ignore the one sps entry (Passer montanus,
#also listed in Sabah as Malaysia).

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab


j=15  #Sarawak is in Malaysia! Ignore the one entry (Acridotheres tristis,
#NOT included in Sarawak as Malaysia).

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab


j=16

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

#prepare region manually

reg_i <- readOGR("Sulawesi",dsn = wd_specific_issues)

#if not
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

a <- reg_i
names(a) <- "GAVIARegion"

#make shapefile

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper(indo[j])

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=17

indo[j]

indo_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j]),]

indo_tab

indo[j] %in% ant_regs
grep("Sumatra",ant_regs)

reg_i <- shp_ants[grep("Sumatra",shp_ants$BENTITY2_N),]

#verify if feature is already in the shapefile

toupper("Sumatra") %in% shp_birds$GAVIARegion

#if yes

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == indo[j])] <- toupper("Sumatra")


t <- table2[which(!is.na(table2$GAVIARegion)),]


i=97 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=98  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=99 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=100 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=101 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

##############  Decision to consider Sicily and Sardinia as individua entries

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

#make sure to separate Sicily and Sardinia data

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                    table2$AreaName1 != "Sicily" &
                    table2$AreaName1 != "Sardinia")] <- countries[i]
 
table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


reg_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == "Sicily"),]

reg_tab

"Sicily" %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == "Sicily"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = "SICILY"

a <- spChFIDs(a,paste0(i,"_1"))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

#make sure to separate Sicily and Sardinia data

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == "Sicily")] <- "SICILY"

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


reg_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == "Sardinia"),]

reg_tab

"Sardinia" %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == "Sardinia"),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = "SARDINIA"

a <- spChFIDs(a,paste0(i,"_2"))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

#make sure to separate Sicily and Sardinia data

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == "Sardinia")] <- "SARDINIA"

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=102  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

length(unique(count_tab$Binomial))

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=103 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

##############  DECISION to consider Japan as one region,
#many species did not carry info on what island they were

countries[i]

length(unique(count_tab$Binomial))

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=104 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

length(unique(count_tab$Binomial))

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=105 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=106 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=107 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=108 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=109 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=110 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=111 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=112 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=113 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=114 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=115 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=116 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=117 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
grep("Libya",griis_regs)

reg_i <- shp_griis[grep("Libya",shp_griis$Region),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=118 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=119 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=120 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=121 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=122 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)
grep("Macedonia",griis_regs)

reg_i <- shp_griis[grep("Macedonia",shp_griis$Region),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=123 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=124 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=125 #### region prepared manually from Indonesia DIVA GIS regions

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

##############  DECISION to consider each Malaysian island or archipelago 
#as one region,
#very few species will be lost

mal <- sort(unique(count_tab$AreaName1))

#prepare region manually

shp_mal <- readOGR("Malaysia",dsn = wd_specific_issues)
names(shp_mal) <- "GAVIARegion"

j=1  #entries with empty province info solve later

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j]),]

mal_tab

j=2  #Java is in Indonesia! Ignore the one sps entry (Passer dosmesticus,
#NOT listed in Java as Indonesia).

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j]),]

mal_tab

j=3 

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("PENINSULA",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"PENINSULA" %in% shp_birds$GAVIARegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

shp_birds <- spRbind(shp_birds,reg_i)

shp_birds$GAVIARegion

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "PENINSULA (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=4

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("PENINSULA",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"PENINSULA (MYS)" %in% shp_birds$GAVIARegion

#if yes

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "PENINSULA (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=5

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("PENINSULA",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"PENINSULA (MYS)" %in% shp_birds$GAVIARegion

#if yes

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "PENINSULA (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=6

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("PENINSULA",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"PENINSULA (MYS)" %in% shp_birds$GAVIARegion

#if yes

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "PENINSULA (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=7

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("PENINSULA",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"PENINSULA (MYS)" %in% shp_birds$GAVIARegion

#if yes

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "PENINSULA (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=8

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("PENINSULA",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"PENINSULA (MYS)" %in% shp_birds$GAVIARegion

#if yes

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "PENINSULA (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=9

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("PENINSULA",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"PENINSULA (MYS)" %in% shp_birds$GAVIARegion

#if yes

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "PENINSULA (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=10

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("BORNEO",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"BORNEO (MYS)" %in% shp_birds$GAVIARegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

shp_birds <- spRbind(shp_birds,reg_i)

shp_birds$GAVIARegion

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "BORNEO (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=11

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("BORNEO",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"BORNEO (MYS)" %in% shp_birds$GAVIARegion

#if yes

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "BORNEO (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=12

mal[j]

mal_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == mal[j]),]

mal_tab


reg_i <- shp_mal[grep("PENINSULA",shp_mal$GAVIARegion),]

#verify if feature is already in the shapefile

"PENINSULA (MYS)" %in% shp_birds$GAVIARegion

#if yes

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])] <- "PENINSULA (MYS)"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == mal[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=126 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=127 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=128 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=129 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=130 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=131 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=132 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=133 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=134 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=135 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=136 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=137 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=138 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=139 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=140 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=141 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=142 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=143 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=144 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=145 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=146 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=147 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

reg_i <- shp_griis[which(toupper(shp_griis$Region) == countries[i]),]

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = countries[i]

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

#include region name in the checklist

table2$GAVIARegion[which(table2$CountryName == countries[i])] <-
  countries[i]

table2$GAVIARegion[which(table2$CountryName == countries[i])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


i=148 #### region for Netherlands Antilles from DIVA GIS regions

countries[i]

count_tab <- table2[which(table2$CountryName == countries[i]),]

count_tab

countries[i] %in% toupper(griis_regs)

##############  DECISION to consider each Netherlands Antilles island  
#as one region,
# few species will be lost

ant <- sort(unique(count_tab$AreaName1))

#prepare region manually

shp_ant <- readOGR("ANT_adm1",dsn = wd_antilles)


j=1  #entries with empty province info solve later

ant[j]

ant_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ant[j]),]

ant_tab


j=2

ant[j]

ant_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ant[j]),]

ant_tab


reg_i <- shp_ant[grep("Bonaire",shp_ant$NAME_1),]

#verify if feature is already in the shapefile

toupper(ant[j]) %in% shp_birds$GAVIARegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = "BONAIRE"

a <- spChFIDs(a,paste(i))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ant[j])] <- "BONAIRE"

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ant[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=3

ant[j]

ant_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ant[j]),]

ant_tab


reg_i <- shp_ant[grep("Cura",shp_ant$NAME_1),]

#verify if feature is already in the shapefile

toupper(ant[j]) %in% shp_birds$GAVIARegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ant[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ant[j])] <- toupper(ant[j])

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ant[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=4

ant[j]

ant_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ant[j]),]

ant_tab


reg_i <- shp_ant[grep(ant[j],shp_ant$NAME_1),]

#verify if feature is already in the shapefile

toupper(ant[j]) %in% shp_birds$GAVIARegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ant[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ant[j])] <- toupper(ant[j])

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ant[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


j=5 #SAINT MARTIN ET SAINT BARTHELEMY ARE FRENCH!!! WHO MADE THIS DATABASE?

ant[j]

ant_tab <- table2[which(table2$CountryName == countries[i] &
                          table2$AreaName1 == ant[j]),]

ant_tab

reg_i1 <- shp_griis[grep("Saint Martin",shp_griis$Region),]
reg_i2 <- shp_griis[grep("Saint Bart",shp_griis$Region),]

reg_i <- spRbind(reg_i1,reg_i2)

#verify if feature is already in the shapefile

toupper(ant[j]) %in% shp_birds$GAVIARegion

#if not

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$GAVIARegion = toupper(ant[j])

a <- spChFIDs(a,paste0(i,"_",j))

shp_birds <- spRbind(shp_birds,a)

shp_birds$GAVIARegion

#plot(shp_birds)

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ant[j])] <- toupper(ant[j])

table2$GAVIARegion[which(table2$CountryName == countries[i] &
                           table2$AreaName1 == ant[j])]

t <- table2[which(!is.na(table2$GAVIARegion)),]


wd <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/Shapefile"

writeOGR(shp_birds,layer = "Shapefile_birds",drive = "ESRI Shapefile",
         dsn = wd)

setwd(wd)

write.csv(table2,"Tables.csv")
