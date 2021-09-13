library(raster);library(rgdal);library(rgeos)


############## Amphibians and Reptiles ##########################


#list WDs
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Regions_shapefile"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

#load shps
shp_amph <- readOGR("Regions_reptiles_amphibians",dsn = wd_shp,
               use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#get regions centroids
centroids <- gCentroid(shp_amph,byid=TRUE)
centroids <- SpatialPointsDataFrame(centroids,shp_amph@data)

#get continent of each region
continents <- over(centroids,shp_IPBES)

#join info
reg_cont <- cbind(BENTITY2_N = centroids$BENTITY2_N,Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Region)),]

i=1 

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=2

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South Asia"


i=3

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=4

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=5

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=6

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "West Africa"


i=7

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=8

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South Asia"


i=9

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South-East Asia"


i=10

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "East Africa and adjacent islands"


i=11

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=12

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South America"


i=13

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South America"


i=14

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=15

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=16

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=17

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South-East Asia"


i=18

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "North-East Asia"


i=19

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South America"


i=20

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=21

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "North-East Asia"


i=22

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South-East Asia"


i=23

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South Asia"


i=24

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=25

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "East Africa and adjacent islands"


i=26

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=27

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South Asia"


i=28

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=29

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "South-East Asia"


i=30

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=31

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Central and Western Europe"


i=32

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Mesoamerica"


i=33

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=34

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "North America"


i=35

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=36

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "East Africa and adjacent islands"


i=37

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=38

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Central and Western Europe"


i=39

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=40

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=41

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=42

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=43

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Central and Western Europe"


i=44

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Caribbean"


i=45

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


i=46

reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$BENTITY2_N
reg_cont[which(reg_cont$BENTITY2_N == missing_cont[i,1]),]$Region <- "Oceania"


unique(reg_cont$Region)

#manually fix regions that have not been asigned to a continent
missing_cont2 <- reg_cont[which(is.na(reg_cont$Region)),]
missing_cont2 


#save lookup tabe 
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles")
write.csv(reg_cont,"Lookup_table_region_cont.csv")




############## Freshwater ##########################


#list WDs
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Freshwater/Simplified_FreshWater_shp"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

#load shps
shp_fresh <- readOGR("Basin042017_3119",dsn = wd_shp,
                    use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#loop though shapefile features to identify each of their continents

pts_regs <- list()
for(i in 1:nrow(shp_fresh))
{
  pts_regs[[i]] <- spsample(shp_fresh[i,], n=100, type='regular') #seed points in each region
  print(i)
}

#get continent of each region

continents <- character()
for(i in 3081:nrow(shp_fresh))
{
  a <- over(pts_regs[[i]],shp_IPBES)
  b <- table(a$Region)
  c <- which.max(b)
  d <- names(c)
  continents[i] <- d
  print(i)
}

#join info
reg_cont <- data.frame(BasinName = shp_fresh$BasinName,Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Continent)),]

missing_cont

#save lookup tabe 
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Freshwater")
write.csv(reg_cont,"Lookup_table_region_cont.csv")



############## Ants and mammals ##########################


#list WDs
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants/Bentity2_shapefile_fullres/Bentity2_shapefile_fullres"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"

#load shps
shp_ants_mammals <- readOGR("Bentity2_shapefile_fullres",dsn = wd_shp,
                     use_iconv=TRUE, encoding="UTF-8")

shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#loop though shapefile features to identify each of their continents

pts_regs <- list()
for(i in 1:nrow(shp_ants_mammals))
{
  pts_regs[[i]] <- spsample(shp_ants_mammals[i,], n=100, type='regular') #seed points in each region
  print(i)
}

#get continent of each region

continents <- character()
for(i in 422:nrow(shp_ants_mammals))
{
  a <- over(pts_regs[[i]],shp_IPBES)
  b <- table(a$Region)
  c <- which.max(b)
  d <- names(c)
  continents[i] <- d
  print(i)
}

### manually fix what does not work

shp_ants_mammals[i,]
plot(world)
plot(pts_regs[[i]],add=T,pch=19,col="red")

#i=32 Baker Island
continents[i] <- "Oceania"

#i=85 Coral Sea Islands
continents[i] <- "Oceania"

#i=153 Howland Island
continents[i] <- "Oceania"

#i=178 Johnston Atoll
continents[i] <- "Oceania"

#i=387 Spratly Islands
continents[i] <- "South-East Asia"

#i=416 Tokelau
continents[i] <- "Oceania"

#i=421 Tromelin Island
continents[i] <- "East Africa and adjacent islands"

#join info
reg_cont <- data.frame(Region = shp_ants_mammals$BENTITY2_N,
                       Continent = continents)

#manually fix regions that have not been asigned to a continent
missing_cont <- reg_cont[which(is.na(reg_cont$Continent)),]

missing_cont

#save lookup tabe 
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Mammals")
write.csv(reg_cont,"Lookup_table_region_cont.csv")
