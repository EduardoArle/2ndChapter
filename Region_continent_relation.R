library(raster);library(rgdal);library(rgeos)

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
