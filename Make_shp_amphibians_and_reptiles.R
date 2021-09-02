#list packages
library(rgdal);library(raster);library(maptools);library(rgeos)

#list wds
wd_shp_ants <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants/Bentity2_shapefile_fullres/Bentity2_shapefile_fullres"
wd_GRIIS_shp <- "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp" 
wd_zaf <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/ZAF_adm"
wd_specific_issues <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Specific_shp_issues"
wd_data_amph_rep <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles"

#load ants shp
shp_ants <- readOGR("Bentity2_shapefile_fullres", dsn = wd_shp_ants)

#load GRIIS shp
shp_griis <- readOGR("GRIIS_ISO3", dsn = wd_GRIIS_shp)

#load South African shapefile
shp_zaf <- readOGR("ZAF_adm1", dsn = wd_zaf)

#load amph and reps table
setwd(wd_data_amph_rep)
table <- read.csv("ddi12617-sup-0001-appendixs1.csv")

#list regions in the table 
tab_regs <- sort(unique(table$Region))

#list regions in the ants shapefile
ant_regs <- sort(unique(shp_ants$BENTITY2_N))

#list regions in the griis shapefile
griis_regs <- sort(unique(shp_griis$Region))

#list south african regions
zaf_regs <- sort(unique(shp_zaf$NAME_1))

#manually compile the shp solving case by case

i=1

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

shp_rep_amph <- spChFIDs(reg_i,paste(i))

i=2

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=3

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)
shp_rep_amph 


i=4

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=5

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=6 ##### DECISION only one species listed, also listed as alien in 
########## Seychelles. Other regions (islands) of the coutry not individually
########## listed. So I disregarded this entry

tab_regs[i]
tab_regs[i] %in% ant_regs  #### Aldabra (Seychelles)
tab_regs[i] %in% griis_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]
table[which(table$Region == "Seychelles"),]
table[which(table$Region == "Mahé"),]
table[which(table$Region == "Cosmoledo"),]
table[which(table$Region == "Farquhar"),]

table2 <- table[-which(table$Region == tab_regs[i]),]


i=7

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=8  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
                                #into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=9

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=10

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=11 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=12 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=13 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=14 ##### DECISION three species listed, also listed as alien in 
########## the province of Buenos Aires. So I disregarded this entry
########### for Argentina Distrito Federal

tab_regs[i]
tab_regs[i] %in% ant_regs  #### Aldabra (Seychelles)
tab_regs[i] %in% griis_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]
table[which(table$Region == "Buenos Aires"),]


table3 <- table2[-which(table2$Region == tab_regs[i]),]


i=15

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=16

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=17

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=18 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]


plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=19 #### both ants and griis shapes clump Ascension, St Helena, and Tristas
    ##### da Cunha together. Separated shapefile needed

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

feature <- shp_ants[grep(tab_regs[i],shp_ants$BENTITY2_N),]
cropped <- crop(feature,extent(-14.5, -14.25, -8, -7.8))

plot(cropped)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]
table[grep("Helena",table$Region),]
table[grep("Tristan",table$Region),]


reg_i <- cropped

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=20 #### no aliens listed, so just ignored

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

table4 <- table3[-which(table3$Region == tab_regs[i]),]


i=21

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=22

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=23

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=24

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=25

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=26 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]


plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=27

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=28

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=29

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=30

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=31

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=32

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=33

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=34 #### griis shape clumps Bonaire Sint Eustatius, and Saba together.   
    ####Separated shapefile needed

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

feature <- shp_griis[grep(tab_regs[i],shp_griis$Region),]
cropped <- crop(feature,extent(-68.5, -68, 11.9, 12.4))

plot(cropped)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]
table[grep("Sint",table$Region),]
table[grep("Saba",table$Region),]

reg_i <- cropped

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=35  #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Bosnia",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Bosnia",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=36  #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Bouvet",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Bouvet",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=37  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]


plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=38

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=39  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[grep("Virgin",shp_griis$Region),]
reg_i <- reg_i[1,]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=40

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=41

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=42

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=43

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=44

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=45

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=46 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Verde",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Verde",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=47

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=48

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=49

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=50

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=51

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=52

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=53

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=54 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]


plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=55 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]


plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=56

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=57

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=58

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=59

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=60 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Cocos",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Cocos",shp_ants$BENTITY2_N),]
reg_i <- reg_i[1,]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=61

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=62

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=63

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=64

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=65

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=66

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=67 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Cordoba",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Cordoba",shp_ants$BENTITY2_N),]
reg_i <- reg_i[2,]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=68

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=69

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=70

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=71

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=72 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]


plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=73

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=74

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=75

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=76

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=77  #### no aliens listed, so just ignored

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

table5 <- table4[-which(table4$Region == tab_regs[i]),]


i=78

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=79

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=80 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=81  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=82

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=83 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[grep("Timor",shp_griis$Region),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=84

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=85  #### south African province AND name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs
tab_regs[i] %in% zaf_regs
grep("Eastern Cape",zaf_regs)

reg_i <- shp_zaf[grep("Eastern Cape",shp_zaf$NAME_1),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=86

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=87 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Galapagos",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Galapagos",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=88

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=89

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=90 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Entre R",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Entre R",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=91

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=92

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=93

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=94  #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Falkland",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Falkland",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=95 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Faroe",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Faroe",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=96

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=97

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=98

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=99

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=100

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=101 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Corsica",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Corsica",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=102 #### south African province 

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs
tab_regs[i] %in% zaf_regs

reg_i <- shp_zaf[which(shp_zaf$NAME_1 == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=103

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=104

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=105

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=106 #### south African province 

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs
tab_regs[i] %in% zaf_regs

reg_i <- shp_zaf[which(shp_zaf$NAME_1 == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=107 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Georgia US",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Georgia US",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=108

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=109 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=110 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=111

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=112 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=113 #### no aliens listed, so just ignored

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

table6 <- table5[-which(table5$Region == tab_regs[i]),]

i=114 #### no aliens listed, so just ignored

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

table7 <- table6[-which(table6$Region == tab_regs[i]),]

i=115 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[grep("Guad",shp_griis$Region),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=116 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=117

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=118

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=119

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=120

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=121

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=122

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=123 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=124

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=125 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("McDo",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("McDo",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=126

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=127

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=128

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=129

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=130

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=131

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=132

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=133

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=134

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=135 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=136

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=137

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=138

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=139

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=140 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=141

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=142 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Sard",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Sard",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=143 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Sici",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Sici",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=144

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=145

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=146

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=147 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=148

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=149 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Juan F",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Juan F",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=150

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=151

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=152

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=153

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=154

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=155

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=156 #### no aliens listed, so just ignored

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

table8 <- table7[-which(table7$Region == tab_regs[i]),]


i=157

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=158 #### south African province 

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs
tab_regs[i] %in% zaf_regs

reg_i <- shp_zaf[which(shp_zaf$NAME_1 == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=159

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=160

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=161 #### the data base separated Newfoundland from Labrador, make shapes
     ##### representing it

reg_i <- readOGR("Labrador",dsn=wd_specific_issues)

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=162

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=163

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=164

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=165

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=166

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=167

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=168 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Lord",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Lord",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=169

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=170

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=171

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=172

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=173

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=174

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=175  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=176 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Maldive",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Maldive",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=177

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=178

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=179 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Marsh",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Marsh",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=180  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=181

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=182

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=183

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=184 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=185 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=186

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=187 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Federal Mex",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Federal Mex",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=188 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Federal Mex",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == "Mexico"),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=189

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=190 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Micho",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Micho",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=191 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Micro",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Micro",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=192

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=193

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=194

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=195

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=196

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=197 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=198

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=199

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=200 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=201

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=202

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=203

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=204 #### south African province 

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs
tab_regs[i] %in% zaf_regs

reg_i <- shp_zaf[which(shp_zaf$NAME_1 == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=205

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=206

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=207

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=208

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=209

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=210 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Nether",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Nether",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=211 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Neuq",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Neuq",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=212

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=213

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=214

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=215

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=216

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=217

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=218

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=219

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=220

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=221  #### the data base separated Newfoundland from Labrador, make shapes
##### representing it

reg_i <- readOGR("Newfoundland",dsn=wd_specific_issues)

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=222

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=223 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Nicobar",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Nicobar",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=224

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=225

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=226 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Norfolk",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Norfolk",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=227

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=228

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=229  #### south African province AND name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs
tab_regs[i] %in% zaf_regs
grep("North West",zaf_regs)

reg_i <- shp_zaf[grep("North West",shp_zaf$NAME_1),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=230   #### south African province AND name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs
tab_regs[i] %in% zaf_regs
grep("Northern Cape",zaf_regs)

reg_i <- shp_zaf[grep("Northern Cape",shp_zaf$NAME_1),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=231 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=232 #### south African province AND name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs
tab_regs[i] %in% zaf_regs
grep("Limpopo",zaf_regs)

reg_i <- shp_zaf[grep("Limpopo",shp_zaf$NAME_1),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=233

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=234

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=235

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=236

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=237 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Nuevo",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Nuevo",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=238

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=239

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=240

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=241

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=242

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=243

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=244

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=245

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=246

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=247

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=248 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=249

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=250

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=251

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=252

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=253 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=254

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=255 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Azores",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Azores",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=256 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Madeira",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Madeira",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=257 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Portugal",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Portugal",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=258 #### São Tomé and Principe estão juntos.   
####Separated shapefile needed

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

feature <- shp_griis[grep(tab_regs[i],shp_griis$Region),]
cropped <- crop(feature,extent(7.2, 7.5, 1.5, 1.7))

plot(cropped)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]
table[grep("Sao Tome",table$Region),]

reg_i <- cropped

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=259

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=260

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=261

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=262

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=263

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=264 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Quere",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Quere",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=265

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=266 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[grep("union",shp_griis$Region),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=267 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Revill",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Revill",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=268

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=269

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=270 #### no aliens listed, so just ignored

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

table9 <- table8[-which(table8$Region == tab_regs[i]),]


i=271

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=272  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[grep("Russia",shp_griis$Region),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=273  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=274 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=275  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=276  #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=277

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=278 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=279

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=280

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=281 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Potosi",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Potosi",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=282 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=283

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=284 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Santa Fe",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Santa Fe",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=285

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=286 #### São Tomé and Principe estão juntos.   
####Separated shapefile needed

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

feature <- shp_griis[grep(tab_regs[i],shp_griis$Region),]
cropped <- crop(feature,extent(6.3, 6.8, 0, 0.5))

plot(cropped)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

reg_i <- cropped

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=287

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=288

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=289

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=290

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=291

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=292

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=293 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[which(shp_griis$Region == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=294

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=295

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=296

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=297 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[grep("Soqotra",shp_griis$Region),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=298

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=299

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=300

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=301

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=302

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=303

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=304 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Balea",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Balea",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=305 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Cana",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Cana",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=306 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Spain",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Spain",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=308

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=309 #### both ants and griis shapes clump Ascension, St Helena, and Tristas
##### da Cunha together. Separated shapefile needed

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

feature <- shp_griis[grep("Helena",shp_griis$Region),]
cropped <- crop(feature,extent(-5.8, -5.6, -16.1, -15.8))

plot(cropped)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]
table[grep("Helena",table$Region),]
table[grep("Ascension",table$Region),]


reg_i <- cropped

reg_i
plot(reg_i)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=310 ## St Martin and St Barthélémy sont ensemble dans cette list, mais
   ##### séparés dans le shafile de GRIIS. 

tab_regs[i]

st_martin <- shp_griis[grep("Saint Martin",shp_griis$Region),]
st_barth <- shp_griis[grep("Saint Bart",shp_griis$Region),]

st_st <- spRbind(st_martin,st_barth)

a <- gUnaryUnion(st_st)

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=311

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=312

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=313

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=314

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=315

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=316

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=317 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Tajikistan",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Tajikistan",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=318

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=319

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=320

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=321

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=322

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=323

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=324

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=325

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=326

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=327

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=328

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=329

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=330 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Tucuman",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Tucuman",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=331

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=332

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=333

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=334

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=335

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=336

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=337

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=338

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=339 #### region taken from GRIIS shapefile, attribute table must be modified
########## to include the feature in the shp

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

reg_i <- shp_griis[grep("Virgin",shp_griis$Region),]
reg_i <- reg_i[2,]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=340

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=341

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=342 #### no aliens listed, so just ignored

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

table10 <- table9[-which(table9$Region == tab_regs[i]),]


i=343

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=344

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=345

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=346

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=347 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Viet",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Viet",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=348

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=349

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=350

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=351

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=352 ## North Western Australia and St South Western Australia sont ensemble dans cette list, mais
##### séparés dans le shapefile de ants. 

tab_regs[i]

reg_i <- shp_ants[grep("Western Australia",shp_ants$BENTITY2_N),]

a <- gUnaryUnion(st)

plot(a)

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=353 #### south African province AND name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
tab_regs[i] %in% griis_regs
tab_regs[i] %in% zaf_regs
grep("Western Cape",zaf_regs)

reg_i <- shp_zaf[grep("Western Cape",shp_zaf$NAME_1),]
reg_i <- reg_i[2,]
reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

### modify attribute table

a <- gBuffer(reg_i,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

a$BENTITY2_N = tab_regs[i]
a$POINT = "NA"

a <- spChFIDs(a,paste(i))

shp_rep_amph <-spRbind(shp_rep_amph,a)


i=354

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=355

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=356

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=357 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Yuca",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Yuca",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=358 #just a name issue

tab_regs[i]
tab_regs[i] %in% ant_regs
shp_ants[grep("Yukon",shp_ants$BENTITY2_N),]

reg_i <- shp_ants[grep("Yukon",shp_ants$BENTITY2_N),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
a$BENTITY2_N <- tab_regs[i]  #standardise country name
shp_rep_amph <-spRbind(shp_rep_amph,a)


i=359 

tab_regs[i]
tab_regs[i] %in% ant_regs

reg_i <- shp_ants[which(shp_ants$BENTITY2_N == tab_regs[i]),]

reg_i
plot(reg_i)

#visualise and check if region is right
table[which(table$Region == tab_regs[i]),]

plot(world,col="gray70",border=NA)
plot(reg_i,add=T,col="red",border=NA)

#make shapefile

a <- spChFIDs(reg_i,paste(i))
shp_rep_amph <-spRbind(shp_rep_amph,a)



shp_rep_amph
plot(shp_rep_amph,col="khaki")
plot(a,col="green",add=T)



plot(shp_rep_amph[350,])


dsn1 <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Regions_shapefile"

writeOGR(shp_rep_amph,layer="Regions_reptiles_amphibians",
         dsn = dsn1, driver = "ESRI Shapefile")

#save new table (without excluded entries)

setwd(wd_data_amph_rep)
write.csv(table10,"Checklist_amphibians_reptiles.csv",row.names = F)

shp <- readOGR("Regions_reptiles_amphibians",dsn=dsn1)

length(unique(shp@data$BENTITY2_N))
