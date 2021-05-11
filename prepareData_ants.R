library(rgdal);library(raster)

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants/Bentities2_23_Oct_2015.qgs"
wd_table <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants"

#load shp
shp <- readOGR("Bentities2_23_Oct_2015.qgs",dsn = wd_shp)

#load table
setwd(wd_table)
table <- read.csv("Exotic Species Records.csv")

#delete rows with native aliens
table2 <- table[-which(table$Notes ==
          "Native and exotic in the same bentity"),]

#check if all regions listed in the table are represented
#in the shapefile
regs <- sort(unique(table2$bentity2))
shp_regs <- sort(unique(shp$BENTITY))

missing <- regs[-which(regs %in% shp_regs)]

#create two tables, one with the regions represented
#in the shapefile and the other with the unrepresented

table_yes <- table2[which(
  table2$bentity2 %in% shp$BENTITY),]

table_no <- table2[-which(
  table2$bentity2 %in% shp$BENTITY),]

#######manually check and fix missing regions

#create a new column in table_no to justify the
#individual decisions

table_no$Decision <- NA

#create a list to include species from regions not
#represented

include <- list()

i=1 #Aegean Islands

missing[i]
shp[grep("Aegean",shp$BENTITY),]

#get entries for the region
a <- table_no[which(table2$bentity2 == missing[i]),]

#check entries for the higher region division
b <- table2[which(table2$bentity2 == "Greece"),]

#identify species that are already listed in the 
#higer regionel level
accounted <- a$Species[which(a$Species %in% b$Species)]

#include note in the table_no
#look for species not already represented by Greece

head(shp@data)

length(shp$BENTITY)
length(shp_regs)

unique(table$Notes)
a <- sort(regs)
b <- sort(shp_regs)
