library(rgdal);library(raster)

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants/Bentity2_shapefile_fullres/Bentity2_shapefile_fullres"
wd_table <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants"

#load shp
shp <- readOGR("Bentity2_shapefile_fullres",dsn = wd_shp)

#load table
setwd(wd_table)
table <- read.csv("Exotic Species Records.csv")

#delete rows with native aliens
table2 <- table[-which(table$Notes ==
          "Native and exotic in the same bentity"),]

#check if all regions listed in the table are represented
#in the shapefile
regs <- sort(unique(table2$bentity2))
shp_regs <- sort(unique(shp$BENTITY2_N))

missing <- regs[-which(regs %in% shp_regs)]

#create two tables, one with the regions represented
#in the shapefile and the other with the unrepresented

table_yes <- table2[which(
  table2$bentity2 %in% shp$BENTITY2_N),]

table_no <- table2[-which(
  table2$bentity2 %in% shp$BENTITY2_N),]

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
table_no$Decision[which(table_no$bentity2 == missing[i] &
                        table_no$Species %in% accounted)] <-
                  "Excuded. Already listed in Greece"

#look for species not already represented by Greece
unaccounted <- a$Species[-which(a$Species %in% b$Species)]

#look one by one

head(shp@data)

length(shp$BENTITY)
length(shp_regs)

unique(table$Notes)
a <- sort(regs)
b <- sort(shp_regs)
