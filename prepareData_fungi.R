library(plyr);library(rgdal);library(raster);library(data.table)
library(plotfunctions);library(maptools);library(rworldmap)

#list WDs
wd_shp <-  "C:/Users/ca13kute/Documents/2nd_Chapter/Fungi/Shapefile"
wd_table <- "C:/Users/ca13kute/Documents/2nd_Chapter/Fungi"

#load shp
shp <- readOGR("Shapefile_fungi",dsn = wd_shp,
               use_iconv=TRUE, encoding="UTF-8")

#check if all regions listed in the table are represented in the shapefile
setwd(wd_table)
sps_reg_list <- read.csv("Table.csv") #load table

#select only the rows for which region could be resolved
sps_reg_list2 <- sps_reg_list[which(!is.na(sps_reg_list$FungiRegion)),]

#check if all regions in the table are represented in the shp
regs <- sort(unique(sps_reg_list2$FungiRegion))
shp_regs <- sort(unique(shp$FungRgn))
missing <- regs[-which(regs %in% shp_regs)]

missing

#make a sps list
sps_list <- unique(sps_reg_list2$species)

#save sps_list
setwd(wd_table)
saveRDS(sps_list,"Sps_list_fungi")

##### Use taxonomicHarmonisation script and then get occ from cluster

#load table with occurrence counts (calculated by script occRegionSpiders)
setwd(wd_table)
sps_reg_count <- readRDS("Fungi_occurrence_region_count")


#######################

library(rgdal);library(raster)

wd_table <- "C:/Users/ca13kute/Documents/2nd_Chapter/Fungi"

#load table
setwd(wd_table)
table <- read.csv("Alien_fungi.csv")

n_sps <- length(unique(table$species))

names(table)

table2 <- table[,-c(20:31)]

table3 <- table2[,c(20,6,9,11,13,15)]

table4 <- table3[sample(c(1:nrow(table3)),10),]

setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/Examples")
write.csv(table4,"Examplary_data.csv")

?sample

table2[,5]

names(table2)

#load shp
shp <- readOGR("Bentity2_shapefile_fullres",dsn = wd_shp)





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
