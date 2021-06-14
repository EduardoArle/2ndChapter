library(rgdal);library(raster)

wd_table <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders"

#load table
setwd(wd_table)
table <- read.csv("alien spiders Wolfgang Nentwig.csv")

table$binomial <- paste0(table$genus," ",table$species)

n_sps <- length(unique(table$binomial))

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
