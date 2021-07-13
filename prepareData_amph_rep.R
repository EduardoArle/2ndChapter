library(plyr);library(rgdal);library(raster);library(data.table)
library(plotfunctions)

#list WDs
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Regions_shapefile"
wd_table <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles"
wd_amph <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Amphibians"

#load shp
shp <- readOGR("Regions_reptiles_amphibians",dsn = wd_shp,
               use_iconv=TRUE, encoding="UTF-8")

#check if all regions listed in the table are represented
#in the shapefile
setwd(wd_table)
sps_reg_list <- read.csv("Checklist_amphibians_reptiles.csv") #load table
regs <- sort(unique(sps_reg_list$Region))
shp_regs <- sort(unique(shp$BENTITY2_N))
missing <- regs[-which(regs %in% shp_regs)]

#load table with occurrence counts (calculated by script occRegionAmphibia)
setwd(wd_amph)
sps_reg_count <- readRDS("Amphibia_occurrence_region_count")
list.files()

names(table)[4] <- "n" #rename species counting column










