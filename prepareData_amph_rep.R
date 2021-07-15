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

names(sps_reg_count)[4] <- "n" #rename species counting column

#create column with species and region info in the occurrence count table
sps_reg_count$sps_reg <- paste0(sps_reg_count$species,"_",
                                sps_reg_count$BENTITY2_N)

#create column with species and region info in the amphibians table
sps_reg_list$sps_reg <- paste0(sps_reg_list$Species,"_",
                                sps_reg_list$Region)


#eliminate duplicated rows in the checklists file (probably due to synonyms
#in the original names that have been resolved)

sps_reg_list2 <- unique(as.data.table(sps_reg_list), #the table has to be in 
                        by = c("sps_reg"))            #data.table


#eliminate rows combining sps_reg_count that are not listed in the amphibian table
sps_reg_count2 <- sps_reg_count[which(sps_reg_count$sps_reg %in% sps_reg_list$sps_reg),]

#check which sps_region combination in the amphibian table have at least 1 GBIF 
#occurrence
sps_reg_list2$confirmed <- as.numeric(sps_reg_list2$sps_reg %in% 
                                        sps_reg_count2$sps_reg)


#calculate the percentage of species per regions confirmed by GBIF

perc_confirmed <- ddply(sps_reg_list2,.(Region),summarise,
                        confirmed=mean(confirmed)*100,
                        n_sps=length(c(Region)))

#include the number of species and the percentage of species listed confirmed in 
#the shapefile

shp2 <- shp #create a copy of the shp
shp2$confirmed <- rep(9999,nrow(shp2))  #include percentage of confirmed sps
shp2$n_sps <- rep(9999,nrow(shp2))  #include n_species  

for(i in 1:nrow(shp2))
{
  a <- which(perc_confirmed$Region == shp$BENTITY2_N[i])
  if(length(a) == 1)
  {
    shp2$confirmed[i] <- perc_confirmed$confirmed[a]  
    shp2$n_sps[i] <- perc_confirmed$n_sps[a]  
  }else{
    shp2$confirmed[i] <- NA 
    shp2$n_sps[i] <- NA 
  }
}

#check if there are at least 50 records in the same continent to 
#model the species occurrence 


#count overall number of records per region
table2_b <- ddply(table2, .(sps_reg), summarise, sum(n)) 
names(table2_b)[2] <- "n" #rename column

#eliminate rows with less than 50 records
table2_c <- table2_b[which(table2_b$n >= 50),]

