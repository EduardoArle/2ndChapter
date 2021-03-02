library(plyr);library(rgdal);library(raster);library(data.table)

#load table with occurrence counts (calculated in previous scripts)
wd.data <- "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/Results"

setwd(wd.data)
table <- readRDS("Occurrence_region_count")

names(table)[4] <- "n" #rename species counting column

#load GloNAF shp (modified version to deal with overlaps)
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GloNAF_modified_shp"
shp <- readOGR("GloNAF_modified",dsn=wd_shp)

#load GloNAF master file (downloaded from GloNAF)
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GLONAF")

sps_reg_list <- read.csv("Taxon_x_List_GloNAF_fixed.csv") #saved again, original was saved with tab or so

regions <- read.csv("Region_GloNAF_vanKleunenetal2018Ecology.csv") #table translating regions ID, names etc

#make a table to include region name and obj ID into the tables
merge_tab <- regions[,c(1,3,5)]

#include region names and obj ID into the table with occurrence counts
table2 <- merge(table,merge_tab,by="OBJIDsic",sort = F, all.x = T)

#include region names and obj ID into the GloNAF table
sps_reg_list2 <- merge(sps_reg_list,merge_tab,by="region_id",sort = F, all.x = T)
  
#create column with species and region info in the occurrence count table
table2$sps_reg <- paste0(table2$species,"_",table2$OBJIDsic)

#create column with species and region info in the GloNAF table
sps_reg_list2$sps_reg <- paste0(sps_reg_list2$standardized_name,"_",
                                sps_reg_list2$OBJIDsic)

#eliminate rows combining sps/reg that are not listed in the GloNAF table
table3 <- table2[which(table2$sps_reg %in% sps_reg_list2$sps_reg),]

#check which sps_region combination in the GloNAF table have at least one GBIF occurrence
sps_reg_list2$confirmed <- as.numeric(sps_reg_list2$sps_reg %in% 
                                      table3$sps_reg)

#check if there are enough records in the region to model the species occurrence
#currently checking if there are 50 records in the region
#think about better solution, Tiffany may be able to help

#count overall number of records per region
table3_b <- ddply(table3, .(sps_reg), summarise, sum(n)) 
names(table3_b)[2] <- "n" #rename column

#eliminate rows with less than 50 records
table3_c <- table3_b[which(table3_b$n >= 50),]

#check which sps_region combination in the GloNAF table have at least 50 GBIF occurrences
sps_reg_list2$enough_recs <- as.numeric(sps_reg_list2$sps_reg %in% 
                                        table3_c$sps_reg)


### calculate the range dynamics evidence

#eliminate rows corresponding to years before 1980 and after 2019
#as well as rows not containing year information

table4 <- table3[which(!is.na(table3$year)),]
table4 <- table4[which(table4$year >= 1970 &
                       table4$year <= 2019),]

#create column informing to with lustre the occurrences belong
table4$lustre <- floor((table4$year - 1970) / 5) + 1

#count sps_reg occurrence in the 5 year period
table5 <- ddply(table4,.(species,OBJIDsic,sps_reg,lustre),
                summarise, n_5y = sum(n))

#eliminate rows with combination sps_reg_n_5y < 10
table6 <- table5[-which(table5$n_5y < 10),]

#count how many periods of five years per region have at least 10 rec
table7 <- ddply(table6,.(OBJIDsic),nrow)

#eliminate duplicated rows in the checklists file (probably due to synonyms
#in the original names that have been resolved)

sps_reg_list3 <- unique(as.data.table(sps_reg_list2), #the table has to be in data.table
                        by = c("sps_reg"))

#make a new table counting how many species have been registered in each region
sps_per_reg <- ddply(sps_reg_list3,.(OBJIDsic,name),nrow)
names(sps_per_reg)[3] <- "n_species"

#include table7 info in sps_per_reg
sps_per_reg2 <- merge(sps_per_reg,table7,
                      by = "OBJIDsic",
                      sort = F, all.x = T)

sps_per_reg2$V1[which(is.na(sps_per_reg2$V1))] <- 0

#calculate range dynamics evidence
sps_per_reg2$Rd <- sps_per_reg2$V1/sps_per_reg2$n_species*10
sps_per_reg2 <- sps_per_reg2[,-4]

#include Im in the table
master_file$Im <- ifelse(master_file$IsInvasive == "Invasive",1,0)
mf_Im <- ddply(master_file,.(Location),summarise,Impac=sum(Im))

mf3 <- merge(mf2,mf_Im,by = "Location", sort = F, all.x = T)
mf3$Im <- mf3$Impac/mf3$n_species*100
mf3 <- mf3[,-5]

#include In 
master_file$In <- ifelse(master_file$FirstRecord_orig == "",0,1)
mf_In <- ddply(master_file,.(Location),summarise,Intro=sum(In))

mf4 <- merge(mf3,mf_In,by = "Location", sort = F, all.x = T)
mf4$In <- mf4$Intro/mf4$n_species*100
mf4 <- mf4[,-6]

#calculate final indicator
mf4$ISI <- (mf4$Rd + mf4$Im +mf4$In)/3

#include IPBES region
#I'll do it manually this time...

setwd(wd.out)
write.csv(mf4,"Information_Status_Indicator.csv")

wd <- "I:/MAS/04_personal/Eduardo/sTWIST/GRIIS_shp"

library(raster);library(rgdal)

shp <- readOGR("GRIIS_ISO3",dsn=wd)

shp@data
head(shp@data)
