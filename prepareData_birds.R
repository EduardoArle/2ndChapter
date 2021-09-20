library(plyr);library(rgdal);library(raster);library(data.table)
library(plotfunctions);library(maptools);library(rworldmap)

#list WDs
wd_shp <-  "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/Shapefile"
wd_table <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA"

#load shp
shp <- readOGR("Shapefile_birds",dsn = wd_shp,
               use_iconv=TRUE, encoding="UTF-8")

#check if all regions listed in the table are represented in the shapefile
setwd(wd_table)
sps_reg_list <- read.csv("Table2.csv") #load table

#select only the rows for which region could be resolved
sps_reg_list2 <- sps_reg_list[which(!is.na(sps_reg_list$GAVIARegion)),]


############

#list wds
wd_data <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA"
wd_range_maps <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/GAVIA_rangemaps"


setwd(wd_data)
table <- read.csv("GAVIA_main_data_table.csv")

regs <- unique(table$AreaName1)
no_info <- table[which(table$AreaName1 == ""),]

sps_n <- length(unique(table$Binomial))

#check range maps
setwd(wd_range_maps)
sps <- list.files(pattern=".shp$")

test <- readOGR(gsub(".shp","",sps[249]),dsn=wd_range_maps)

test@data

world2 <- spTransform(world,proj4string(test))
plot(world2)
plot(test,add=F,col="magenta",border=NA)



?project

names(table)[4] <- "n" #rename species counting column

#load GloNAF shp (modified version to deal with overlaps)
wd_shp <- 
  "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GloNAF_modified_shp"
shp <- readOGR("GloNAF_modified",dsn=wd_shp)

#load GloNAF master file (downloaded from GloNAF)
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GLONAF")

sps_reg_list <- read.csv("Taxon_x_List_GloNAF_fixed.csv") #saved again, original
                                                  # was saved with tab or so
regions <- read.csv("Region_GloNAF_vanKleunenetal2018Ecology.csv") #table 
                                          #translating regions ID, names etc
#make a table to include region name and obj ID into the tables
merge_tab <- regions[,c(1,3,5)]

#include region names and obj ID into the table with occurrence counts
table2 <- merge(table,merge_tab,by="OBJIDsic",sort = F, all.x = T)

#include region names and obj ID into the GloNAF table
sps_reg_list2 <- merge(sps_reg_list,merge_tab,by="region_id",sort = F, 
                       all.x = T)

#create column with species and region info in the occurrence count table
table2$sps_reg <- paste0(table2$species,"_",table2$OBJIDsic)

#create column with species and region info in the GloNAF table
sps_reg_list2$sps_reg <- paste0(sps_reg_list2$standardized_name,"_",
                                sps_reg_list2$OBJIDsic)

#eliminate duplicated rows in the checklists file (probably due to synonyms
#in the original names that have been resolved)

sps_reg_list3 <- unique(as.data.table(sps_reg_list2), #the table has to be in 
                        by = c("sps_reg"))            #data.table
                        

#eliminate rows combining sps/reg that are not listed in the GloNAF table
table3 <- table2[which(table2$sps_reg %in% sps_reg_list2$sps_reg),]

#check which sps_region combination in the GloNAF table have at least one GBIF 
#occurrence
sps_reg_list3$confirmed <- as.numeric(sps_reg_list3$sps_reg %in% 
                                      table3$sps_reg)

#calculate the percentage of species per regions confirmed by GBIF

perc_confirmed <- ddply(sps_reg_list3,.(name,OBJIDsic),summarise,
              confirmed=mean(confirmed)*100,
              n_sps=sum(OBJIDsic)/unique(OBJIDsic))

#include the number of species, the percentage of species listed confirmed in 
#the shapefile, and region names

shp2 <- shp #create a copy of the shp
shp2$confirmed <- rep(9999,nrow(shp2))  #include percentage of confirmed sps
shp2$n_sps <- rep(9999,nrow(shp2))  #include n_species  
shp2$region <- rep("XX",nrow(shp2))  #include region names  

for(i in 1:nrow(shp2))
{
  a <- which(perc_confirmed$OBJIDsic == shp$OBJIDsic[i])
  if(length(a) == 1)
  {
    shp2$confirmed[i] <- perc_confirmed$confirmed[a]  
    shp2$n_sps[i] <- perc_confirmed$n_sps[a]  
    shp2$region[i] <- perc_confirmed$name[a]
  }else{
    shp2$confirmed[i] <- NA 
    shp2$n_sps[i] <- NA 
    shp2$name[i] <- NA 
  }
}

#check if there are enough records in the region to model the species occurrence
#currently checking if there are 50 records in the region
#think about better solution, Tiffany may be able to help

#count overall number of records per region
table3_b <- ddply(table3, .(sps_reg), summarise, sum(n)) 
names(table3_b)[2] <- "n" #rename column

#eliminate rows with less than 50 records
table3_c <- table3_b[which(table3_b$n >= 50),]

#check which sps_region combination in the GloNAF table have at least 50 GBIF 
#occurrences
sps_reg_list3$enough_recs <- as.numeric(sps_reg_list3$sps_reg %in% 
                                        table3_c$sps_reg)

#calculate the percentage of species per regions with >=50 GBIF recs

perc_enough_recs <- ddply(sps_reg_list3,.(name,OBJIDsic),summarise,
                        enough_recs=mean(enough_recs)*100)

#include the the percentage of species with enough records in the shp

shp2$enough_recs <- rep(9999,nrow(shp2))  #include percentage of confirmed sps

for(i in 1:nrow(shp2))
{
  a <- which(perc_enough_recs$OBJIDsic == shp$OBJIDsic[i])
  if(length(a) == 1)
  {
    shp2$enough_recs[i] <- perc_enough_recs$enough_recs[a]  
  }else{
    shp2$enough_recs[i] <- NA 
  }
}

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

#include the range dynamics value in the shp

shp2$Rd <- rep(9999,nrow(shp2))  #include percentage of confirmed sps

for(i in 1:nrow(shp2))
{
  a <- which(sps_per_reg2$OBJIDsic == shp$OBJIDsic[i])
  if(length(a) == 1)
  {
    shp2$Rd[i] <- sps_per_reg2$Rd[a]  
  }else{
    shp2$Rd[i] <- NA 
  }
}

### save table

setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Results/GloNAF")

write.csv(shp2@data,"GloNAF.csv")

### plot maps

# Load world map frame and continent outline
setwd("C:/Users/ca13kute/Documents/sTWIST")

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
shp2 <- spTransform(shp2,CRS(proj4string(world)))


##### PLOT THE CONFIRMED MAP

#create vector to populate with the colours
col_confirmed <- rep("xx",nrow(shp2)) 

#create vector to populate with the transparency
alpha_confirmed <- shp2$confirmed * 2.55

col_confirmed <- rgb(40,40,148,
                 alpha=alpha_confirmed,
                 maxColorValue = 255)

plot(shp2,col=col_confirmed)
plot(worldmapframe,add=T)


col_leg <- colorRampPalette(c("white", rgb(40,40,148,
                                           alpha=255,
                                           maxColorValue = 255)))

gradientLegend(valRange = c(0, 100), 
               pos=c(0.3,-0.04,0.7,-0.075),
               color = col_leg(20), 
               side = 1,
               n.seg = 1)


##### PLOT THE ENOUGH RECORDS MAP

#create vector to populate with the colours
col_enough <- rep("xx",nrow(shp2)) 

#create vector to populate with the transparency
alpha_enough <- shp2$enough_recs * 2.55

col_enough <- rgb(191,144,0,
                     alpha=alpha_enough,
                     maxColorValue = 255)

plot(shp2,col=col_enough)
plot(worldmapframe,add=T)


col_leg <- colorRampPalette(c("white", rgb(191,144,0,
                                           alpha=255,
                                           maxColorValue = 255)))

gradientLegend(valRange = c(0, 100), 
               pos=c(0.3,-0.04,0.7,-0.075),
               color = col_leg(20), 
               side = 1,
               n.seg = 1)


##### PLOT THE RANGE DINAMIC MAP

#create vector to populate with the colours
col_Rd <- rep("xx",nrow(shp2)) 

#create vector to populate with the transparency
alpha_Rd <- shp2$Rd * 2.55

col_Rd <- rgb(56,87,35,
                  alpha=alpha_Rd,
                  maxColorValue = 255)

plot(shp2,col=col_Rd)
plot(worldmapframe,add=T)


col_leg <- colorRampPalette(c("white", rgb(56,87,35,
                                           alpha=255,
                                           maxColorValue = 255)))

gradientLegend(valRange = c(0, 100), 
               pos=c(0.3,-0.04,0.7,-0.075),
               color = col_leg(20), 
               side = 1,
               n.seg = 1)

##### PLOT INFO BURDEN

#create vector to populate with the colours
col_n_sps <- rep("xx",nrow(shp2)) 

#create vector to populate with the transparency (use log scale)
alpha_n_sps <- log(shp2$n_sps)/max(log(shp2$n_sps)) * 255

col_n_sps <- rgb(135,0,0,
              alpha=alpha_n_sps,
              maxColorValue = 255)

plot(shp2,col=col_n_sps)
plot(worldmapframe,add=T)


col_leg <- colorRampPalette(c("white", rgb(135,0,0,
                                           alpha=255,
                                           maxColorValue = 255)))

# could not plot values the way I want (log) adapt the function
myGradientLegend(valRange = c(1, max(shp2$n_sps)), 
               pos=c(0.3,-0.04,0.7,-0.075),
               color = col_leg(20), 
               side = 1,
               n.seg = c(1,max(shp2$n_sps)/4,max(shp2$n_sps)/2,
                         max(shp2$n_sps)*3/4,max(shp2$n_sps)),
               values = c("1",paste(round(exp(log(max(shp2$n_sps))/4))),
                          paste(round(exp(log(max(shp2$n_sps))/2))),
                          paste(round(exp(log(max(shp2$n_sps))*3/4))),
                          paste(max(shp2$n_sps))))



