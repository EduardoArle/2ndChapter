library(plyr);library(rgdal);library(raster);library(data.table)
library(plotfunctions)

#load table with occurrence counts (calculated in previous scripts)
wd.data <- "C:/Users/ca13kute/Documents/2nd_Chapter/Fresh_water"

setwd(wd.data)
table <- readRDS("Occurrence_region_count")

names(table)[4] <- "n" #rename species counting column

#load FreshWater shp 
wd_shp <- 
  "C:/Users/ca13kute/Documents/2nd_Chapter/Fresh_water/Data/datatoFigshare"
shp <- readOGR("Basin042017_3119",dsn=wd_shp)

#load FreshWater master file 
setwd(wd_shp)
sps_reg_list <- read.csv2("Occurrence_table.csv")

#change col names
names(sps_reg_list) <- gsub("^X[0-9].","\\1",names(sps_reg_list))

#eliminate questinable records
sps_reg_list2 <- sps_reg_list[which(sps_reg_list$Occurrence.Status == 
                                      "valid"),]

#eliminate native records
sps_reg_list3 <- sps_reg_list2[which(sps_reg_list2$Native.Exotic.Status == 
                                      "exotic"),]

#substitute "." for " " in species names
sps_reg_list3$Fishbase.Valid.Species.Name <- gsub("\\."," ",
                                    sps_reg_list3$Fishbase.Valid.Species.Name)

#create column with species and region info in the occurrence count table
table$sps_reg <- paste0(table$species,"_",table$freshWaterRegion)

#create column with species and region info in the FreshWater table
sps_reg_list3$sps_reg <- paste0(sps_reg_list3$Fishbase.Valid.Species.Name,"_",
                                sps_reg_list3$Basin.Name)

#eliminate duplicated rows in the checklists file (probably due to synonyms
#in the original names that have been resolved)

sps_reg_list4 <- unique(as.data.table(sps_reg_list3), #the table has to be in 
                        by = c("sps_reg"))            #data.table
                        

#eliminate rows combining sps/reg that are not listed in the FreshWater table
table2 <- table[which(table$sps_reg %in% sps_reg_list4$sps_reg),]

#check which sps_region combination in the FreshWater table have at least 1 GBIF 
#occurrence
sps_reg_list4$confirmed <- as.numeric(sps_reg_list4$sps_reg %in% 
                                      table2$sps_reg)

#calculate the percentage of species per regions confirmed by GBIF

perc_confirmed <- ddply(sps_reg_list4,.(Basin.Name),summarise,
              confirmed=mean(confirmed)*100,
              n_sps=length(c(Basin.Name)))

#include the number of species and the percentage of species listed confirmed in 
#the shapefile

shp2 <- shp #create a copy of the shp
shp2$confirmed <- rep(9999,nrow(shp2))  #include percentage of confirmed sps
shp2$n_sps <- rep(9999,nrow(shp2))  #include n_species  

for(i in 1:nrow(shp2))
{
  a <- which(perc_confirmed$Basin.Name == shp$BasinName[i])
  if(length(a) == 1)
  {
    shp2$confirmed[i] <- perc_confirmed$confirmed[a]  
    shp2$n_sps[i] <- perc_confirmed$n_sps[a]  
  }else{
    shp2$confirmed[i] <- NA 
    shp2$n_sps[i] <- NA 
  }
}

#check if there are enough records in the region to model the species occurrence
#currently checking if there are 50 records in the region
#think about better solution, Tiffany may be able to help

#count overall number of records per region
table2_b <- ddply(table2, .(sps_reg), summarise, sum(n)) 
names(table2_b)[2] <- "n" #rename column

#eliminate rows with less than 50 records
table2_c <- table2_b[which(table2_b$n >= 50),]

#check which sps_region combination in the GloNAF table have at least 50 GBIF 
#occurrences
sps_reg_list4$enough_recs <- as.numeric(sps_reg_list4$sps_reg %in% 
                                        table2_c$sps_reg)

#calculate the percentage of species per regions with >=50 GBIF recs

perc_enough_recs <- ddply(sps_reg_list4,.(Basin.Name),summarise,
                        enough_recs=mean(enough_recs)*100)

#include the the percentage of species with enough records in the shp

shp2$enough_recs <- rep(9999,nrow(shp2))  #include percentage of confirmed sps

for(i in 1:nrow(shp2))
{
  a <- which(perc_enough_recs$Basin.Name == shp2$BasinName[i])
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

table3 <- table2[which(!is.na(table2$year)),]
table3 <- table3[which(table3$year >= 1970 &
                       table3$year <= 2019),]

#create column informing to with lustre the occurrences belong
table3$lustre <- floor((table3$year - 1970) / 5) + 1

#count sps_reg occurrence in the 5 year period
table4 <- ddply(table3,.(species,freshWaterRegion,sps_reg,lustre),
                summarise, n_5y = sum(n))

#eliminate rows with combination sps_reg_n_5y < 10
table5 <- table4[-which(table4$n_5y < 10),]

#count how many periods of five years per region have at least 10 rec
table6 <- ddply(table5,.(freshWaterRegion),nrow)

#make a new table counting how many species have been registered in each region
sps_per_reg <- ddply(sps_reg_list4,.(Basin.Name),nrow)
names(sps_per_reg)[2] <- "n_species"

#include table7 info in sps_per_reg
sps_per_reg2 <- merge(sps_per_reg,table6,
                      by.x = "Basin.Name",
                      by.y = "freshWaterRegion",
                      sort = F, all.x = T)

sps_per_reg2$V1[which(is.na(sps_per_reg2$V1))] <- 0

#calculate range dynamics evidence
sps_per_reg2$Rd <- sps_per_reg2$V1/sps_per_reg2$n_species*10
sps_per_reg2 <- sps_per_reg2[,-3]

#include the range dynamics value in the shp

shp2$Rd <- rep(9999,nrow(shp2))  #include percentage of confirmed sps

for(i in 1:nrow(shp2))
{
  a <- which(sps_per_reg2$Basin.Name == shp$BasinName[i])
  if(length(a) == 1)
  {
    shp2$Rd[i] <- sps_per_reg2$Rd[a]  
  }else{
    shp2$Rd[i] <- NA 
  }
}

### save table

setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Results/FreshWater")

write.csv(shp2@data,"FreshWater.csv")

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
alpha_confirmed <- shp2$confirmed[which(!is.na(shp2$confirmed))] * 2.55

col_confirmed[which(!is.na(shp2$confirmed))] <- rgb(40,40,148,
                                                alpha=alpha_confirmed,
                                                maxColorValue = 255)

col_confirmed[which(col_confirmed=="xx")] <- "white"

plot(worldmapframe)
plot(shp2,col=col_confirmed,add=T)
plot(shp2[which(is.na(shp2$confirmed)),],add=T,density=150)

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
alpha_enough <- shp2$enough_recs[which(!is.na(shp2$enough_recs))] * 2.55

col_enough[which(!is.na(shp2$enough_recs))] <- rgb(191,144,0,
                                          alpha=alpha_enough,
                                          maxColorValue = 255)

col_enough[which(col_enough=="xx")] <- "white"

plot(worldmapframe)
plot(shp2,col=col_enough,add=T)
plot(shp2[which(is.na(shp2$enough_recs)),],add=T,density=150)

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
alpha_Rd <- shp2$Rd[which(!is.na(shp2$Rd))] * 2.55

col_Rd[which(!is.na(shp2$Rd))] <- rgb(56,87,35,
                                  alpha=alpha_enough,
                                  maxColorValue = 255)

col_Rd[which(col_Rd=="xx")] <- "white"

plot(worldmapframe)
plot(shp2,col=col_Rd,add=T)
plot(shp2[which(is.na(shp2$Rd)),],add=T,density=150)

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
alpha_n_sps <- log(shp2$n_sps[which(!is.na(shp2$n_sps))])/
       max(log(shp2$n_sps[which(!is.na(shp2$n_sps))])) * 255

col_n_sps[which(!is.na(shp2$n_sps))] <- rgb(135,0,0,
                                        alpha=alpha_n_sps,
                                        maxColorValue = 255)

col_n_sps[which(col_n_sps=="xx")] <- "white"

plot(worldmapframe)
plot(shp2,col=col_n_sps,add=T)
plot(shp2[which(is.na(shp2$n_sps)),],add=T,density=150)

col_leg <- colorRampPalette(c("white", rgb(135,0,0,
                                           alpha=255,
                                           maxColorValue = 255)))

# could not plot values the way I want (log) adapt the function
myGradientLegend(valRange = c(1, max(shp2$n_sps,na.rm=T)), 
               pos=c(0.3,-0.04,0.7,-0.075),
               color = col_leg(20), 
               side = 1,
               n.seg = c(1,max(shp2$n_sps,na.rm=T)/4,max(shp2$n_sps,na.rm=T)/2,
                         max(shp2$n_sps,na.rm=T)*3/4,max(shp2$n_sps,na.rm=T)),
               values = c("1",paste(round(exp(log(max(shp2$n_sps,na.rm=T))/4))),
                          paste(round(exp(log(max(shp2$n_sps,na.rm=T))/2))),
                          paste(round(exp(log(max(shp2$n_sps,na.rm=T))*3/4))),
                          paste(max(shp2$n_sps,na.rm=T))))