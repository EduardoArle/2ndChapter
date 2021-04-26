library(raster);library(rgdal)

wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/ipbes_regions_subregions_shape_1.1"

#load IPBES shapefie
ipbes <- readOGR("IPBES_Regions_Subregions2",dsn=wd_IPBES)

#create new column for "meso region"
ipbes$Meso_Region <- NA

#populate the column with "meso regions"
ipbes$Meso_Region[which(ipbes$Sub_Region == "Antarctica")] <-
  "Antarctica"
  
ipbes$Meso_Region[which(ipbes$Sub_Region == "Caribbean")] <-
  "North America"
  
ipbes$Meso_Region[which(ipbes$Sub_Region == "Central Africa")] <-
  "Africa"

ipbes$Meso_Region[which(ipbes$Sub_Region == "Central and Western Europe")] <-
  "Central and Western Europe" 

ipbes$Meso_Region[which(ipbes$Sub_Region == "Western Asia")] <-
  "Asia"  

ipbes$Meso_Region[which(ipbes$Sub_Region == "South America")] <-
  "South America"  

ipbes$Meso_Region[which(ipbes$Sub_Region == "Eastern Europe")] <-
  "Eastern Europe"  

ipbes$Meso_Region[which(ipbes$Sub_Region == "Oceania")] <-
  "Oceania"

ipbes$Meso_Region[which(ipbes$Sub_Region == "Central Africa")] <-
  "Africa"

ipbes$Meso_Region[which(ipbes$Sub_Region == "West Africa")] <-
  "Africa"

ipbes$Meso_Region[which(ipbes$Sub_Region == "Mesoamerica")] <-
  "North America"

ipbes$Meso_Region[which(ipbes$Sub_Region == "South-East Asia")] <-
  "Asia" 

ipbes$Meso_Region[which(ipbes$Sub_Region ==  "North America")] <-
  "North America"

ipbes$Meso_Region[which(ipbes$Sub_Region == "East Africa and adjacent islands")] <-
  "Africa" 

ipbes$Meso_Region[which(ipbes$Sub_Region == "North Africa")] <-
  "Africa"

ipbes$Meso_Region[which(ipbes$Sub_Region == "Central Asia" )] <-
  "Asia"

unique(ipbes$Meso_Region)
ipbes$Sub_Region[which(is.na(ipbes$Meso_Region))]

unique(ipbes$Sub_Region)
