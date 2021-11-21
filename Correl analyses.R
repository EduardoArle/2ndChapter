library(data.table);library(plyr);library(rworldmap)
library(raster);library(rgdal);library(rworldxtra)
library(rgeos)

#prepare data manually to include treaties in the GDP table
#country name problems, as always

setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Correl analyses")

GDP <- read.csv("GDP.csv",sep="/t")

treaties <- read.csv("International_treaties.csv")

treaties_country <- treaties[,c(2,9)]

write.csv(treaties_country,"International_treaties_countries.csv")

write.csv(GDP,"Variables.csv",row.names = F)

#make shapefile with countries that have both info

GDP2 <- GDP[which(!is.na(GDP$interTreaties)),]

world <- getMap(resolution = "high")

world2 <- world[which(world$NAME %in% GDP2$country),]

missing <- GDP2$country[-which(GDP2$country %in% world$NAME)]
missing

#manually select these countries

regs_world <- sort(world$NAME)

other_countries <- c("Antigua and Barb.","Bosnia and Herz.",
                     "Central African Rep.","Czech Rep.",
                     "Dominican Rep.","Congo (Kinshasa)",
                     "Eq. Guinea","Swaziland","Guinea Bissau",
                     "Marshall Is.","N. Korea","Macedonia",
                     "Congo (Brazzaville)","St. Kitts and Nevis",
                     "St. Vin. and Gren.","Solomon Is.",
                     "S. Korea","East Timor")

all_countries <- world[which(world$NAME %in% GDP2$country |
                            world$NAME %in% other_countries),]

#create new column with the names in the table 

all_countries$CountryName <- as.character(all_countries$NAME)

for(i in 1:length(missing))
{
  a <- which(all_countries$CountryName == other_countries[i])
  all_countries$CountryName[a] <- missing[i]
}

#include GDP per capita and international treaties data in the shp

all_countries$gdpPerCapita <- "xx"
all_countries$interTreaties <- "xx"

for(i in 1:nrow(all_countries))
{
  a <- which(GDP$country == all_countries$CountryName[i])
  all_countries$gdpPerCapita[i] <- GDP$gdpPerCapita[a]
  all_countries$interTreaties[i] <- GDP$interTreaties[a]
}

#round GDP per capita

all_countries$gdpPerCapita <- 
  round(as.numeric(all_countries$gdpPerCapita))

### travel distance variable

setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Correl analyses/access_50k")

tr_dist <- raster("acc_50k.tif")

# agregate per countrie

all_countries$travelTime <- "xx"

for(i in 1:nrow(all_countries))
{
  a <- all_countries[i,] #select each country

  r2 <- crop(tr_dist, extent(a)) #crop
  r3 <- mask(r2, a) #mask
  
  mean_tr <- mean(r3[],na.rm=T)
  
  all_countries$travelTime[i] <- mean_tr
  print(i)
}

#round travel time

all_countries$travelTime <- 
  round(as.numeric(all_countries$travelTime))

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Correl analyses/Shapefile"

writeOGR(all_countries,layer = "Shapefile_variables",
         driver = "ESRI Shapefile",dsn = wd_shp)

##### Import results and calculate average completeness per country

# Import shapefiles

#amphibians
wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Regions_shapefile"
shp_amph <- readOGR("Regions_reptiles_amphibians",dsn = wd_shp,
               use_iconv=TRUE, encoding="UTF-8")



### relate shp regions to countries
reg_count <- list()

for(i in 1:nrow(shp_amph))
{
  pts_regs <- spsample(shp_amph[i,], n=1000, type='regular') #seed points in each region
  a <- over(pts_regs,all_countries)
  b <- table(a$CountryName)
  
  plot(shp_amph[i,])
  shp_amph[i,]$BENTITY2_N
  plot(pts_regs,add=T,pch=19,col="magenta")
  
  b
  
  #make sure that only countries with at least 10% of the region covering are considered
  c <- b*100/sum(b) 
  
  c
  
  d <- c[which(c >= 30)]
  
  d
  
  if(!is.null(names(d))){
    reg_count[[i]] <- names(d)
  }else{
    reg_count[[i]] <- NA
  }
  
  names(reg_count)[i] <- shp_amph[i,]$BENTITY2_N
  
  reg_count
  print(i)
}

#make a table summarising these relationships

for(i in 1:length(reg_count)){
  if(i == 1){
    rel_reg_count <- data.frame(Region = names(reg_count)[i],
                                Country = reg_count[[i]])
  }else{
    a <- data.frame(Region = names(reg_count)[i],
                              Country = reg_count[[i]])
    rel_reg_count <- rbind(rel_reg_count,a)
  }
}



######
wd_results <- "C:/Users/ca13kute/Documents/2nd_Chapter/Results"

setwd(wd_results)

taxa <- list.files()
