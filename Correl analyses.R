library(data.table);library(plyr);library(rworldmap)
library(raster);library(rgdal)

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

world <- getMap()

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

wd_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Correl analyses/Shapefile"

writeOGR(all_countries,layer = "Shapefile_variables",
         driver = "ESRI Shapefile",dsn = wd_shp)

