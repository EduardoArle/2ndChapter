library(plyr);library(raster);library(rgdal);library(plotfunctions)
library(png)

#paths
wd_results <- "C:/Users/ca13kute/Documents/2nd_Chapter/Results"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"
wd_icons <- "C:/Users/ca13kute/Documents/2nd_Chapter/Figures/Figure1/Icons"

#list taxa
setwd(wd_results)
taxa <- list.files()

#load results for all taxa
results <- list()

for(i in 1:length(taxa))
{
  setwd(paste0(wd_results,"/",taxa[i],"/Tables"))
  results[[i]] <- read.csv(list.files()[1])
}

names(results) <- taxa

#calculate average indicators per continent
means <- list()

for(i in 1:length(results))
{
  means[[i]] <- ddply(results[[i]],.(continent),
                      summarise, 
                      confirmed = mean(confirmed,na.rm=T),
                      modeling = mean(modelling,na.rm=T),
                      Rd = mean(Rd,na.rm=T),
                      entries = sum(n_sps))
}

names(means) <- names(results)

#make one table for each indicator

confirmed <- means[[1]][,c(1,2)]

names(confirmed)[2] <- names(means)[1]

for(i in 2:length(means))
{
  a <- means[[i]][,c(1,2)]
  names(a)[2] <- names(means)[i]
  
  confirmed <- merge(confirmed,a,by="continent",all.x=T)
}

confirmed$cont_short <- confirmed$continent
confirmed$cont_short[c(4,6)] <- c("Western Europe","East Africa")

#reorganise rows and columns in the order I want in the figure

confirmed <- confirmed[,c(1,11,6,8,10,3,5,9,2,7,4)]
confirmed <- confirmed[c(4,7,11,14,8,2,12,16,17,10,6,3,5,9,13,15,18,1),]


modelling <- means[[1]][,c(1,3)]

names(modelling)[2] <- names(means)[1]

for(i in 2:length(means))
{
  a <- means[[i]][,c(1,3)]
  names(a)[2] <- names(means)[i]
  
  modelling <- merge(modelling,a,by="continent",all.x=T)
}

modelling$cont_short <- modelling$continent
modelling$cont_short[c(4,6)] <- c("Western Europe","East Africa")

#reorganise rows and columns in the order I want in the figure

modelling <- modelling[,c(1,11,6,8,10,3,5,9,2,7,4)]
modelling <- modelling[c(4,7,11,14,8,2,12,16,17,10,6,3,5,9,13,15,18,1),]


rd <- means[[1]][,c(1,4)]

names(rd)[2] <- names(means)[1]

for(i in 2:length(means))
{
  a <- means[[i]][,c(1,4)]
  names(a)[2] <- names(means)[i]
  
  rd <- merge(rd,a,by="continent",all.x=T)
}

rd$cont_short <- rd$continent
rd$cont_short[c(4,6)] <- c("Western Europe","East Africa")

#reorganise rows and columns in the order I want in the figure

rd <- rd[,c(1,11,6,8,10,3,5,9,2,7,4)]
rd <- rd[c(4,7,11,14,8,2,12,16,17,10,6,3,5,9,13,15,18,1),]


################## CALCULATE THE AVERAGE OF EACH INDICATOR
################## ACROSS TAXA PER CONTINENT

conf_continent <- confirmed
res <- confirmed[,-c(1,2)]

conf_continent$average <- apply(res,1,mean,na.rm=T)


model_continent <- modelling
res <- modelling[,-c(1,2)]

model_continent$average <- apply(res,1,mean,na.rm=T)


rd_continent <- rd
res <- rd[,-c(1,2)]

rd_continent$average <- apply(res,1,mean,na.rm=T)


#calculate the averge amongst indicators per continent

means <- cbind(conf_continent$average,
               model_continent$average,
               rd_continent$average)

average_conts <- data.frame(Continent = rd_continent$continent,
                            Cont_short = rd_continent$cont_short,
                            Mean_indicators = 
                              apply(means,1,mean))

##### plot big map

#load IPBES sub region map
shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#include calculated average in the shp
shp_IPBES$Mean_ind <- rep(9999,nrow(shp_IPBES))
  
for(i in 1:nrow(shp_IPBES))
{
  a <- which(average_conts$Continent == shp_IPBES$Region[i])
  shp_IPBES$Mean_ind[i] <- average_conts$Mean_indicators[a]  
}

#create colour ramp to represent the values
colramp0 <- colorRampPalette(c("#fe0002", "#d80027", "#a1015d",
                              "#63009e", "#2a00d6", "#0302fc"))

colramp <- colorRampPalette(c("#9e0142", "#d53e4f", "#f46d43",
                              "#fdae61", "#fee08b", "#ffffbf",
                              "#e6f598", "#abdda4", "#66c2a5",
                              "#3288bd", "#5e4fa2"))

#populate the table with the colours to be plotted 

colours <- colramp(100)[cut(c(0,100,shp_IPBES$Mean_ind), 
                            breaks = 100)][-c(1,2)]

shp_IPBES$colour <- colours

#plot map

### plot maps

# Load world map frame and continent outline
setwd("C:/Users/ca13kute/Documents/sTWIST")

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
shp2 <- spTransform(shp_IPBES,CRS(proj4string(world)))



################## CALCULATE THE AVERAGE OF EACH INDICATOR
################## ACROSS CONTINENTS PER TAXON

res <- t(confirmed[,-c(1,2)])
conf_taxon <- apply(res,1,mean,na.rm=T)

res <- t(modelling[,-c(1,2)])
model_taxon <- apply(res,1,mean,na.rm=T)

res <- t(rd[,-c(1,2)])
rd_taxon <- apply(res,1,mean,na.rm=T)

means <- cbind(conf_taxon,
               model_taxon,
               rd_taxon)

mean_taxon <- data.frame(Taxon = names(conf_taxon),
                         Mean_indicators = 
                           apply(means,1,mean))

#populate the table with the colours to be plotted 

colours <- colramp(100)[cut(c(0,100,mean_taxon$Mean_indicators), 
                            breaks = 100)][-c(1,2)]

mean_taxon$Colour <- colours

#load icons
setwd(wd_icons)

a <- lapply(list.files(),readPNG)
icons <- lapply(a,as.raster)

#manually changing the HEX values before the transparency to
#the colours selected to plot each taxon icon

amph0 <- gsub("^.{0,7}","#B60048",icons[[1]])
ant0 <- gsub("^.{0,7}","#E1001E",icons[[2]])
bird0 <- gsub("^.{0,7}","#6A0096",icons[[3]])
fresh0 <- gsub("^.{0,7}","#C90035",icons[[4]])
fungus0 <- gsub("^.{0,7}","#E80016",icons[[5]])
mammal0 <- gsub("^.{0,7}","#CC0032",icons[[6]])
plant0 <- gsub("^.{0,7}","#C3003A",icons[[7]])
reptile0 <- gsub("^.{0,7}","#BB0042",icons[[8]])
spider0 <- gsub("^.{0,7}","#E80016",icons[[9]])

amph <- gsub("^.{0,7}","#FDBE6E",icons[[1]])
ant <- gsub("^.{0,7}","#E45648",icons[[2]])
bird <- gsub("^.{0,7}","#ECF7A1",icons[[3]])
fresh <- gsub("^.{0,7}","#F88F52",icons[[4]])
fungus <- gsub("^.{0,7}","#D8434D",icons[[5]])
mammal <- gsub("^.{0,7}","#F7884F",icons[[6]])
plant <- gsub("^.{0,7}","#FA9C58",icons[[7]])
reptile <- gsub("^.{0,7}","#FDB466",icons[[8]])
spider <- gsub("^.{0,7}","#D8434D",icons[[9]])

### PLOT

par(mar=c(2,2,2,2))
plot(shp2,border=NA,col=shp2$colour)
plot(worldmapframe,add=T)


myGradientLegend(valRange = c(0, 50, 100),
                 pos=c(0.3,0,0.7,.015),
                 color = colramp(100),
                 side = 1,
                 n.seg = 1,
                 values = c("0","50","100%"),
                 cex = 1)


rasterImage(bird,-13400000,-2050000,-12100000,-500000) #bird
rasterImage(mammal,-12000000,-1800000,-11000000,-550000) #mammal
rasterImage(amph,-11000000,-1950000,-9200000,-700000) #amphibian
rasterImage(reptile,-13300000,-2900000,-12200000,-2000000) #reptile
rasterImage(fresh,-11950000,-3150000,-10950000,-1750000) #freshwater
rasterImage(ant,-11000000,-3050000,-9200000,-1750000) #freshwater
rasterImage(spider,-13700000,-4700000,-11800000,-2750000) #freshwater
rasterImage(plant,-11900000,-4190000,-11000000,-3150000) #freshwater
rasterImage(fungus,-11000000,-4400000,-9200000,-2800000) #freshwater






