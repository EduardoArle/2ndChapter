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


##### plot big map

#load IPBES sub region map
shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#include calculated average in the shp
shp_IPBES$confirmed <- rep(9999,nrow(shp_IPBES))
shp_IPBES$modelling <- rep(9999,nrow(shp_IPBES))
shp_IPBES$rd <- rep(9999,nrow(shp_IPBES))

  
for(i in 1:nrow(shp_IPBES))
{
  a <- which(conf_continent$continent == shp_IPBES$Region[i])
  shp_IPBES$confirmed[i] <- conf_continent$average[a]  
  
  a <- which(model_continent$continent == shp_IPBES$Region[i])
  shp_IPBES$modelling[i] <- model_continent$average[a] 
  
  a <- which(rd_continent$continent == shp_IPBES$Region[i])
  shp_IPBES$rd[i] <- rd_continent$average[a] 
}

#create colour ramp to represent the values
colramp <- colorRampPalette(c("#fe0002", "#d80027", "#a1015d",
                              "#63009e", "#2a00d6", "#0302fc"))

#populate the table with the colours to be plotted 

col_conf <- colramp(100)[cut(c(0,100,shp_IPBES$confirmed), 
                            breaks = 100)][-c(1,2)]

col_model <- colramp(100)[cut(c(0,100,shp_IPBES$modelling), 
                             breaks = 100)][-c(1,2)]

col_rd <- colramp(100)[cut(c(0,100,shp_IPBES$rd), 
                              breaks = 100)][-c(1,2)]


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

means <- as.data.frame(cbind(conf_taxon,
               model_taxon,
               rd_taxon))

#populate the table with the colours to be plotted 

col_tax_c <- colramp(100)[cut(c(0,100,means$conf_taxon), 
                            breaks = 100)][-c(1,2)]

col_tax_m <- colramp(100)[cut(c(0,100,means$model_taxon), 
                              breaks = 100)][-c(1,2)]

col_tax_rd <- colramp(100)[cut(c(0,100,means$rd_taxon), 
                              breaks = 100)][-c(1,2)]

means <- cbind(means,col_tax_c,col_tax_m,col_tax_rd)

#load icons
setwd(wd_icons)

a <- lapply(list.files(),readPNG)
icons <- lapply(a,as.raster)

###### CONFIRMED

#manually changing the HEX values before the transparency to
#the colours selected to plot each taxon icon

means 

amph <- gsub("^.{0,7}","#5800A8",icons[[1]])
ant <- gsub("^.{0,7}","#B0004D",icons[[2]])
bird <- gsub("^.{0,7}","#2600D9",icons[[3]])
fresh <- gsub("^.{0,7}","#90006E",icons[[4]])
fungus <- gsub("^.{0,7}","#C90035",icons[[5]])
mammal <- gsub("^.{0,7}","#6D0092",icons[[6]])
plant <- gsub("^.{0,7}","#860078",icons[[7]])
reptile <- gsub("^.{0,7}","#5B00A5",icons[[8]])
spider <- gsub("^.{0,7}","#C90035",icons[[9]])

### PLOT

par(mar=c(2,2,2,2))
plot(shp2,border=NA,col=col_conf)
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


###### MODELLING

#manually changing the HEX values before the transparency to
#the colours selected to plot each taxon icon

means 

amph <- gsub("^.{0,7}","#C1003D",icons[[1]])
ant <- gsub("^.{0,7}","#E80016",icons[[2]])
bird <- gsub("^.{0,7}","#5500AB",icons[[3]])
fresh <- gsub("^.{0,7}","#C1003D",icons[[4]])
fungus <- gsub("^.{0,7}","#EE0010",icons[[5]])
mammal <- gsub("^.{0,7}","#E60018",icons[[6]])
plant <- gsub("^.{0,7}","#B60048",icons[[7]])
reptile <- gsub("^.{0,7}","#CC0032",icons[[8]])
spider <- gsub("^.{0,7}","#F0000F",icons[[9]])

### PLOT

par(mar=c(2,2,2,2))
plot(shp2,border=NA,col=col_model)
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


###### RANGE DYNAMICS

#manually changing the HEX values before the transparency to
#the colours selected to plot each taxon icon

means 

amph <- gsub("^.{0,7}","#F2000D",icons[[1]])
ant <- gsub("^.{0,7}","#FE0002",icons[[2]])
bird <- gsub("^.{0,7}","#C60037",icons[[3]])
fresh <- gsub("^.{0,7}","#FC0003",icons[[4]])
fungus <- gsub("^.{0,7}","#FE0002",icons[[5]])
mammal <- gsub("^.{0,7}","#F60009",icons[[6]])
plant <- gsub("^.{0,7}","#FC0003",icons[[7]])
reptile <- gsub("^.{0,7}","#F80007",icons[[8]])
spider <- gsub("^.{0,7}","#FE0002",icons[[9]])

### PLOT

par(mar=c(2,2,2,2))
plot(shp2,border=NA,col=col_rd)
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



