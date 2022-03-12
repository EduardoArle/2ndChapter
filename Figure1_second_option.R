library(plyr);library(raster);library(rgdal);library(plotfunctions)
library(png);library(data.table)

#paths
wd_results <- "C:/Users/ca13kute/Documents/2nd_Chapter/Results"
wd_IPBES <- "C:/Users/ca13kute/Documents/2nd_Chapter/IPBES/Simplified_shp"
wd_icons <- "C:/Users/ca13kute/Documents/2nd_Chapter/Figures/Figure1/Icons"
wd_cont_burden <- "C:/Users/ca13kute/Documents/2nd_Chapter/Species_burden_continent"

#list taxa
setwd(wd_results)
taxa <- list.files()

#load species/continent relations for all taxa
setwd(wd_cont_burden)
sps_cont <- lapply(list.files(),read.csv)
names(sps_cont) <- gsub("_continent.csv","",list.files())

#mantain only one row per sps-cont combinations
sps_cont_unique <- lapply(sps_cont,function(x){
  unique(as.data.table(x),by="sps_cont")
})

#count species listed per continent
sps_per_cont <- lapply(sps_cont_unique,function(x){
  ddply(x,.(Continent),nrow)
})

#rename the column with the counts per cont with the taxon name
for(i in 1:length(sps_per_cont))
{
  names(sps_per_cont[[i]])[2] <- names(sps_per_cont)[[i]]
}

#merge tables for each taxon
sps_per_cont2 <- sps_per_cont[[1]]
for(i in 1:(length(sps_per_cont)-1))
{
  sps_per_cont2 <- merge(sps_per_cont2,sps_per_cont[[i+1]],all=T)
}

#load indicator results for all taxa
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

# species burden per cont
sps_per_cont2$Total <- apply(sps_per_cont2[,-1], 1,
                             function(x) sum(x, na.rm = T))

##### plot big map

#load IPBES sub region map
shp_IPBES <- readOGR("IPBES_SubRegion",dsn = wd_IPBES)

#include sps burden and calculated average in the shp
shp_IPBES$burden <- rep(9999,nrow(shp_IPBES))
shp_IPBES$confirmed <- rep(9999,nrow(shp_IPBES))
shp_IPBES$modelling <- rep(9999,nrow(shp_IPBES))
shp_IPBES$rd <- rep(9999,nrow(shp_IPBES))


for(i in 1:nrow(shp_IPBES))
{
  a <- which(sps_per_cont2$Continent == shp_IPBES$Region[i])
  shp_IPBES$burden[i] <- sps_per_cont2$Total[a]
   
  a <- which(conf_continent$continent == shp_IPBES$Region[i])
  shp_IPBES$confirmed[i] <- conf_continent$average[a]  
  
  a <- which(model_continent$continent == shp_IPBES$Region[i])
  shp_IPBES$modelling[i] <- model_continent$average[a] 
  
  a <- which(rd_continent$continent == shp_IPBES$Region[i])
  shp_IPBES$rd[i] <- rd_continent$average[a] 
}

#create colour ramp to represent the values
colramp <- colorRampPalette(c("#9e0142", "#d53e4f", "#f46d43",
                              "#fdae61", "#fee08b", "#ffffbf",
                              "#e6f598", "#abdda4", "#66c2a5",
                              "#3288bd", "#5e4fa2"))

#populate the table with the colours to be plotted 

col_burden <- colramp(100)[cut(log(shp_IPBES$burden), 
                               breaks = 100)]
shp_IPBES$col_burden <- col_burden

col_conf <- colramp(100)[cut(c(0,100,shp_IPBES$confirmed), 
                            breaks = 100)][-c(1,2)]
shp_IPBES$col_conf <- col_conf

col_model <- colramp(100)[cut(c(0,100,shp_IPBES$modelling), 
                             breaks = 100)][-c(1,2)]
shp_IPBES$col_model <- col_model

col_rd <- colramp(100)[cut(c(0,100,shp_IPBES$rd), 
                              breaks = 100)][-c(1,2)]
shp_IPBES$col_rd <- col_rd


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

results2 <- results

#include taxon name in each table
for(i in 1:length(results2))
{
  results2[[i]] <- cbind(Taxon=names(results2)[i],results2[[i]])
}

#join all results in one table
results3 <- rbindlist(results2)

#calculate average and SD of indicators per taxon

means_taxon <- ddply(results3,.(Taxon),
                     summarise, 
                     mean_confirmed = mean(confirmed,na.rm=T),
                     mean_modelling = mean(modelling,na.rm=T),
                     mean_Rd = mean(Rd,na.rm=T),
                     entries = sum(n_sps))

means_taxon$species <- c(76,282,458,601,595,221,12704,197,267)

#populate the table with the colours to be plotted 

colours_burden <- colramp(100)[cut(log(means_taxon$species), 
                                   breaks = 100)]

means_taxon$col_burden <- colours_burden

colours_conf <- colramp(100)[cut(c(0,100,means_taxon$mean_confirmed), 
                                 breaks = 100)][-c(1,2)]

means_taxon$col_conf <- colours_conf


colours_mod <- colramp(100)[cut(c(0,100,means_taxon$mean_modelling), 
                                breaks = 100)][-c(1,2)]

means_taxon$col_mod <- colours_mod

colours_rd <- colramp(100)[cut(c(0,100,means_taxon$mean_Rd), 
                               breaks = 100)][-c(1,2)]

means_taxon$col_Rd <- colours_rd

#load icons
setwd(wd_icons)

a <- lapply(list.files(),readPNG)
icons <- lapply(a,as.raster)

###### BURDEN (plot in log scale)

#manually changing the HEX values before the transparency to
#the colours selected to plot each taxon icon

means_taxon[,c(1,7)]

amph <- gsub("^.{0,7}","#9E0142",icons[[1]])
ant <- gsub("^.{0,7}","#F88F52",icons[[2]])
bird <- gsub("^.{0,7}","#FDC877",icons[[3]])
fresh <- gsub("^.{0,7}","#FEE18D",icons[[4]])
fungus <- gsub("^.{0,7}","#FEE18D",icons[[5]])
mammal <- gsub("^.{0,7}","#F46E43",icons[[6]])
plant <- gsub("^.{0,7}","#5E4FA2",icons[[7]])
reptile <- gsub("^.{0,7}","#EE6445",icons[[8]])
spider <- gsub("^.{0,7}","#F7884F",icons[[9]])

### PLOT

par(mar=c(2,2,2,2))
plot(shp2,border=NA,col=shp_IPBES$col_burden)
plot(worldmapframe,add=T)

myGradientLegend(valRange = c(min(shp_IPBES$burden), max(shp_IPBES$burden)), 
                 pos=c(0.3,0,0.7,.015),
                 color = colramp(100), 
                 side = 1,
                 n.seg = c(min(shp_IPBES$burden),
                           max(shp_IPBES$burden)/4,
                           max(shp_IPBES$burden)/2,
                           max(shp_IPBES$burden)*3/4,
                           max(shp_IPBES$burden)),
                 values = c(paste(round(exp(log(min(shp_IPBES$burden))))),
                            paste(round(exp(log(max(shp_IPBES$burden))/4))),
                            paste(round(exp(log(max(shp_IPBES$burden))/2))),
                            paste(round(exp(log(max(shp_IPBES$burden))*3/4))),
                            paste(max(shp_IPBES$burden))),
                 cex = 1)

## draw box
rect(-17500000,-7600000,-9000000,1500000, col = "white")

#plot icons
rasterImage(bird,-17100000,-1500000,-14700000,1150000) #bird
rasterImage(mammal,-14200000,-1000000,-12500000,1100000) #mammal
rasterImage(amph,-12100000,-1250000,-9200000,950000) #amphibian
rasterImage(reptile,-16700000,-3100000,-14900000,-1600000) #reptile
rasterImage(fresh,-14200000,-3680000,-12400000,-1120000) #freshwater
rasterImage(ant,-12100000,-3350000,-9200000,-1270000) #ants
rasterImage(spider,-17500000,-6400000,-14200000,-2950000) #spiders
rasterImage(plant,-14100000,-5300000,-12300000,-3800000) #freshwater
rasterImage(fungus,-12200000,-5800000,-9200000,-3250000) #freshwater

myGradientLegend(valRange = c(min(means_taxon$species),
                              max(means_taxon$species)), 
                 pos=c(0.17,0.16,0.31,.169),
                 color = colramp(100), 
                 side = 1,
                 n.seg = c(min(means_taxon$species),
                           max(means_taxon$species)/4,
                           max(means_taxon$species)/2,
                           max(means_taxon$species)*3/4,
                           max(means_taxon$species)),
                 values = c(paste(round(exp(log(min(means_taxon$species))))),
                            paste(round(exp(log(max(means_taxon$species))/4))),
                            paste(round(exp(log(max(means_taxon$species))/2))),
                            paste(round(exp(log(max(means_taxon$species))*3/4))),
                            paste(max(means_taxon$species))),
                 cex = 1)

###### CONFIRMED

#manually changing the HEX values before the transparency to
#the colours selected to plot each taxon icon

means_taxon

amph <- gsub("^.{0,7}","#6EC5A4",icons[[1]])
ant <- gsub("^.{0,7}","#FDD27F",icons[[2]])
bird <- gsub("^.{0,7}","#429AB5",icons[[3]])
fresh <- gsub("^.{0,7}","#FEFAB7",icons[[4]])
fungus <- gsub("^.{0,7}","#FDD784",icons[[5]])
mammal <- gsub("^.{0,7}","#D6EE9B",icons[[6]])
plant <- gsub("^.{0,7}","#FEFAB7",icons[[7]])
reptile <- gsub("^.{0,7}","#83CDA4",icons[[8]])
spider <- gsub("^.{0,7}","#FDBE6E",icons[[9]])

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

means_taxon

amph <- gsub("^.{0,7}","#FEEA9C",icons[[1]])
ant <- gsub("^.{0,7}","#F57446",icons[[2]])
bird <- gsub("^.{0,7}","#8AD0A4",icons[[3]])
fresh <- gsub("^.{0,7}","#E2F398",icons[[4]])
fungus <- gsub("^.{0,7}","#F88F52",icons[[5]])
mammal <- gsub("^.{0,7}","#FA9C58",icons[[6]])
plant <- gsub("^.{0,7}","#F8FCB5",icons[[7]])
reptile <- gsub("^.{0,7}","#FEF0A7",icons[[8]])
spider <- gsub("^.{0,7}","#F16943",icons[[9]])

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

means_taxon

amph <- gsub("^.{0,7}","#CA324C",icons[[1]])
ant <- gsub("^.{0,7}","#9E0142",icons[[2]])
bird <- gsub("^.{0,7}","#FDB466",icons[[3]])
fresh <- gsub("^.{0,7}","#A90D44",icons[[4]])
fungus <- gsub("^.{0,7}","#A30743",icons[[5]])
mammal <- gsub("^.{0,7}","#C42C4B",icons[[6]])
plant <- gsub("^.{0,7}","#A30743",icons[[7]])
reptile <- gsub("^.{0,7}","#B91F48",icons[[8]])
spider <- gsub("^.{0,7}","#9E0142",icons[[9]])

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




