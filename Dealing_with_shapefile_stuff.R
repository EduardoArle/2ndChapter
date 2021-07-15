library(rgdal);library(raster);library(rgeos)

wd_clip <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Specific_shp_issues"
wd_indonesia_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/IDN_adm"
wd_specific_issues <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/Specific_shp_issues"
wd_malaysia_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/MYS_adm"
wd_uk_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/GBR_adm"

nf_lab <- readOGR("Newfoundland_Labrador",dsn=wd_clip)

clipping <- readOGR("Clipping", dsn=wd_clip)


newfoundland <- gIntersection(nf_lab,clipping)
labrador <- gDifference(nf_lab,clipping)

newfoundland$BENTITY2_N <- "Newfoundland"
newfoundland$POINT <- "NA"

labrador$BENTITY2_N <- "Labrador"
labrador$POINT <- "NA"


writeOGR(newfoundland,layer="Newfoundland",dsn=wd_clip,drive="ESRI Shapefile")
writeOGR(labrador,layer="Labrador",dsn=wd_clip,drive="ESRI Shapefile")


plot(labrador)
plot(newfoundland)

plot(nf_lab)
plot(clipping)

?gDifference

#### indonesia

# Lesser Sunda Islands (IDN)

indo_shp <- readOGR("IDN_adm1", dsn = wd_indonesia_shp)

nusa_1 <- indo_shp[grep("Nusa",indo_shp$NAME_1),]

bali <- indo_shp[grep("Bali", indo_shp$NAME_1),]

sunda <- spRbind(nusa_1 ,bali)

sunda <- gBuffer(sunda, width = 0)

sunda$GAVIARegion <- toupper("Lesser Sunda Islands (IDN)")

plot(sunda,add=T,col="red")

writeOGR(sunda,layer = "Lesser Sunda Islands (IDN)",drive = "ESRI Shapefile",
         dsn = wd_specific_issues)

# New Guinea

papua <- indo_shp[grep("Papua",indo_shp$NAME_1),]

w_papua <- indo_shp[grep("Jaya", indo_shp$NAME_1),]

new_gui <- spRbind(papua ,w_papua)

new_gui <- gBuffer(new_gui, width = 0)

new_gui$GAVIARegion <- toupper("New Guinea (IDN)")

plot(new_gui,add=F,col="red")

writeOGR(new_gui,layer = "New Guinea (IDN)",drive = "ESRI Shapefile",
         dsn = wd_specific_issues)


# Kalimantan

kalimantan <- indo_shp[grep("Kalimantan",indo_shp$NAME_1),]

kalimantan <- gBuffer(kalimantan, width = 0)

kalimantan <- gSimplify(kalimantan,tol=0.05)

kalimantan$GAVIARegion <- toupper("kalimantan")

plot(kalimantan,add=F,col="red")

writeOGR(kalimantan,layer = "Kalimantan",drive = "ESRI Shapefile",
         dsn = wd_specific_issues)


# Maluku


maluku <- indo_shp[grep("Maluku",indo_shp$NAME_1),]

maluku <- gBuffer(maluku, width = 0)

maluku2 <- gSimplify(maluku,tol=0.02)

maluku2$GAVIARegion <- toupper("maluku")

plot(maluku2,add=F,col="red")

writeOGR(maluku2,layer = "Maluku",drive = "ESRI Shapefile",
         dsn = wd_specific_issues)



# Sulawesi

sulawesi <- indo_shp[grep("Sulawesi",indo_shp$NAME_1),]

sulawesi <- gBuffer(sulawesi, width = 0)

sulawesi2 <- gSimplify(sulawesi,tol=0.02)

sulawesi2$GAVIARegion <- toupper("sulawesi")

plot(sulawesi2,add=F,col="red")

writeOGR(sulawesi2,layer = "Sulawesi",drive = "ESRI Shapefile",
         dsn = wd_specific_issues)



#### Malaysia

# Peninsula

mal_shp <- readOGR("MYS_adm1", dsn = wd_malaysia_shp)

plot(mal_shp)

peninsula <- mal_shp[-which(mal_shp$NAME_1 == "Sabah" |
                            mal_shp$NAME_1 == "Sarawak"),]
 
plot(peninsula,add=T,col="green")

borneo <- mal_shp[which(mal_shp$NAME_1 == "Sabah" |
                              mal_shp$NAME_1 == "Sarawak"),]

plot(borneo,add=T,col="orange")

peninsula <- gBuffer(peninsula,width = 0) #buffer of 0 to transform SpatialPolygonDataFrame
#into SpatialPolygon

peninsula$GAVIARegion = "PENINSULA (MYS)"

peninsula <- spChFIDs(peninsula,paste0(i,"_1"))

borneo <- gBuffer(borneo, width = 0)

borneo$GAVIARegion = "BORNEO (MYS)"

borneo <- spChFIDs(borneo,paste0(i,"_2"))

shp_mal <- spRbind(peninsula,borneo)

writeOGR(shp_mal,layer = "Malaysia",drive = "ESRI Shapefile",
         dsn = wd_specific_issues)


#### UK

uk_shp <- readOGR("GBR_adm1", dsn = wd_uk_shp)

