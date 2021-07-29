library(rgdal);library(raster);library(rgeos)

wd_clip <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Specific_shp_issues"
wd_indonesia_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/IDN_adm"
wd_specific_issues <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/Specific_shp_issues"
wd_malaysia_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/MYS_adm"
wd_uk_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA/GBR_adm"
wd_shp_ants <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants/Bentity2_shapefile_fullres/Bentity2_shapefile_fullres"
wd_chile <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/CHL_adm"
wd_russia_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/RUS_adm"
wd_nz_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/NZL_adm"
wd_jp_shp <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders/JPN_adm"


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


### CHILE

#load Chile shp
shp_chile <- readOGR("CHL_adm1", dsn = wd_chile, use_iconv=TRUE, encoding="UTF-8")

shp_chile@data

plot(shp_chile)

chile_regions <- shp_chile$NAME_1

#North

chile_north <- shp_chile[which(shp_chile$NAME_1 ==  "Arica y Parinacota" |
                               shp_chile$NAME_1 ==  "Tarapacá" |
                               shp_chile$NAME_1 ==  "Antofagasta" |
                               shp_chile$NAME_1 ==  "Atacama"),]


chile_north <- gUnaryUnion(chile_north)

plot(chile_north)

chile_north$SpiderRegion = "Chile North"

chile_north <- spChFIDs(chile_north,paste(1))

#Centre

chile_centre <- shp_chile[which(shp_chile$NAME_1 ==  "Coquimbo" |
                                 shp_chile$NAME_1 ==  "Valparaíso" |
                                 shp_chile$NAME_1 ==  "Región Metropolitana de Santiago" |
                                 shp_chile$NAME_1 ==  "Libertador General Bernardo O'Higgins" |
                                 shp_chile$NAME_1 ==  "Maule" |
                                 shp_chile$NAME_1 ==  "Bío-Bío" |
                                 shp_chile$NAME_1 ==  "Araucanía" |
                                 shp_chile$NAME_1 ==  "Los Ríos"),]

chile_centre <- gUnaryUnion(chile_centre)

plot(chile_centre)

#cut the islands

b <- as(extent(-75, -68, -47, -25), 'SpatialPolygons')
chile_centre <- crop(chile_centre,b)

plot(chile_centre)

chile_centre$SpiderRegion = "Chile Central"

chile_centre <- spChFIDs(chile_centre,paste(2))


#South

chile_south <- shp_chile[which(shp_chile$NAME_1 ==  "Los Lagos" |
                                 shp_chile$NAME_1 ==  "Aisén del General Carlos Ibáñez del Campo" |
                                 shp_chile$NAME_1 ==  "Magallanes y Antártica Chilena"),]

chile_south <- gUnaryUnion(chile_south)
chile_south2 <- gSimplify(chile_south,tol=0.01)

plot(chile_south2)

chile_south2$SpiderRegion = "Chile South"

chile_south2 <- spChFIDs(chile_south2,paste(3))

#put them together

shp_chile_provs <- spRbind(chile_north,chile_centre)
shp_chile_provs <- spRbind(shp_chile_provs,chile_south2)


plot(shp_chile_provs)

writeOGR(shp_chile_provs,layer = "Chile",drive = "ESRI Shapefile",
         dsn = wd_specific_issues)

# EUROPEAN RUSSIA ... CAZZO

russia_shp <- readOGR("RUS_adm1",dsn=wd_russia_shp)

# Central European Russia

central_eur_rus <- russia_shp[which(russia_shp$NAME_1 == "Bryansk" |
                                    russia_shp$NAME_1 == "Kaluga" |
                                    russia_shp$NAME_1 == "Smolensk" |
                                    russia_shp$NAME_1 == "Tver'" |
                                    russia_shp$NAME_1 == "Yaroslavl'" |
                                    russia_shp$NAME_1 == "Kostroma" |
                                    russia_shp$NAME_1 == "Nizhegorod" |
                                    russia_shp$NAME_1 == "Chuvash" |
                                    russia_shp$NAME_1 == "Ul'yanovsk" |
                                    russia_shp$NAME_1 == "Penza" |
                                    russia_shp$NAME_1 == "Tambov" |
                                    russia_shp$NAME_1 == "Voronezh" |
                                    russia_shp$NAME_1 == "Belgorod" |
                                    russia_shp$NAME_1 == "Kursk" |
                                    russia_shp$NAME_1 == "Orel" |
                                    russia_shp$NAME_1 == "Tula" |
                                    russia_shp$NAME_1 == "Moscow City" |
                                    russia_shp$NAME_1 == "Moskva" |
                                    russia_shp$NAME_1 == "Vladimir" |
                                    russia_shp$NAME_1 == "Ivanovo" |
                                    russia_shp$NAME_1 == "Ryazan'" |
                                    russia_shp$NAME_1 == "Mordovia" |
                                    russia_shp$NAME_1 == "Lipetsk"),]


plot(central_eur_rus,col="orange")

central_eur_rus2 <- gUnaryUnion(central_eur_rus)

central_eur_rus2$SpiderRegion = "Central European Russia"

central_eur_rus2 <- spChFIDs(central_eur_rus2,paste(1))

eur_rus <- central_eur_rus2

plot(central_eur_rus2)

# East European Russia

south_eur_rus <- russia_shp[which(russia_shp$NAME_1 == "Rostov" |
                                      russia_shp$NAME_1 == "Kalmyk" |
                                      russia_shp$NAME_1 == "Volgograd" |
                                      russia_shp$NAME_1 == "Astrakhan'" |
                                      russia_shp$NAME_1 == "Saratov"),]


plot(south_eur_rus,col="orange")

south_eur_rus2 <- gUnaryUnion(south_eur_rus)

south_eur_rus2$SpiderRegion = "South European Russia"

south_eur_rus2 <- spChFIDs(south_eur_rus2,paste(nrow(eur_rus)+1))

eur_rus <- spRbind(eur_rus,south_eur_rus2)

plot(south_eur_rus2)


# North European Russia

north_eur_rus <- russia_shp[which(russia_shp$NAME_1 == "Murmansk" |
                                  russia_shp$NAME_1 == "Karelia" |
                                  russia_shp$NAME_1 == "Vologda" |
                                  russia_shp$NAME_1 == "Nenets" |
                                  russia_shp$NAME_1 == "Komi"),]

plot(north_eur_rus,col="orange")

### Arkhangel'sk Include Islands that look like should not be included in
# North European Russia... Manual fix

Arkhangel <- russia_shp[which(russia_shp$NAME_1 == "Arkhangel'sk"),]

b <- as(extent(34, 51, 60, 67), 'SpatialPolygons')
Arkhangel2 <- crop(Arkhangel,b)
Arkhangel2 <- gBuffer(Arkhangel2,width=0)
Arkhangel2$SpiderRegion <- "Arkhangel"
Arkhangel2 <- spChFIDs(Arkhangel2,paste(10))

plot(Arkhangel2)

north_eur_rus2 <- gUnaryUnion(north_eur_rus)

north_eur_rus2$SpiderRegion = "North European Russia"

north_eur_rus3 <- spRbind(north_eur_rus2,Arkhangel2)

north_eur_rus3 <- gUnaryUnion(north_eur_rus3)

north_eur_rus3$SpiderRegion = "North European Russia"

north_eur_rus3 <- spChFIDs(north_eur_rus3,paste(nrow(eur_rus)+1))

eur_rus <- spRbind(eur_rus,north_eur_rus3)


# Northwest European Russia

northwest_eur_rus <- russia_shp[which(russia_shp$NAME_1 == "Leningrad" |
                                      russia_shp$NAME_1 == "City of St. Petersburg" |
                                      russia_shp$NAME_1 == "Pskov" |
                                      russia_shp$NAME_1 == "Novgorod"),]


plot(northwest_eur_rus,col="orange")

northwest_eur_rus2 <- gUnaryUnion(northwest_eur_rus)

northwest_eur_rus2$SpiderRegion = "Northwest European Russia"

northwest_eur_rus2 <- spChFIDs(northwest_eur_rus2,paste(nrow(eur_rus)+1))

eur_rus <- spRbind(eur_rus,northwest_eur_rus2)

plot(east_eur_rus2)


# East European Russia

east_eur_rus <- russia_shp[which(russia_shp$NAME_1 == "Kirov" |
                                  russia_shp$NAME_1 == "Perm'" |
                                  russia_shp$NAME_1 == "Udmurt" |
                                  russia_shp$NAME_1 == "Bashkortostan" |
                                  russia_shp$NAME_1 == "Orenburg" |
                                  russia_shp$NAME_1 == "Samara" |
                                  russia_shp$NAME_1 == "Tatarstan" |
                                  russia_shp$NAME_1 == "Mariy-El"),]


plot(east_eur_rus,col="orange")

east_eur_rus2 <- gUnaryUnion(east_eur_rus)

east_eur_rus2$SpiderRegion = "East European Russia"

east_eur_rus2 <- spChFIDs(east_eur_rus2,paste(nrow(eur_rus)+1))

eur_rus <- spRbind(eur_rus,east_eur_rus2)

plot(east_eur_rus)

writeOGR(eur_rus,layer = "European Russia",drive = "ESRI Shapefile",
         dsn = wd_specific_issues)


plot(eur_rus,col="red")

eur_rus$SpiderRegion

russia_shp$NAME_1


#### New Zealand 

shp_nz <- readOGR("NZL_adm1",dsn=wd_nz_shp)

shp_nz@data


#### Japan

shp_jp <- readOGR("JPN_adm1",dsn=wd_jp_shp)

shp_jp@data
plot(shp_jp)
