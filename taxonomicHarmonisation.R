#load libraries
library(taxize)

#################### Amphibians ##################

#set path
wd_species <- "C:/Users/ca13kute/Documents/3rd_Chapter/Amphibians"
#read species table
setwd(wd_species)
list <- read.csv("Amphibia_species_list.csv")
list <- sort(list[,1])


#################### Reptiles ##################

#set path
wd_species <- "C:/Users/ca13kute/Documents/3rd_Chapter/Reptiles"
#read species table
setwd(wd_species)
list <- readRDS("Sps_list_rep")


#################### Freshwater ##################

#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/Freshwater"
#read species table
setwd(wd_species)
list <- readRDS("Sps_list_freshwater")


#################### Mammals #################################
#set path
wd_species <- "C:/Users/ca13kute/Documents/2nd_Chapter/Mammals"

#read species table
setwd(wd_species)
list <- read.csv("Alien_mammal_checklist.csv")
list <- sort(unique(list[,2]))





###################### HARMONISATION ######################



#make list of GBIF resolved names
gbifDarwinCore <- character()
all_names <- character()

for(i in 1:length(list))
{
  match <- get_ids_(list[i], db = 'gbif',rows=1:10)
  match2 <- match[[1]][[1]]
  match3 <- match2[which(match2$matchtype == "EXACT"),]
  
  if(nrow(match2) == 0){
    gbifDarwinCore[i] <- "not matched"
  }
  if(!"species" %in% names(match2)){
    gbifDarwinCore[i] <- "not to species level"
    all_names[i] <- NA
  }else{
    gbifDarwinCore[i] <- unique(match3$species)[1]
  }
  if(length(unique(match3$species)) == 1){
    all_names[i] <- NA
  }else{
    all_names[i] <- paste(unique(match3$species),collapse = " / ")
  }
  print(i)
}


table <- data.frame(entry = list,gbifDarwinCore = gbifDarwinCore,
           otherNames = all_names)

changed_names <- table[table$entry != table$gbifDarwinCore,]

other_names <- table[!is.na(table$otherNames),]



####save reptiles
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Reptiles")

write.csv(table,"Reptilia_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


####save freshwater
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Freshwater")

write.csv(table,"Freshwater_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)


#### save mammals
setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Mammals")

write.csv(table,"Mammalia_aliens_harmonised.csv",row.names = F)
write.csv(changed_names,"Taxonomic_harmonisasion_differences.csv",row.names = F)
