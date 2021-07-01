#load libraries
library(taxize)

#set paths
wd_species <- "C:/Users/ca13kute/Documents/3rd_Chapter/Amphibians"

#read species table
setwd(wd_species)
list <- read.csv("Amphibia_species_list.csv")
list <- sort(list[,1])

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