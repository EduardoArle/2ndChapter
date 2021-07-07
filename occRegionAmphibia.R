library("plyr");library("data.table")

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Amphibia_data")

#read table in
table <- read.csv("Alien_amphibian_GBIF_occurrences.csv")

#eliminate fossil specimens
table2 <- table[-which(table$basisOfRecord == "FOSSIL_SPECIMEN"),]

#select unique occurrences by location and time
table3 <- unique(as.data.table(table2),
                 by=c("locationID","temporalID","speciesID"))

#count observations per species per year per region
table4 <- ddply(table3,.(species,year,BENTITY2_N), nrow)

saveRDS(table2,"Amphibia_occurrence_region_count")