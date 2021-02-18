wd_external <- "/gpfs1/data/idiv_meyer/00_data/original/GBIF/26_april_2020/External_data"
wd_out <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/GloNAF_data/GBIF_selected_points"

setwd(wd_out)
#table <- read.csv("GloNAF_GBIF_occurrences.csv",nrow = 100000)
table <- read.csv("GloNAF_GBIF_occurrences.csv")

table <- table[complete.cases(table$decimalLongitude),]
table <- table[complete.cases(table$decimalLatitude),]

table <- unique(table)

setwd(wd_external)

field <- readRDS("locationID_glonafRegionID") #read table with all gbifIDs and corresponding IDs of each field
field <- field[which(field$locationID %in% table$locationID),] #select only the rows with gbifIDs corresponding to the query

tab <- merge(table,field,by="locationID",sort=F)
tab <- tab[complete.cases(tab$OBJIDsic),]

setwd(wd_out)

saveRDS(tab,"GloNAF_GBIF_shpID")
