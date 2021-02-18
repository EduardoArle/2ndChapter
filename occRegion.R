library("plyr")

setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/GloNAF_data/GBIF_selected_points")

table <- readRDS("GloNAF_GBIF_shpID")

table2 <- ddply(table,.(species,year,OBJIDsic), nrow)

saveRDS(table2,"Occurrence_region_count")