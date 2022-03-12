### AMPHIBIANS

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Amphibia_data")

amphs <- readRDS("Amphibia_occurrence_region_count")

n_amphs <- length(unique(amphs$species))
n_amphs

recs_amphs <- sum(amphs$V1)
recs_amphs

### ANTS

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Ants")

ants <- readRDS("Ants_occurrence_region_count")

n_ants <- length(unique(ants$species))
n_ants

recs_ants <- sum(ants$V1)
recs_ants

### BIRDS

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/GAVIA_data")

birds <- readRDS("Birds_occurrence_region_count")

n_birds <- length(unique(birds$species))
n_birds

recs_birds <- sum(birds$V1)
recs_birds

### FRESHWATER FISH

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Freshwater")

fresh <- readRDS("Freshwater_occurrence_region_count")

n_fresh <- length(unique(fresh$species))
n_fresh

recs_fresh <- sum(fresh$V1)
recs_fresh

### FUNGI

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Fungi")

fungi <- readRDS("Fungi_occurrence_region_count")

n_fungi <- length(unique(fungi$species))
n_fungi

recs_fungi <- sum(fungi$V1)
recs_fungi


### MAMMALS

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/DAMA")

mammals <- readRDS("Mammals_occurrence_region_count")

n_mammals <- length(unique(mammals$species))
n_mammals

recs_mammals <- sum(mammals$V1)
recs_mammals


### PLANTS

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/GloNAF_data")

plants <- readRDS("Plants_occurrence_region_count")

n_plants <- length(unique(plants$species))
n_plants

recs_plants <- sum(plants$V1)
recs_plants


### REPTILES

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Reptilia_data")

reps <- readRDS("Reptilia_occurrence_region_count")

n_reps <- length(unique(reps$species))
n_reps

recs_reps <- sum(reps$V1)
recs_reps


### SPIDERS

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Spiders")

spiders <- readRDS("Spiders_occurrence_region_count")

n_spiders <- length(unique(spiders$species))
n_spiders

recs_spiders <- sum(spiders$V1)
recs_spiders


###### Make table

table <- data.frame(Taxon=c('Birds','Mammals','Amphibians',
                            'Repties','Freshwater Fish','Ants',
                            'Spiders','Plants','Fungi'),
                    N_records=c(recs_birds,recs_mammals,
                                recs_amphs,recs_reps,
                                recs_fresh,recs_ants,
                                recs_spiders,recs_plants,
                                recs_fungi))

setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Tables")

write.csv(table,"Number_of_records.csv",row.names = F)
