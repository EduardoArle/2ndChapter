wd_birds <- ("C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA")
wd_plants <- ("C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GLONAF")

setwd(wd_birds)
birds <- read.csv("Final_checklist_birds.csv")
birds_antactica <- birds[grep("ANTARCTICA",birds$GAVIARegion),]
birds_antactica_list <- unique(birds_antactica$Binomial)

setwd(wd_plants)
plants <- read.csv("Final_checklist_plants.csv")
plants_antarctica <- plants[which(plants$name == "Antarctica"),]
plants_antarctica_list <- unique(plants_antarctica$standardized_name)




