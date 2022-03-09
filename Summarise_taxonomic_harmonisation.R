# set paths 

Birds <- "C:/Users/ca13kute/Documents/2nd_Chapter/GAVIA"
Mammals <- "C:/Users/ca13kute/Documents/2nd_Chapter/Mammals"
Amphibians <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Amphibians"
Reptiles <- "C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/Reptiles"
Freshwater_Fish <- "C:/Users/ca13kute/Documents/2nd_Chapter/Freshwater"
Ants <- "C:/Users/ca13kute/Documents/2nd_Chapter/Ants"
Spiders <- "C:/Users/ca13kute/Documents/2nd_Chapter/Spiders"
Plants <- "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GLONAF"
Fungi <- "C:/Users/ca13kute/Documents/2nd_Chapter/Fungi"

taxa <- list(Birds,Mammals,Amphibians,Reptiles,
             Freshwater_Fish,Ants,Spiders,Plants,Fungi)

names(taxa) <- c("Birds","Mammals","Amphibians","Reptiles",
                 "Freshwater_Fish","Ants","Spiders","Plants","Fungi")

results_taxa <- list()
for(i in 1:length(taxa))
{
  setwd(taxa[[i]])
  results_taxa[[i]] <- read.csv(list.files(pattern = "aliens_harmonised.csv"))
}

names(results_taxa) <- c("Birds","Mammals","Amphibians","Reptiles",
                 "Freshwater_Fish","Ants","Spiders","Plants","Fungi")


### Count names that changed and unresolved ones
entries <- numeric()
unresolved <- numeric()
resolved <- numeric()
name_changes <- numeric()

#####  INCLUDE CALCULATIONS not to species level #####

for(i in 1:length(results_taxa))
{
  #count unique entries
  entries[[i]] <- nrow(results_taxa[[i]])
  
  #count unresolved names
  unresolved[[i]] <- length(which(
    is.na(results_taxa[[i]]$gbifDarwinCore)))
  
  #count unique resolved names
  resolved[[i]] <- length(unique(results_taxa[[i]]$gbifDarwinCore))
  resolved[[i]] <- ifelse(unresolved[[i]] > 0,
                          resolved[[i]] - 1,resolved[[i]])
  
  #count how many entries changed
  name_changes[[i]] <- length(which(results_taxa[[i]]$entry != 
                                      results_taxa[[i]]$gbifDarwinCore)) 
}



a <- which(duplicated(results_taxa[[i]]$gbifDarwinCore))

b <- results_taxa[[i]]$gbifDarwinCore[a]

c <- which(results_taxa[[i]]$gbifDarwinCore == b)

results_taxa[[i]][c,]
