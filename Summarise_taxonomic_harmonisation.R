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


for(i in 1:length(results_taxa))
{
  #count unique entries
  entries[[i]] <- nrow(results_taxa[[i]])
  
  #count unresolved names
  a <- which(is.na(results_taxa[[i]]$gbifDarwinCore))
  b <- which(results_taxa[[i]]$gbifDarwinCore == "not to species level")
  unresolved[[i]] <- length(a) + length(b)
  
  #count unique resolved names
  res <- results_taxa[[i]][which(
              !is.na(results_taxa[[i]]$gbifDarwinCore)),]
  res2 <- res[which(res$gbifDarwinCore != "not to species level"),]
  resolved[[i]] <- length(unique(res2$gbifDarwinCore))
  
  #count how many entries changed
  name_changes[[i]] <- length(which(res2$entry != res2$gbifDarwinCore)) 
}

harmonised <- data.frame(Taxon = names(taxa),
                         Entries = entries,
                         Unresolved = unresolved,
                         Hamonised_species_names = resolved,
                         Name_chages = name_changes)


setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Figures/Extended data")

write.csv(harmonised,"Ext_data_table1.csv",row.names = F)

############################# SCRAP #########################

a <- which(duplicated(results_taxa[[i]]$gbifDarwinCore))

b <- results_taxa[[i]]$gbifDarwinCore[a]

c <- which(results_taxa[[i]]$gbifDarwinCore %in% b)

results_taxa[[i]][c,]
