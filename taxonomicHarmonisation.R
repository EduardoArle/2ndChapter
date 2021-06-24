#load libraries
library(taxize)

#set paths
wd_species <- "C:/Users/ca13kute/Documents/3rd_Chapter/Amphibians"

#read species table
setwd(wd_species)
list <- read.csv("Amphibia_species_list.csv")
list <- list[,1]

#make list of GBIF resolved names
gbifDarwinCore <- character()

t <- numeric()

for(i in 1:length(list))
{
  match <- resolve(list[i],db = "gnr")
  match2 <- as.data.frame(match[[1]])
  match3 <- match2[which(match2$data_source_title == "GBIF Backbone Taxonomy"),]
  t[i] <- nrow(match3)
  if(nrow(match3) == 0){
    gbifDarwinCore[i] <- "not matched"
  }else{
    gbifDarwinCore[i] <- match3$matched_name
  }
  print(i)
}

resolve(list[73],db = "gnr")

comp <- data.frame(original=list,resolved=gbifDarwinCore)


as.data.frame(resolve("Pelophylax kl. grafi",db = "gnr")[[1]])


setwd("C:/Users/ca13kute/Documents/2nd_Chapter/Amphibians and Reptiles/0309231-200613084148143")

tab <- read.csv("0309231-200613084148143.csv",sep="\t",encoding = "UTF-8")

grep("Pelophylax kl. grafi",tab$scientificName)

unique(tab$scientificName)

head(tab)
nrow(tab)

list[which(t == 0)]

which(t == 0)
which(t == 1)
which(t > 1)

length(t)
t[[1]]$data_source_title

class(t[[1]])
