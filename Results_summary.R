library(plyr);library(data.table)

#paths
wd_results <- "C:/Users/ca13kute/Documents/2nd_Chapter/Results"

#list taxa
setwd(wd_results)
taxa <- list.files()

#load results for all taxa
results <- list()

for(i in 1:length(taxa))
{
  setwd(paste0(wd_results,"/",taxa[i],"/Tables"))
  results[[i]] <- read.csv(list.files()[1])
}

names(results) <- taxa
results2 <- results

#include taxon name in each table
for(i in 1:length(results2))
{
  results2[[i]] <- cbind(Taxon=names(results2)[i],results2[[i]])
}

#join all results in one table
results3 <- rbindlist(results2)

#calculate average and SD of indicators per taxon

means_SD <- ddply(results3,.(Taxon),
                    summarise, 
                    mean_confirmed = mean(confirmed,na.rm=T),
                    SD_confirmed = sd(confirmed,na.rm=T),
                    mean_modelling = mean(modelling,na.rm=T),
                    SD_modelling = sd(modelling,na.rm=T),
                    mean_Rd = mean(Rd,na.rm=T),
                    SD_Rd = sd(Rd,na.rm=T),
                    entries = sum(n_sps))

setwd(wd_results)
write.csv(means_SD,"Means_SD_results.csv")
