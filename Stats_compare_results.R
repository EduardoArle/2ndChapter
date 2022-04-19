library(multcomp);library(sandwich);library(data.table)


#paths
wd_results <- "C:/Users/ca13kute/Documents/2nd_Chapter/Results"
wd_test_res <- "C:/Users/ca13kute/Documents/2nd_Chapter/Figures/SI/Tukey_contrasts"

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

#include a column in the results tables indicating the taxon

for(i in 1:length(results))
{
  results[[i]] <- cbind(Taxon = taxa[i],results[[i]])
}

#join all results in one table

results2 <- rbindlist(results)
results2$Taxon <- as.factor(results2$Taxon)
results2$continent <- as.factor(results2$continent)

####################### CONFIRMATION #########################


#Fit an Analysis of Variance Model for the completeness indicator

amod <- aov(confirmed ~ Taxon, data = results2)

#run the General Linear Hypotheses

amod_glht <- glht(amod, mcp(Taxon = "Tukey"),
                  vcov = vcovHC)

#get summary of the analysis
result_conf <- summary(amod_glht)

#find a way of transforming this s#### into a data.frame
output <- capture.output(result_conf, file=NULL,append=FALSE)

output2 <- output[c(11:46)] #select only the positions with results

#split strings to make a data frame
rows_res <- list()
for(i in 1:length(output2))
{
  a <- output2[i] #select each string
  b <- gsub("([0-9])( )","\\1,",a) #sub each "number + space" for "number + ,"
  c <- gsub(" ","",b) #eliminate spaces
  rows_res[[i]] <- strsplit(c,",")[[1]] #split strings using "," as separator
}

#make a matrix to input the rows with resuts
matrix_res <- matrix(nrow = length(rows_res),
                            ncol = 6)

for(i in 1:nrow(matrix_res))
{
  if(length(rows_res[[i]]) == 5){
    rows_res[[i]] <- c(rows_res[[i]],NA)
  }
  matrix_res[i,] <- rows_res[[i]]
}

matrix_res <- as.data.frame(matrix_res)

names(matrix_res) <- c("Taxa","Estimate","Std. Error",
                       "t value","Pr(>|t|)","Significant")

setwd(wd_test_res)
write.csv(matrix_res,"Tukey_contrasts_confirmation.csv",row.names = F)



####################### MODELLING #########################


#Fit an Analysis of Variance Model for the completeness indicator

amod <- aov(modelling ~ Taxon, data = results2)

#run the General Linear Hypotheses

amod_glht <- glht(amod, mcp(Taxon = "Tukey"),
                  vcov = vcovHC)

#get summary of the analysis
result_conf <- summary(amod_glht)

#find a way of transforming this s#### into a data.frame
output <- capture.output(result_conf, file=NULL,append=FALSE)

output2 <- output[c(11:46)] #select only the positions with results

#split strings to make a data frame
rows_res <- list()
for(i in 1:length(output2))
{
  a <- output2[i] #select each string
  b <- gsub("([0-9])( )","\\1,",a) #sub each "number + space" for "number + ,"
  c <- gsub(" ","",b) #eliminate spaces
  rows_res[[i]] <- strsplit(c,",")[[1]] #split strings using "," as separator
}

#make a matrix to input the rows with resuts
matrix_res <- matrix(nrow = length(rows_res),
                     ncol = 6)

for(i in 1:nrow(matrix_res))
{
  if(length(rows_res[[i]]) == 5){
    rows_res[[i]] <- c(rows_res[[i]],NA)
  }
  matrix_res[i,] <- rows_res[[i]]
}

matrix_res <- as.data.frame(matrix_res)

names(matrix_res) <- c("Taxa","Estimate","Std. Error",
                       "t value","Pr(>|t|)","Significant")

setwd(wd_test_res)
write.csv(matrix_res,"Tukey_contrasts_modelling.csv",row.names = F)



####################### RANGE DYNAMICS #########################


#Fit an Analysis of Variance Model for the completeness indicator

amod <- aov(Rd ~ Taxon, data = results2)

#run the General Linear Hypotheses

amod_glht <- glht(amod, mcp(Taxon = "Tukey"),
                  vcov = vcovHC)

#get summary of the analysis
result_conf <- summary(amod_glht)

#find a way of transforming this s#### into a data.frame
output <- capture.output(result_conf, file=NULL,append=FALSE)

output2 <- output[c(11:46)] #select only the positions with results

#split strings to make a data frame
rows_res <- list()
for(i in 1:length(output2))
{
  a <- output2[i] #select each string
  b <- gsub("([0-9])( )","\\1,",a) #sub each "number + space" for "number + ,"
  c <- gsub(" ","",b) #eliminate spaces
  rows_res[[i]] <- strsplit(c,",")[[1]] #split strings using "," as separator
}

#make a matrix to input the rows with resuts
matrix_res <- matrix(nrow = length(rows_res),
                     ncol = 6)

for(i in 1:nrow(matrix_res))
{
  if(length(rows_res[[i]]) == 5){
    rows_res[[i]] <- c(rows_res[[i]],NA)
  }
  matrix_res[i,] <- rows_res[[i]]
}

matrix_res <- as.data.frame(matrix_res)

names(matrix_res) <- c("Taxa","Estimate","Std. Error",
                       "t value","Pr(>|t|)","Significant")

setwd(wd_test_res)
write.csv(matrix_res,"Tukey_contrasts_range_dynamics.csv",row.names = F)


################  SUB-CONTINENT COMPARISON ################

#Fit an Analysis of Variance Model for the completeness indicator

amod <- aov(confirmed ~ continent, data = results2)

#run the General Linear Hypotheses

amod_glht <- glht(amod, mcp(continent = "Tukey"),
                  vcov = vcovHC)

#get summary of the analysis
result_conf <- summary(amod_glht)

#find a way of transforming this s#### into a data.frame
output <- capture.output(result_conf, file=NULL,append=FALSE)

output2 <- output[c(473:625)] #select only the positions with results

#split strings to make a data frame
rows_res <- list()
for(i in 1:length(output2))
{
  a <- output2[i] #select each string
  b <- gsub("([0-9])( )","\\1,",a) #sub each "number + space" for "number + ,"
  c <- gsub(" ","",b) #eliminate spaces
  rows_res[[i]] <- strsplit(c,",")[[1]] #split strings using "," as separator
}

#make a matrix to input the rows with resuts
matrix_res <- matrix(nrow = length(rows_res),
                     ncol = 2)

for(i in 1:nrow(matrix_res))
{
  matrix_res[i,] <- rows_res[[i]]
}

matrix_res <- as.data.frame(matrix_res)

names(matrix_res) <- c("Sub continent","Significant")

setwd(wd_test_res)
write.csv(matrix_res,"Tukey_contrasts_confirmation_continent.csv",row.names = F)



####################### MODELLING #########################


#Fit an Analysis of Variance Model for the completeness indicator

amod <- aov(modelling ~ continent, data = results2)

#run the General Linear Hypotheses

amod_glht <- glht(amod, mcp(continent = "Tukey"),
                  vcov = vcovHC)

#get summary of the analysis
result_conf <- summary(amod_glht)

#find a way of transforming this s#### into a data.frame
output <- capture.output(result_conf, file=NULL,append=FALSE)

output2 <- output[c(473:625)] #select only the positions with results

#split strings to make a data frame
rows_res <- list()
for(i in 1:length(output2))
{
  a <- output2[i] #select each string
  b <- gsub("([0-9])( )","\\1,",a) #sub each "number + space" for "number + ,"
  c <- gsub(" ","",b) #eliminate spaces
  rows_res[[i]] <- strsplit(c,",")[[1]] #split strings using "," as separator
}

#make a matrix to input the rows with resuts
matrix_res <- matrix(nrow = length(rows_res),
                     ncol = 2)

for(i in 1:nrow(matrix_res))
{
  matrix_res[i,] <- rows_res[[i]]
}

matrix_res <- as.data.frame(matrix_res)

names(matrix_res) <- c("Sub continent","Significant")

setwd(wd_test_res)
write.csv(matrix_res,"Tukey_contrasts_modelling_continent.csv",row.names = F)



####################### RANGE DYNAMICS #########################


#Fit an Analysis of Variance Model for the completeness indicator

amod <- aov(Rd ~ continent, data = results2)

#run the General Linear Hypotheses

amod_glht <- glht(amod, mcp(continent = "Tukey"),
                  vcov = vcovHC)

#get summary of the analysis
result_conf <- summary(amod_glht)

#find a way of transforming this s#### into a data.frame
output <- capture.output(result_conf, file=NULL,append=FALSE)

output2 <- output[c(473:625)] #select only the positions with results

#split strings to make a data frame
rows_res <- list()
for(i in 1:length(output2))
{
  a <- output2[i] #select each string
  b <- gsub("([0-9])( )","\\1,",a) #sub each "number + space" for "number + ,"
  c <- gsub(" ","",b) #eliminate spaces
  rows_res[[i]] <- strsplit(c,",")[[1]] #split strings using "," as separator
}

#make a matrix to input the rows with resuts
matrix_res <- matrix(nrow = length(rows_res),
                     ncol = 2)

for(i in 1:nrow(matrix_res))
{
  matrix_res[i,] <- rows_res[[i]]
}

matrix_res <- as.data.frame(matrix_res)

names(matrix_res) <- c("Sub continent","Significant")

setwd(wd_test_res)
write.csv(matrix_res,"Tukey_contrasts_range_dynamics_continent.csv",row.names = F)
