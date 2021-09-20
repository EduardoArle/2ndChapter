library(plyr)

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

#calculate average indicators per continent
means <- list()

for(i in 1:length(results))
{
        means[[i]] <- ddply(results[[i]],.(continent),
                  summarise, 
                  confirmed = mean(confirmed,na.rm=T),
                  modeling = mean(modelling,na.rm=T),
                  Rd = mean(Rd,na.rm=T),
                  entries = sum(n_sps))
}

names(means) <- names(results)

#make one table for each indicator

confirmed <- means[[1]][,c(1,2)]

names(confirmed)[2] <- names(means)[1]

for(i in 2:length(means))
{
        a <- means[[i]][,c(1,2)]
        names(a)[2] <- names(means)[i]
        
        confirmed <- merge(confirmed,a,by="continent",all.x=T)
}

confirmed$cont_short <- confirmed$continent
confirmed$cont_short[c(4,6)] <- c("Western Europe","East Africa")


modelling <- means[[1]][,c(1,3)]

names(modelling)[2] <- names(means)[1]

for(i in 2:length(means))
{
        a <- means[[i]][,c(1,3)]
        names(a)[2] <- names(means)[i]
        
        modelling <- merge(modelling,a,by="continent",all.x=T)
}

modelling$cont_short <- modelling$continent
modelling$cont_short[c(4,6)] <- c("Western Europe","East Africa")


rd <- means[[1]][,c(1,4)]

names(rd)[2] <- names(means)[1]

for(i in 2:length(means))
{
        a <- means[[i]][,c(1,4)]
        names(a)[2] <- names(means)[i]
        
        rd <- merge(rd,a,by="continent",all.x=T)
}

rd$cont_short <- rd$continent
rd$cont_short[c(4,6)] <- c("Western Europe","East Africa")



### plot results represented by colour in a table (all in base, bitch)

#create colour ramp to represent the values
colramp <- colorRampPalette(c("#B70101", "#F51616", "#F67A7A",
                              "#CE9A91", "#98A5B6",
                              "#806FEC", "#3F3FE4", "#0202A1"))


### CONFIRMED ####


#populate the table with the colours to be plotted 

col_conf <- confirmed[,-ncol(confirmed)]  #make a copy of the table

for(i in 2:ncol(col_conf))
{
        col_values <- colramp(100)[cut(c(0, 100, col_conf[,i]), breaks = 100)]
        col_values2 <- col_values[-c(1, 2)] 
        col_values2[is.na(col_values2)] <- "white" #make NAs grey
        col_conf[,i] <- col_values2
}

par(mar=c(6,6,1,3))

#make the empty plot
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10),
     xaxs = "i",yaxs = "i", axes=F, frame.plot=TRUE)

#decide how many rows and cols the table needs
rows <- 6
cols <- 18

#make lines creating a table (cols)
for(i in 1:(cols-1))
{
        a <- c(i*10/cols,i*10/cols,i*10/cols)
        b <- c(0,5,10)
        lines(a,b)
}

#make lines creating a table (rows)
for(i in 1:(rows-1))
{
        a <- c(0,5,10)
        b <- c(i*10/rows,i*10/rows,i*10/rows)
        lines(a,b)
}

#plot squares with colours in the results table
for(i in 1:rows)
{
        for(j in 1:cols)
        {
                points((10/cols/2)+(10/cols*(j-1)),
                       (10/rows/2)+(10/rows*(i-1)),
                       bg = col_conf[j,i+1],
                       pch = 22, cex = 5)
        }
}

#add axes
axis(side = 1, 
     at = seq(10/cols/2,(10/cols/2)+(10/cols*(cols-1)),by = 10/cols),
     labels = confirmed$cont_short, cex.axis = .8, padj = 0, las =2)


axis(side = 2, 
     at = seq(10/rows/2,(10/rows/2)+(10/rows*(rows-1)),by = 10/rows),
     labels = names(confirmed)[-c(1,8)], cex.axis = 1, padj = 0, las =1)


myGradientLegend(valRange = c(0, 100),
                 pos=c(.2,-.060,.9,-.030),
                 color = colramp(20),
                 side = 1,
                 n.seg = 0,
                 values = c("0","100%"),
                 cex = 1)


#populate the table with the colours to be plotted 

col_mod <- modelling[,-ncol(modelling)]  #make a copy of the table

for(i in 2:ncol(col_mod))
{
        col_values <- colramp(100)[cut(c(0, 100, col_mod[,i]), breaks = 100)]
        col_values2 <- col_values[-c(1, 2)] 
        col_values2[is.na(col_values2)] <- "white" #make NAs grey
        col_mod[,i] <- col_values2
}

par(mar=c(6,6,1,3))

#make the empty plot
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10),
     xaxs = "i",yaxs = "i", axes=F, frame.plot=TRUE)

#decide how many rows and cols the table needs
rows <- 6
cols <- 18

#make lines creating a table (cols)
for(i in 1:(cols-1))
{
        a <- c(i*10/cols,i*10/cols,i*10/cols)
        b <- c(0,5,10)
        lines(a,b)
}

#make lines creating a table (rows)
for(i in 1:(rows-1))
{
        a <- c(0,5,10)
        b <- c(i*10/rows,i*10/rows,i*10/rows)
        lines(a,b)
}

#plot squares with colours in the results table
for(i in 1:rows)
{
        for(j in 1:cols)
        {
                points((10/cols/2)+(10/cols*(j-1)),
                       (10/rows/2)+(10/rows*(i-1)),
                       bg = col_mod[j,i+1],
                       pch = 22, cex = 5)
        }
}

#add axes
axis(side = 1, 
     at = seq(10/cols/2,(10/cols/2)+(10/cols*(cols-1)),by = 10/cols),
     labels = modelling$cont_short, cex.axis = .8, padj = 0, las =2)


axis(side = 2, 
     at = seq(10/rows/2,(10/rows/2)+(10/rows*(rows-1)),by = 10/rows),
     labels = names(modelling)[-c(1,8)], cex.axis = 1, padj = 0, las =1)


myGradientLegend(valRange = c(0, 100),
                 pos=c(.2,-.060,.9,-.030),
                 color = colramp(20),
                 side = 1,
                 n.seg = 0,
                 values = c("0","100%"),
                 cex = 1)


#populate the table with the colours to be plotted 

col_rd <- rd[,-ncol(rd)]  #make a copy of the table

for(i in 2:ncol(col_rd))
{
        col_values <- colramp(100)[cut(c(0, 100, col_rd[,i]), breaks = 100)]
        col_values2 <- col_values[-c(1, 2)] 
        col_values2[is.na(col_values2)] <- "white" #make NAs grey
        col_rd[,i] <- col_values2
}

par(mar=c(6,6,1,3))

#make the empty plot
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10),
     xaxs = "i",yaxs = "i", axes=F, frame.plot=TRUE)

#decide how many rows and cols the table needs
rows <- 6
cols <- 18

#make lines creating a table (cols)
for(i in 1:(cols-1))
{
        a <- c(i*10/cols,i*10/cols,i*10/cols)
        b <- c(0,5,10)
        lines(a,b)
}

#make lines creating a table (rows)
for(i in 1:(rows-1))
{
        a <- c(0,5,10)
        b <- c(i*10/rows,i*10/rows,i*10/rows)
        lines(a,b)
}

#plot squares with colours in the results table
for(i in 1:rows)
{
        for(j in 1:cols)
        {
                points((10/cols/2)+(10/cols*(j-1)),
                       (10/rows/2)+(10/rows*(i-1)),
                       bg = col_rd[j,i+1],
                       pch = 22, cex = 5)
        }
}

#add axes
axis(side = 1, 
     at = seq(10/cols/2,(10/cols/2)+(10/cols*(cols-1)),by = 10/cols),
     labels = rd$cont_short, cex.axis = .8, padj = 0, las =2)


axis(side = 2, 
     at = seq(10/rows/2,(10/rows/2)+(10/rows*(rows-1)),by = 10/rows),
     labels = names(rd)[-c(1,8)], cex.axis = 1, padj = 0, las =1)


myGradientLegend(valRange = c(0, 100),
                 pos=c(.2,-.060,.9,-.030),
                 color = colramp(20),
                 side = 1,
                 n.seg = 0,
                 values = c("0","100%"),
                 cex = 1)

