#Revanth Korrapolu
#rrk69

census_data <- read.csv("CENSUSNEW.csv")

#Unproportional amount of Fire people have status = "never worked" but they have profession. Predominantly
#from india. Varying education from 5th grade to phd. Probably typos.
mosaicplot(census_data$STATUS~census_data$ELEMENTS, shade=TRUE, main="Element vs Status")

#Plot of Element vs Profession
library(ggplot2)

#ggplot function - Did not implement
ggMMplot <- function(var1, var2){
       require(ggplot2)
       levVar1 <- length(levels(var1))
       levVar2 <- length(levels(var2))
       
         jointTable <- prop.table(table(var1, var2))
         plotData <- as.data.frame(jointTable)
         plotData$marginVar1 <- prop.table(table(var1))
         plotData$var2Height <- plotData$Freq / plotData$marginVar1
         plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
               plotData$marginVar1 / 2
         
           ggplot(plotData, aes(var1Center, var2Height)) +
               geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
               geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) 
}

ggMMplot(census_data$ELEMENTS, census_data$PROFESSION) + xlab("Elements") + ylab("Frequency of Professions")


#Mean of Gains by Element
tapply(census_data$CAPITALGAINS,census_data$ELEMENTS,mean)
#Fire Elements have high capital gains

#Mean of Losses by Element
tapply(census_data$CAPITALLOSS,census_data$ELEMENTS,mean)
#Earth Elements has low capital losses

Fire.data <- subset(census_data, census_data$ELEMENTS == "Fire")
Fire.CapitalGains <- Fire.data$CAPITALGAINS

sd.Fire <- sd(Fire.CapitalGains)
sd.all <- sd(census_data$CAPITALGAINS)

mean.Fire <- mean(Fire.CapitalGains)
mean.all <- mean(census_data$CAPITALGAINS)

sd.Fire.all <- sqrt(sd.Fire^2/len_Fire + sd.all^2/len_all)

zeta <- (mean.Fire - mean.all)/sd.Fire.all

p = 1-pnorm(zeta)

#p = 5.084821e-14




