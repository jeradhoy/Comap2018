setwd("../Comap2018/Data")
energy <- read.csv("ProblemCData.csv", header=TRUE)
colnames(energy) <- c("MSN", "state", "year", "data")
library("tidyverse")
energy_tidy <- spread(energy, key=MSN, value=data)
energyList <- split(energy_tidy[,-1], f=energy_tidy$state)


library(mgcv)

AZ.ts <- ts(energyList$AZ$RETCB, start=1960, end=2009, 1)
AZ.gam <- gam(AZ.ts~s(time(AZ.ts)), bs="ts")
plot(energyList$AZ$RETCB~energyList$AZ$year, type="b")
lines(fitted(AZ.gam)~as.vector(time(AZ.ts)), col="red")

CA.ts <- ts(energyList$CA$RETCB, start=c(1960,1), end=c(2009,1), frequency=1)
CA.gam <- gam(CA.ts~s(time(CA.ts)), bs="ts")
plot(energyList$CA$RETCB~energyList$CA$year, type="b")
lines(fitted(CA.gam)~as.vector(time(CA.ts)))

NM.ts <- ts(energyList$NM$RETCB, start=1960, end=2009, 1)
NM.gam <- gam(NM.ts~s(time(NM.ts)), bs="ts")
plot(energyList$NM$RETCB~energyList$NM$year, type="b")
lines(fitted(NM.gam)~as.vector(time(NM.ts)), col="red")


TX.ts <- ts(energyList$TX$RETCB, start=1960, end=2009, 1)
TX.gam <- gam(TX.ts~s(time(TX.ts)), bs="ts")
plot(energyList$TX$RETCB~energyList$TX$year, type="b")
lines(fitted(TX.gam)~as.vector(time(TX.ts)))
