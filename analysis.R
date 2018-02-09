setwd("../Comap2018/Data")
energy <- read.csv("ProblemCData.csv", header=TRUE)
colnames(energy) <- c("MSN", "state", "year", "data")
library("tidyverse")
energy_tidy <- spread(energy, key=MSN, value=data)
energyList <- split(energy_tidy[,-1], f=energy_tidy$state)
##this is a change