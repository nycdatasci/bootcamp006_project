library(dplyr)
library(rworldmap)

aps <- read.csv('Data/APS2015.csv', header = TRUE, stringsAsFactors = FALSE, strip.white=TRUE)
nes <- read.csv('Data/NES2015.csv', header = TRUE, stringsAsFactors = FALSE, check.names = TRUE, strip.white=TRUE)
nescols <-names(nes)
nescols <- gsub('\\.', ' ', nescols)
colnames(nes) <- nescols
nes <- nes[,-1]


worldaps <- joinCountryData2Map(aps, joinCode = "NAME", nameJoinColumn = "Economy")
worldnes <- joinCountryData2Map(nes, joinCode = "NAME", nameJoinColumn = "Economy")

nesRadar <- rbind(rep(8, 14), rep(0, 14), nes)
