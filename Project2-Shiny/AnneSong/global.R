# convert matrix to dataframe
# US_StateStat <- data.frame(state.name = rownames(state.x77), state.x77)
# remove row names
test = US_StateStat
US_Stat = test
rownames(US_Stat) <- NULL
# create variable with colnames as choice
choice <- colnames(US_Stat)[-1]