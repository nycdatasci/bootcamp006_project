setwd('~/Desktop/Data_Science/GCS_Folder/Rdata_Files/')
load('first_50.RData')
load('first_72.RData')


diffs = c()
for(i in 2:73){
      check = first_72
      check$diff = abs(check$last_score - check[,i])
      diff = mean(check$diff)
      diffs = append(diffs, diff)
}

data = data.frame(diffs = diffs, index = c(1:72))
ggplot(data = data, aes(x = index, y = diffs)) + geom_line() +
      ggtitle('Average Difference Between Current Score and Last Score \nFor Hours Since Minimum Score') +
      ylab('GCS Score Difference') +
      xlab('Hours Since Minimum Score')

sames = c()
for(i in 2:73){
      check = first_72
      check$same = check$last_score == check[,i]
      check$same = ifelse(check$same==T, 1, 0)
      same = mean(check$same)
      sames = append(sames, same)
}
data = data.frame(sames = sames, index = c(1:72))
ggplot(data = data, aes(x = index, y = sames)) + geom_line() +
      ggtitle('Probability that Current GCS Score = Last GCS score \nFor Hours Since Minimum Score') +
      ylab('Probability') +
      xlab('Hours Since Minimum Score')


