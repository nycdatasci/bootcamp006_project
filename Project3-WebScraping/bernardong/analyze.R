library ("dplyr")
library ("wordcloud")
library ("tm")

# read in the review corpus
ebookt <- read.csv(file.choose(),sep=",",header=TRUE)

# choose the top 20 words for the ebook
head(ebookt %>% mutate(score=1*tr1+2*tr2+3*tr3+4*tr4+5*tr5) %>% filter(title!="") %>% arrange(-score),200) 

# create a word cloud
tcloud <- head(ebookt %>% mutate(score=1*tr1+2*tr2+3*tr3+4*tr4+5*tr5) %>% filter(title!="") %>% arrange(-score),200)[,-2:-6]
cloudstr <- ""

# save the top 200 wordlist - with scores
write.csv(tcloud,file="top200_countscores.csv",row.names=FALSE,quote=FALSE)

# save the top 200 wordlist - without scores
write.csv(tcloud[1],file="top200_count.csv",row.names=FALSE,quote=FALSE)

# generate the word cloud as needed
for (x in 1:50) {
  for (y in 1:tcloud[x,2]) {cloudstr <- paste0(cloudstr," ",tcloud[x,1])}
}

wordcloud(cloudstr, scale=c(5,0.5), max.words=50, random.order=FALSE, 
  rot.per=0.35, use.r.layout=FALSE, 
  colors=c("black","brown","blue","orange","green","pink","purple","red"))

# read in the price, rating, review count
prrno <- read.csv(file.choose(),sep=",",header=TRUE)
summary(prrno)
plot(prrno)
