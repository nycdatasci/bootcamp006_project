# David Richard Steinmetz
# NYCDSA Project 4 - Kaggle Machine Learning


# Libraries ---------------------------------------------------------------

library(ggplot2)
library(drs)
library(gridExtra)
loadlib('gtools')


# Functions ---------------------------------------------------------------

#A function to help determine the number of clusters when we do not have an
#idea ahead of time.
wssplot = function(data, nc = 15, seed = 0) {
  wss = (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    cat('Calculating within cluster sum of squares for', i, 'clusters\n')
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers = i, iter.max = 10, nstart = 10)$withinss)
  }  # withinss = within cluster sum of squares
  plot(1:nc, wss, type = "b",
       xlab = "Number of Clusters",
       ylab = "Within-Cluster Variance",
       main = "Scree Plot for the K-Means Procedure")
}


# Preprocess Data ---------------------------------------------------------

# Scale the data to use with k-means
to_scale <- train[,-c('EventId', 'Weight', 'Label'),with=FALSE]
scale.sm <- as.data.table(scale(to_scale))


# Determine K -------------------------------------------------------------

wssplot(scale.sm)


# K-Means Clustering ------------------------------------------------------

# Take complete cases 
# - kmeans doesn't work with missing values
# - in this case imputation is not a good choice because of the amount of NA's
scale.sm <- scale.sm[complete.cases(scale.sm)]

# Create k-means model
set.seed(0)
km.train <- kmeans(scale.sm, centers = 4, nstart = 2)  # Use 4 clusters
system.time(kmeans(scale.sm, centers = 4, nstart = 10))


# Plotting ----------------------------------------------------------------

# Create combinations of two variables for scatterplots to investigate k-means
combinations <- combinations(n = length(names(scale.sm)), 
                             r = 2, 
                             v = names(scale.sm), 
                             repeats.allowed = FALSE)  # Do not plot var1/var1

# Subset dataset to plot quicker, run again to update plots of new trained model
per <- 0.01  # Percentage of points to use
set.seed(0)
rows <- sample(1:nrow(scale.sm), size = (per * nrow(scale.sm)))
to_plot <- scale.sm[rows,]
km.to_plot <- km.train$cluster[rows]

# Create scatterplots to investigate k-means results, store in a list
g <- list()
for (i in 1:(nrow(combinations))){
  g[[i]] <- ggplot(to_plot, 
                   aes_string(x = combinations[i,1], 
                              y = combinations[i,2]))
  g[[i]] <- g[[i]] + geom_point(aes(colour = as.factor(km.to_plot)))
  g[[i]] <- g[[i]] + scale_colour_discrete(name = 'Cluster')
  g[[i]] <- g[[i]] + theme_minimal()
}

# Iterate through plots to see if any are useful
# for (i in 1:length(combinations)){
#   print(g[[i]])
#   if (substr(readkey(),1,1)=='q') break
# }


# Iterate through plots to see if any are useful / 2x2 grid
for (i in 1:(nrow(combinations)/4)){
  a <- (i-1)*4
  if (length(g) >= a+4){
    grid.arrange(g[[a+1]], g[[a+2]], g[[a+3]], g[[a+4]], nrow=2, ncol=2)
  } else if ((a+4 - length(g))==3) {
    grid.arrange(g[[a+1]], g[[a+2]], g[[a+3]], nrow=2, ncol=2)
  } else if ((a+4 - length(g))==2) {
    grid.arrange(g[[a+1]], g[[a+2]], nrow=2, ncol=2)
  } else if ((a+4 - length(g))==1) {
    grid.arrange(g[[a+1]], nrow=2, ncol=2)
  }
  if (substr(readkey(),1,1)=='q') break
}


# Create plotting combinations for a particular variable
to_find <- 'DER_prodeta_jet_jet'
comb.dt <- as.data.table(combinations)
combinations <- as.matrix(comb.dt[V1==to_find | V2==to_find])
colnames(combinations) <- NULL
