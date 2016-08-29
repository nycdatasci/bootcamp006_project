library(corrplot)
library(dplyr)
library(VIM)
library(e1071)
library(dplyr)
library(Hmisc)
library(VIM)
library(psych)
library(flexclust)
library(bestglm)

setwd('~/Desktop/Kaggle_higgs/Will/')
load('train.RData')
train = train[,-31]
test = read.csv("test.csv")

#four dataframes with columns removed. data manipulation

df_0 = filter(train, PRI_jet_num == 0)
df_1 = filter(train, PRI_jet_num == 1)
df_2 = filter(train, PRI_jet_num == 2)
df_3 = filter(train, PRI_jet_num == 3)

df_0['DER_mass_MMC'] = impute(df_0['DER_mass_MMC'], "random")
df_1['DER_mass_MMC'] = impute(df_1['DER_mass_MMC'], "random")
df_2['DER_mass_MMC'] = impute(df_2['DER_mass_MMC'], "random")
df_3['DER_mass_MMC'] = impute(df_3['DER_mass_MMC'], "random")

df_0 = df_0[,colSums(is.na(df_0))<nrow(df_0)]
df_1 = df_1[,colSums(is.na(df_1))<nrow(df_1)] 
df_2 = df_2[,colSums(is.na(df_2))<nrow(df_2)]
df_3 = df_3[,colSums(is.na(df_3))<nrow(df_3)]

#__________S-B distribution graphs______________#

clustering_visuals = function(k){
      zero = as.data.frame(scale(df_0[,-c(1,20,21,22)]))
      set.seed(0)
      model = kmeans(zero, centers = k, nstart = 20)
      zero$KmeansCluster = model$cluster
      zero$label = df_0$Label
      y = ceiling(k/3)
      if(k%%3==0){y=y+1}
      tab = table(zero$label)
      par(mfrow = c(y,3))
      plot(zero$label, main = paste("Overall Dist. of S/B", 
                                    "b:", 
                                    round(tab[1]/sum(tab), 3),
                                    " s:", 
                                    round(tab[2]/sum(tab), 3)))
      for(i in 1:k){
            x = filter(zero, KmeansCluster == i)
            perc_b = table(x$label)[1]/sum(table(x$label))
            perc_s = table(x$label)[2]/sum(table(x$label))
            plot(x$label, main = paste("Cluster # ", 
                                       i, 
                                       ", B:", 
                                       round(perc_b, 3), 
                                       ", S: ", 
                                       round(perc_s, 3)))
      }
}

# clustering_visuals(2)
# clustering_visuals(3)
# clustering_visuals(4)
# clustering_visuals(5)
# clustering_visuals(6)
# clustering_visuals(7)
# clustering_visuals(8)
clustering_visuals(9)



View(breakdown(15))

#_________clustering max B graph___________

clustering_graph_b = function(rangek){
      zero = as.data.frame(scale(df_0[,-c(1,20,21,22)]))
      max_list = c()
      for(i in 1:rangek){
            print(i)
            zero = as.data.frame(scale(df_0[,-c(1,20,21,22)]))
            set.seed(0)
            model = kmeans(zero, centers = i, nstart = 20)
            zero$KmeansCluster = model$cluster
            zero$label = df_0$Label
            k_b = c()
            for(n in 1:i){
                  print(paste("# of clusters =", i))
                  print(paste("cluster #", n))
                  x = filter(zero, KmeansCluster == n)
                  tab = table(x$label)
                  b = tab[1]/sum(tab)
                  print(paste('b =', b))
                  s = tab[2]/sum(tab)
                  print(paste('s =', s))
                  k_b[n] = b
                  print(paste("k_b =", k_b))}
            max_list[i] = max(k_b)
            print(paste("max of k_b = ", max(k_b)))
            print(paste('max_list = ', max_list))
      }
      max_list = as.data.frame(as.numeric(max_list))
      max_list$k = 1:nrow(max_list)
      colnames(max_list) = c("values", "k")
      g = ggplot(data = max_list, aes(x = k, y = values))
      g + geom_point() + xlab(paste("K:", " 1 -", rangek)) + ylab('maximum percent B') +
            ggtitle('Maximum within Cluster Percentage B for each K') + 
            geom_hline(yintercept = max_list$values[1])
}

#clustering_graph_b(20)

#_______________clustering max s graph__________________

clustering_graph_s = function(rangek){
      zero = as.data.frame(scale(df_0[,-c(1,20,21,22)]))
      max_list = c()
      for(i in 1:rangek){
            print(paste("K = ", i))
            zero = as.data.frame(scale(df_0[,-c(1,20,21,22)]))
            set.seed(0)
            model = kmeans(zero, centers = i, nstart = 20)
            zero$KmeansCluster = model$cluster
            zero$label = df_0$Label
            k_s = c()
            for(n in 1:i){
                  print(paste("cluster #", n))
                  x = filter(zero, KmeansCluster == n)
                  tab = table(x$label)
                  b = tab[1]/sum(tab)
                  s = tab[2]/sum(tab)
                  print(paste('S =', s))
                  k_s[n] = s
                  }
            max_list[i] = max(k_s)
            print(paste("max of k_s for K =", i, "is", max(k_s)))
      }
      max_list = as.data.frame(as.numeric(max_list))
      max_list$k = 1:nrow(max_list)
      colnames(max_list) = c("values", "k")
      g = ggplot(data = max_list, aes(x = k, y = values))
      g + geom_point() + xlab(paste("K:", " 1 -", rangek)) + ylab('Maximum Percent S') +
            ggtitle('Maximum within Cluster Percentage S for each K') + 
            geom_hline(yintercept = max_list$values[1])
}

#clustering_graph_s(40)


#___________clustering diffs graph______________

clustering_graph_s = function(rangek){
      zero = as.data.frame(scale(df_0[,-c(1,20,21,22)]))
      min_list = c()
      for(i in 1:rangek){
            print(i)
            zero = as.data.frame(scale(df_0[,-c(1,20,21,22)]))
            set.seed(0)
            model = kmeans(zero, centers = i, nstart = 20)
            zero$KmeansCluster = model$cluster
            zero$label = df_0$Label
            k_diffs = c()
            for(n in 1:i){
                  print(paste("# of clusters =", i))
                  print(paste("cluster #", n))
                  x = filter(zero, KmeansCluster == n)
                  tab = table(x$label)
                  b = tab[1]/sum(tab)
                  print(paste('b =', b))
                  s = tab[2]/sum(tab)
                  print(paste('s =', s))
                  k_diffs[n] = b - s
                  print(paste("kdiffs =", k_diffs))}
            min_list[i] = min(k_diffs)
            print(paste("min of k_diffs = ", min(k_diffs)))
            print(paste('min_list = ', min_list))
      }
      min_list = as.data.frame(as.numeric(min_list))
      min_list$k = 1:nrow(min_list)
      colnames(min_list) = c("values", "k")
      g = ggplot(data = min_list, aes(x = k, y = values))
      g + geom_point() + xlab(paste("K:", " 1 -", rangek)) + ylab('Minimum percentage difference') +
            ggtitle('Minimum within Cluster Percentage B-S Difference for each K') + 
            geom_hline(yintercept = min_list$values[1])
}

#clustering_graph_s(40)

#____________________S-B breakdown for any K__________________
breakdown = function(k){
      zero = as.data.frame(scale(df_0[,-c(1,20,21,22)]))
      set.seed(0)
      model = kmeans(zero, centers = k, nstart = 20)
      zero$KmeansCluster = model$cluster
      zero$label = df_0$Label
      df = zero%>%
            group_by(KmeansCluster)%>%
            summarise(s = sum(label=="s")/n(), b = sum(label=="b")/n())
      df
}

#__plotting breakdown w/ summarise___

plot_breakdown_max = function(k){
      max_s = c()
      max_b = c()
      for(i in 1:k){
            print(paste("When K =", i))
           bd = breakdown(i)
           max_b[i] = max(bd$b)
           max_s[i] = max(bd$s)
           print(paste("Max B =", max(bd$b), 'and Max S =', max(bd$s)))
      }
      dataf = data.frame(max_s, max_b)
      par(mfrow = c(1,2))
      plot(max_s)
      plot(max_b)
      dataf
}

#plot_breakdown_max(4)


#____________________________________________________________________
#__________________________logistic regression_______________________
#____________________________________________________________________

#fit model assess accuracy 
log = as.data.frame(scale(df_0[,-c(1, 20, 21, 22)]))
log$Label = df_0$Label
zero_logist = glm(Label ~ .,
                  family = "binomial", data = log)
test_predictions = round(zero_logist$fitted.values)
table = table(truth = log$Label, prediction = test_predictions)
table
performance = (table[1] + table[4])/sum(table)
performance
majority_guess = sum(table[1], table[3])/sum(table)
majority_guess
(performance-majority_guess)*100

#visualize logistic regression

summary(zero_logist)
coef_sort = as.data.frame(sort(zero_logist$coefficients))
par(mfrow = c(1,1))
plot(sort(zero_logist$coefficients),
     main = "Sorted, Scaled Coefficients of Saturated Logistic Regression (minus empty columns)")
plot(sort(vif(zero_logist)), main = "variance inflation factors for logistic regression")
sort(vif(zero_logist))

#removing one redundant column 
log = as.data.frame(scale(zero[,-c(46)]))
log$Label = df_0$Label
zero_logist = glm(Label ~ .,
                  family = "binomial", data = log)
plot(sort(zero_logist$coefficients),
     main = "Sorted, Scaled Coefficients of Saturated Logistic Regression (minus empty columns)")
test_predictions = round(zero_logist$fitted.values)
table = table(truth = log$Label, prediction = test_predictions)
table
performance = (table[1] + table[4])/sum(table)
performance
majority_guess = sum(table[1], table[3])/sum(table)
majority_guess
(performance-majority_guess)*100


#____________________________________________________#
