# IMPORT DATA
TD <- read.csv("TaiwanCreditData.csv", header=TRUE)

# TOPLINE CHECK OF DATA
sum(is.na(TD))
p <- names(TD)
str(TD)
summary(TD)

# CONVERT CATEGORICAL VARIABLES TO FACTORS
TDF = TD
for (i in c(3:5,7:12,25)) {TDF[,i] = as.factor(TD[,i])}

# CHECK INTEGRITY OF CONVERSION TO FACTORS
if(sum(TDF[,c(3:5,7:12,25)]!=TD[,c(3:5,7:12,25)])!= 0) {
  print (" Check Data")
}
sum(is.na(TDF))
p <- names(TDF)
str(TDF)
summary(TDF)

# CHANGE COLUMN NAMES
  colnames(TDF)[2] = "LIMIT"
  colnames(TDF)[3] = "GENDER"
  colnames(TDF)[7] = "PMT.STATUS6"
  colnames(TDF)[8] = "PMT.STATUS5"
  colnames(TDF)[9] = "PMT.STATUS4"
  colnames(TDF)[10] = "PMT.STATUS3"
  colnames(TDF)[11] = "PMT.STATUS2"
  colnames(TDF)[12] = "PMT.STATUS1"
  colnames(TDF)[13] = "BILL.AMT6"
  colnames(TDF)[14] = "BILL.AMT5"
  colnames(TDF)[15] = "BILL.AMT4"
  colnames(TDF)[16] = "BILL.AMT3"
  colnames(TDF)[17] = "BILL.AMT2"
  colnames(TDF)[18] = "BILL.AMT1"
  colnames(TDF)[19] = "PMT.AMT6"
  colnames(TDF)[20] = "PMT.AMT5"
  colnames(TDF)[21] = "PMT.AMT4"
  colnames(TDF)[22] = "PMT.AMT3"
  colnames(TDF)[23] = "PMT.AMT2"
  colnames(TDF)[24] = "PMT.AMT1"
  colnames(TDF)[25] = "DEFAULT.IND"

# INITIAL LOOK AT DATA
colnames(TDF)
head(TDF)
# Correct errors in marriage column
sum(TDF[,5]==0)
TDF[TDF[,5]==0),]
# Replace erronous 0s with 3s    (Others = 3)
TDF[TDF[,5]==0,5]=3

# CORRELATION PLOT
library("corrplot", lib.loc = "/Library/Frameworks/
        R.framework/Versions/3.3/Resources/library")
M <- cor(TDF,method="spearman")
corrplot(M, method="circle")

# SCATTER PLOT
   # 1. CREDIT LIMIT AND DEFAULT BY GENDER
g = ggplot(TDF,aes(DEFAULT.IND,LIMIT,fill=GENDER))
g + stat_summary(fun.y=mean, geom="bar",position="dodge") +
  ggtitle("Credit Card Limit and Defaults By Gender")

  # 2. CREDIT LIMIT AND DEFAULT BY EDUCATION
g = ggplot(TDF,aes(EDUCATION,LIMIT,fill=DEFAULT.IND))
g + stat_summary(fun.y=mean, geom="bar",position="dodge") +
  ggtitle("Credit Card Limit and Defaults By Education")


# BAR CHART DISCRETE ONE VARIABLE
# Breakdown by GENDER
g = ggplot(TDF,aes(GENDER))
g + geom_bar(aes(fill=GENDER)) + 
  ggtitle("Breakdown By GENDER (1=Male 2=Female)")
# Breakdown by Education
g = ggplot(TDF,aes(EDUCATION))
g + geom_bar(aes(fill=EDUCATION)) + 
  ggtitle("Breakdown By Education (1=Grad 2=Univ 3=HS)")
  # By Marital Status
g = ggplot(TDF,aes(MARRIAGE))
g + geom_bar(aes(fill=MARRIAGE)) + 
  ggtitle("Brkdown By Marital Status (1=Married  2=Single)")
  # By Age
g = ggplot(TDF,aes(AGE))
g + geom_bar(aes(fill=AGE)) + 
  ggtitle("Breakdown By Age")
  # Defaults and Education Level - Normal Count
g = ggplot(TDF,aes(GENDER))
g + geom_bar(aes(fill=DEFAULT.IND)) + 
  ggtitle("Defaults and Gender - Normal Count")
# Defaults and Education Level - Proportional Count
g = ggplot(TDF,aes(GENDER))
g + geom_bar(aes(fill=DEFAULT.IND),position="fill") + 
  ggtitle("Defaults and Gender - Proportional")
# Defaults and Education Level - Proportional Count
g = ggplot(TDF,aes(EDUCATION))
g + geom_bar(aes(fill=DEFAULT.IND)) + 
  ggtitle("Defaults and Education Level - Normal Count")
  # Defaults and Education Level - Proportional Count
g = ggplot(TDF,aes(EDUCATION))
g + geom_bar(aes(fill=DEFAULT.IND),position="fill") + 
  ggtitle("Defaults and Education Level - Proportional")
# Defaults and Marriage Level - Normal Count
g = ggplot(TDF,aes(MARRIAGE))
g + geom_bar(aes(fill=DEFAULT.IND)) + 
  ggtitle("Defaults and Marital Status - Normal Count")
# Defaults and Marriage Level - Proportional Count
g = ggplot(TDF,aes(MARRIAGE))
g + geom_bar(aes(fill=DEFAULT.IND),position="fill") + 
  ggtitle("Defaults and Marital Status - Proportional")
# Defaults and Age Level - Normal Count
g = ggplot(TDF,aes(AGE))
g + geom_bar(aes(fill=DEFAULT.IND)) + 
  ggtitle("Defaults and Age - Normal Count")
# Defaults and Age Level - Proportional Count
g = ggplot(TDF,aes(AGE))
g + geom_bar(aes(fill=DEFAULT.IND),position="fill") + 
  ggtitle("Defaults and Age - Proportional Count")
# Defaults and Card Limit - Normal Count
#g = ggplot(TDF,aes(LIMIT))
#g + geom_bar(aes(fill=DEFAULT.IND),position="fill") + 
#  stat_bin(binwidth = 25000, origin = 'boundary') +
#  ggtitle("Defaults and Card Limit - Normal Count")
# Defaults and Card Limit - Proportional Count
#g = ggplot(TDF,aes(LIMIT))
#g + geom_bar(aes(fill=DEFAULT.IND),position="fill") + 
#  ggtitle("Defaults and Card Limit - Proportional Count")

# HISTOGRAM
#g = ggplot(TDF,aes(PMT.AMT1))
#g + geom_histogram(aes(binwidth = 1))

# GEOM_COUNT WITH TWO DISCRETE VARIABLES(
# 1
# g = ggplot(TDF,aes(GENDER,DEFAULT.IND))
# g + geom_count()
# 2
# g = ggplot(TDF,aes(EDUCATION,DEFAULT.IND))
# g + geom_count()




# GEOM_POINT FOR SERIES DATA
#SERIES = "6 MONTH SEQUENCE"
#ggplot(df, aes(SERIES, y = value, color = variable)) + 
#  geom_point(aes(y = y1, col = "y1")) + 
#  geom_point(aes(y = y2, col = "y2"))


# Scatter plot
#install.packages("car")
# scatterplotMatrix(TDF[, c('GENDER','LIMIT')]#,
                  #diagonal = 'histogram',
                  # ellipse = TRUE
                  )

# BOXPLOT
# library(plotly)

#df <- TDF[sample(1:nrow(TDF), size = 1000),]
#ggplot(TDF, aes(GENDER, LIMIT, fill = LIMIT)) + 
#  geom_boxplot(outlier.shape = NA) #+ 


 # ggtitle("Ignore outliers in ggplot2")

# Need to modify the plotly object and make outlier points have opacity equal to 0
# p <- plotly_build(p)
# p$data <- lapply(p$data, FUN = function(x){
#  x$marker = list(opacity = 0)
#  return(x)
#})
#p

