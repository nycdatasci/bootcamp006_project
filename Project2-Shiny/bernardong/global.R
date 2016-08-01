# ------------------------------------------------------------------------------------------
# Created by: Bernard Ong
# Created for: Shiny Project
# Project: Shiny Dashboard for Showcase (NTYC DSA 2nd Project Requirement)
# Purpose: Dashboard code (global defaults settings and functions)
# ------------------------------------------------------------------------------------------

library(xts)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Global error management
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# enable and use this only during development to get detailed error messages
#if (!interactive()) {
#  options(shiny.error = function() {
#  stop(" Development mode debugging use.")
#  })
#}

# use this only during deployment
options(shiny.error = function() {
  stop(" Don't worry, Bubo is not mad.
  Data might just not be available based on the criteria set.
  Check to ensure the criteria is valid.
  Check that a valid ticker symbol is being used.")
})

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the Global variable
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load the localuized weather info from CSV file
wdf <- read.csv("./NYCW2.csv")
wdf1 <- xts(wdf[,-1],order.by=as.Date(wdf$EST))
colnames(wdf1) = c("MaxTemp","MeanTemp","MinTemp","MaxDew","MeanDew","MinDew","MaxHum",
                   "MeanHum","MinHum","MaxSeaP","MeanSeaP","MinSeaP","MaxVis","MeanVis","MinVis",
                   "MaxWind","MeanWind","MaxGust","Rain","Cloud","WindDeg")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Global functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getStockXts <- function (tparm,tsers) {
  # pass input string stock param, and xts series return the time series
  if (tparm=="Adjusted") {
    bbb <- cbind(tsers$Adjusted) }
  else
    bbb <- cbind(tsers$Volume)
  return (bbb)
}

getWeatherXts <- function (tparm,tsers) {
  # pass input string weather param, and xts series return the time series
  if (tparm=="MaxTemp") {
    bbb <- cbind(tsers$MaxTemp) }
  else if (tparm=="MaxHum") {
    bbb <- cbind(tsers$MaxHum) }
  else if (tparm=="MaxWind") {
    bbb <- cbind(tsers$MaxWind) }
  else if (tparm=="MaxVis") {
    bbb <- cbind(tsers$MaxVis) }
  else if (tparm=="Cloud") {
    bbb <- cbind(tsers$Cloud) }
  else
    bbb <- cbind(tsers$Rain)
  return (bbb)
}

getCorrScore <- function (x) {
  y <- ""
  x <- round(x,digits=3)
  if (x>=0.70) {
    y <- "Very Strong Positive" }
  else if (x<0.70 & x>=0.40) {
    y <- "Strong Positive" }
  else if (x<0.40 & x>=0.30) {
    y <- "Moderate Positive" }
  else if (x<0.30 & x>=0.20) {
    y <- "Weak Positive"}
  else if (x<0.20 & x>=0.01) {
    y <- "Negligible" }
  else if (x<0.01 & x>=(-0.01)) {
    y <- "None" }
  else if (x<(-0.01) & x>=(-0.19)) {
    y <- "Negligible" }
  else if (x<(-0.19) & x>=(-0.29)) {
    y <- "Weak Negative" }
  else if (x<(-0.29) & x>=(-0.39)) {
    y <- "Moderate Negative" }
  else if (x<(-0.39) & x>=(-0.69)) {
    y <- "Strong Negative" }
  else if (x<(-0.69)) {
    y <- "Very Strong Negative" }
  return (paste0(x*100,"% = ",y))
}
