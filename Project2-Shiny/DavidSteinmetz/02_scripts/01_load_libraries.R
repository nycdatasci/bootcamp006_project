# NYCDSA Shiny project
# David Richard Steinmetz
# davidsteinmetz@gmail.com
# Last updated on: 2016-07-29

# Install and load libraries
list.of.packages <- c('foreign','data.table','ggplot2','R.utils','xlsx')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
list.of.packages
invisible(lapply(list.of.packages, require, character.only = TRUE))
rm(list.of.packages,new.packages)