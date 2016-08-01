library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggmap)
library(maptools)

source("linechart.R")

cols = c('PUMA', 'Property Value', 'Household Income', 'Household Type (Married)', 'Sex (Male)', 'Race (White)', 'Education (Higher)')
ACS = read.csv('data/ACS_05-14.csv')
ACS$PUMA = sprintf('%05d', ACS$PUMA)
#contPUMAS = read.csv('data/ACS_d10s.csv')$PUMA
# data = read.csv('data/ACS_d10s.csv') %>% 
#   select(2, VAL.y, HINCP.y, HHT_MDC.y, SEX_M.y, RACE_W.y, EDU_HIED.y)
d10s = read.csv('data/ACS_d10s.csv')

PUMA_cont = sprintf('%05d', d10s$d10comb.PUMA)

Incomes = select(ACS, PUMA, Year, HINCP, AGEP, PINCP, VAL) %>% 
  filter(PUMA %in% PUMA_cont)

ACS_P = read.csv('data/ACS_Percentages.csv')
ACS_P$PUMA = sprintf('%05d', ACS_P$PUMA)

#shapes = readShapePoly('data/tl_2010_36_puma10.shp')
