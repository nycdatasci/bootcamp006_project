# NYC Data Science Academy
# Project 1: Exploratory and R Visualization Project 
# Author: Chia-An Chen
# 07/17/16

# Data Source: http://www.fda.gov/Drugs/InformationOnDrugs/ucm079750.htm
# File Url: http://www.fda.gov/downloads/Drugs/InformationOnDrugs/UCM054599.zip
# Date Downloaded: 07/13/16 (a new update may be released around 07/22/16)

# Load libraries
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(RColorBrewer)
library(scales)

# Mannually convert .txt into .csv if needed for easier reading, and read data into R
# Modify the path accordindly if you download the data to your computer
application = read.csv("/Users/annecool37/Dropbox/DataScience/Project1/drugsatfda/application.csv", header = TRUE, sep = ",")
reg_date = read.table("/Users/annecool37/Dropbox/DataScience/Project1/drugsatfda/RegActionDate.txt", header = TRUE, sep = "\t")
product = read.csv("/Users/annecool37/Dropbox/DataScience/Project1/drugsatfda/Product.csv", header = TRUE, sep = ",")

# get ApplNo and Date for approval doc
id_date = filter(reg_date, DocType == "N") %>% transmute(ApplNo = ApplNo, date=as.integer(substr(as.character(ActionDate),1,4)))
application_partial = select(application, -ApplType, -MostRecentLabelAvailableFlag, -CurrentPatentFlag, -ActionType)
product_partial = distinct(select(product, -ProductNo, -Form, -Dosage, -ProductMktStatus ,-TECode, -ReferenceDrug, -drugname))
master_file = inner_join (inner_join(id_date, application_partial) , product_partial)

#####################
# Active Ingredient #
#####################
# dataset for studying active ingredient
# add a column that group by every 10 year
active_ingr = inner_join(inner_join(id_date, select(application, ApplNo, SponsorApplicant)),distinct(select(product, ApplNo, activeingred))) %>%
  mutate(activeingred = as.character(activeingred), year_10 = floor(date/10)*10) %>% rename(year = date)

# trend of total approvals
approval_case_ai = summarise(group_by(active_ingr, year), approved_cases = n())
total_approval = ggplot(approval_case_ai, aes(x=year, y=approved_cases)) + geom_point(size = 5, alpha = .5) + geom_smooth(color ="black") +
  labs (y="number of approved cases", title = "Number of Approved New Drug Applications vs. Year") + 
  theme_bw() 
total_approval

# get a general idea of which ingredient is often used w/o grouping by year
# get string of all active ingredients
df_active = data.frame(activeingred = unlist(strsplit(active_ingr$activeingred, split="; ")))
# find the active ingredient with highest frequency
sort_active = arrange(summarise(group_by(df_active, activeingred), count=n()), desc(count))
# list top 10 active ingredients and their application
# n is the number of active ingredient w/ top freqencies
n = 10
top_10_actives = sort_active$activeingred[1:n]
application_vec = data.frame( 
    "Application" = c("Pain Reliever", "Treat High Blood Pressure", "Oral Contraceptive", "Water and Electrolytes", 
                      "Nonsteroidal Anti-inflammatory Drug (NSAID)", "Water and Electrolytes", "Pain Reliever", 
                      "Increase Blood Sugar", "Treat Type 2 Diabetes", "Pain Reliever", "Other"))

# add columns: count, percentage, and label w/ applications
actives_count = transmute(sort_active, activeingred = as.character(activeingred), count = count)[1:n,]
remain_count = nrow(sort_active) - summarise(actives_count, sum(count))[[1]]
sort_active_for_plot = rbind(actives_count, c("OTHER", remain_count)) %>%
  transmute(activeingred = activeingred, count = as.integer(count)) %>% 
  mutate(percentage = round(count*100/sum(count))) %>% cbind(application_vec) 
sort_active_for_bar = arrange(sort_active_for_plot, desc(count)) %>%
  mutate(label = paste(as.character(sort_active_for_plot$activeingred), 
                       as.character(sort_active_for_plot$Application), sep = ": "))
# create bar plot
sorted_actives_bar = ggplot(sort_active_for_bar, aes(x = reorder(label, count), y = count, fill = label)) + 
  geom_bar(width = 1, stat ="identity", alpha =.8,  colour="grey", show.legend = FALSE) + coord_flip() +
  theme_bw() + #theme(axis.text.y = element_text(size = 15), plot.title = element_text(size = 20)) + 
  labs(title = "Top 10 Active Ingredients and Their Applications", x ="", y ="Number of Approved Cases") +
  scale_fill_brewer(palette = "BrBG", guide = FALSE) 

# generate df for the top n active ingredients
master_ai = arrange(distinct(select(active_ingr, year)), year) %>% mutate(year = as.factor(year))
for (i in 1:n) {
  df = data.frame(table(active_ingr[grep(sort_active$activeingred[i], active_ingr$activeingr), "year"])) %>% rename("year"=Var1)
  master_ai = left_join(master_ai, df, by = "year")
}
master_ai[is.na(master_ai)] = 0
names(master_ai) = c("year",as.character(sort_active$activeingred)[1:n])

# transformation for plotting purpose
ai_to_plot = gather(master_ai, "year", "drug", 2:(n+1)) 
names(ai_to_plot) = c("year","active_ingr", "count")
ai_to_plot = transmute(ai_to_plot, year = as.integer(year), active_ingr=active_ingr, count=count)

# normalize data
ai_to_plot = inner_join(ai_to_plot, approval_case_ai) %>% mutate(norm_count = count/approved_cases)
ai_label = select(sort_active_for_plot, activeingred, Application)[1:10,] %>% arrange(activeingred) %>% 
  mutate( "label" = paste(as.character(activeingred), as.character(Application), sep = ": ")) 

# create general plotting function for active ingredient
plot_active_fun = function(data, y_value, ...) {
  ggplot(data, aes(x=strptime(year, format = "%Y"), y = y_value, colour = active_ingr, group = active_ingr)) + 
    geom_smooth(se = FALSE) + theme_bw()
}

# plot: count of different active ingredients vs. year
# total counts for each active ingredient
g_ai = plot_active_fun(ai_to_plot, ai_to_plot$count) + labs(y = "Number of Approved Cases", x = "Year") +
  scale_colour_brewer(palette = "BrBG") 

# normalized counts for each active ingredient
norm_g_ai = plot_active_fun(ai_to_plot, ai_to_plot$norm_count) + labs(y = "Percentage of Approved Cases", x = "Year") +
  scale_colour_brewer(palette = "BrBG", name="ACTIVE INGREDIENT: Application", labels = ai_label$label) + scale_y_continuous(labels=percent)

# combine two plots
p_frame = plot_grid(g_ai + theme(legend.position="none"), norm_g_ai + theme(legend.position="none"),
                    align = 'vh', hjust = -1, nrow = 1, labels = c("Standard", "Normalized"))
grobs = ggplotGrob(norm_g_ai)$grobs
legend = grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
title = ggdraw() + draw_label( "Approved Cases for Top 10 Active Ingredients vs. Year", fontface = "bold", size = 20)
p_with_legend = plot_grid(p_frame, legend, rel_widths = c(2,1))
final_ai_plot = plot_grid(title, p_with_legend, ncol=1, rel_heights=c(0.1, 1))

# subset pain reliever after year 1960 for plotting
pain_names = c("ACETAMINOPHEN", "HYDROCODONE BITARTRATE", "IBUPROFEN", "OXYCODONE HYDROCHLORIDE")
pain_label = filter(ai_label, activeingred %in% pain_names) %>% arrange(activeingred)
# filter data after 1960
pain_to_plot = filter(ai_to_plot, active_ingr %in% pain_names, year >=1960) %>%
  arrange(active_ingr)

# functino to plot bar plot
bar_active_fun = function(data, y_value, grouping, label_vec){
  ggplot(data, aes(x=strptime(year, format = "%Y"), y=y_value, fill = grouping, group = grouping)) + 
    geom_bar(stat="identity") + geom_smooth(color = "black", size = .5) + theme_bw() +
    scale_fill_brewer(palette = "BrBG", name="Active Ingredient", labels = label_vec)
}

# pain reliver bar plot, normalized w/ total approval cases
pain_bar_norm = bar_active_fun(pain_to_plot, pain_to_plot$norm_count, pain_to_plot$active_ingr, pain_label) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  labs(title = "Pain Reliever - Percentage of Approved Cases vs. Year", 
       y = "Percentage of Approved Cases", x = "Year") +
  facet_grid(.~active_ingr)

############################
### all plots for slides ###
############################
## Number of Approved New Drug Applications Has Increase Over Past Decades 
total_approval
## Top 10 Active Ingredients in Approved Cases
sorted_actives_bar
## How Top 10 Active Ingredients Changes Over Years
final_ai_plot
# pain reliver bar plot, normalized w/ total approval cases
pain_bar_norm

########
# Form #
########
id_date_all = transmute(reg_date, ApplNo = ApplNo, date=as.integer(substr(as.character(ActionDate),1,4)))
form_df = distinct(select(product, ApplNo, Form)) %>% inner_join(id_date) %>% mutate( Form = as.character(Form))
# form_df = (select(product, ApplNo, Form)) %>% full_join(id_date)
# length(which(is.na(form_df)==TRUE)): 7050 missing year in 35087 total variables --> 20% record is missing

# create tidy form data frame, and separate type and form in form_df$Form
length = nrow(form_df)
type = character()
form = character()
for (i in 1:length) {
  type[i] = unlist(strsplit(form_df$Form[i], split=";"))[1]
  form[i] = unlist(strsplit(form_df$Form[i], split=";"))[2]
}
type = data.frame(type)
form = data.frame(form)
form_df = cbind(form_df, c(type, form)) %>% select(-ApplNo, -Form) %>% rename(year = date)

# group, rank, and sort form
master_form = group_by(form_df, year, form) %>% summarise(count = n())
sort_form = arrange(summarise(group_by(form_df, form), count=n()), desc(count))

# manually pick top 10 forms via as.character(sort_form$form[1:10])
form_list = c("ORAL", "INJECTION", "TOPICAL", "OPHTHALMIC", "INHALATION", "INTRAVENOUS", "IV (INFUSION)", "NASAL", "SUBCUTANEOUS", "TRANSDERMAL", "VAGINAL")
form_to_plot = data.frame(year = numeric(), form = factor(), count = numeric())
n = 10
for (i in 1:n) { # zoom in the graph by setting 3:10 in the for loop if needed
  sub = filter(master_form, form == form_list[i])
  form_to_plot = full_join(form_to_plot, sub)
}
# normalize data with number of total cases each year
approval_case_sum_f = summarise(group_by(form_df, year), form_cases = n())
form_to_plot = inner_join(form_to_plot, approval_case_sum_f) %>% mutate(norm_count = count/form_cases)
# plot: count of different forms vs. year
# plot w/ raw data
g_f = ggplot(form_to_plot, aes(x=strptime(year, format = "%Y"), y=count, colour = form, group = form)) + geom_smooth(se = FALSE) +
  labs(y = "Number of Approved Cases", x = "Year") + scale_colour_brewer(palette = "BrBG")
# plot w/ normaized data
norm_g_f = ggplot(form_to_plot, aes(x=strptime(year, format = "%Y"), y=norm_count, colour = form, group = form)) + 
  geom_smooth(se = FALSE) + labs(y = "Number of Approved Cases", x = "Year") + 
  scale_colour_brewer(palette = "BrBG", name="Forms of Drug") + scale_y_continuous(labels = percent)
# combine two plots
p_frame = plot_grid(g_f + theme(legend.position="none"), norm_g_f + theme(legend.position="none"),
                    align = 'vh', hjust = -1, nrow = 1, labels = c("Standard", "Normalized"))
grobs = ggplotGrob(norm_g_f)$grobs
legend = grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
title = ggdraw() + draw_label( "Approved Cases for Top 10 Forms vs. Year", fontface = "bold", size = 20)
p_with_legend = plot_grid(p_frame, legend, rel_widths = c(2,1))
final_form_plot = plot_grid(title, p_with_legend, ncol=1, rel_heights=c(0.1, 1))
final_form_plot

#############
# Potential #
#############
# P	Priority review drug: A drug that appears to represent an advance over available therapy
# S	Standard review drug: A drug that appears to have therapeutic qualities similar to those of an already marketed drug
review = select(application_partial, ApplNo, Ther_Potential) %>% filter(Ther_Potential != "")  %>% inner_join(id_date) %>% transmute (year = date, Ther_Potential= as.character(Ther_Potential))
length = nrow(review)
potential = character()
for (i in 1:length) {
  potential[i] = unlist(strsplit(review$Ther_Potential[i], "*"))[1]
}
review = cbind(review, potential) %>% select (year, potential)
review_case_sum = summarise(group_by(review, year), review_cases = n())

# Priority & Standard drugs
review_to_plot = data.frame(table(review)) %>% filter(potential != "") %>% rename (count = Freq) %>% 
  mutate(year = as.integer(as.character(year))) %>% inner_join(review_case_sum) %>% mutate(norm_count = count/review_cases)
# plot w/ raw data
g_ps = ggplot(review_to_plot, aes(x=strptime(year, format = "%Y"), y=count, colour = potential, group = potential)) + 
  geom_smooth(se = FALSE) + labs(y = "Number of Approved Cases", x = "Year") +
  scale_colour_brewer(palette = "Set1")
# plot w/ normalized data
norm_g_ps = ggplot(review_to_plot, aes(x=strptime(year, format = "%Y"), y=norm_count, colour = potential, group = potential)) + 
  geom_smooth(se = FALSE) +
  labs(y = "Percent of Approved Cases", x = "Year") + 
  scale_colour_brewer(palette = "Set1", name="Potentials of Drug", labels = c("Priority", "Standard")) + 
  scale_y_continuous(labels = percent)
# combine two plots
p_frame = plot_grid(g_ps + theme(legend.position="none"), norm_g_ps + theme(legend.position="none"),
                    align = 'vh', hjust = -1, nrow = 1, labels = c("Standard", "Normalized"))
grobs = ggplotGrob(norm_g_ps)$grobs
legend = grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
title = ggdraw() + draw_label( "Two Potentials vs. Year", fontface = "bold", size = 20)
p_with_legend = plot_grid(p_frame, legend, rel_widths = c(2,1))
final_potential_plot = plot_grid(title, p_with_legend, ncol=1, rel_heights=c(0.1, 1))
final_potential_plot

######################
# Miscellaneous Note #
######################
## Important historical facts for FDA
# http://www.fda.gov/AboutFDA/WhatWeDo/History/ProductRegulation/PromotingSafeandEffectiveDrugsfor100Years/
# The Food, Drug, and Cosmetic Act of 1938
# The Kefauver-Harris Drug Amendments 1962
# Orphan Drug Act (ODA) was passed in 1983
# Generic Drugs was established in 1989

## Top 10 active ingredient and their applications
# ACETAMINOPHEN: pain reliever and fever redurcer; Prescription sometimes needed
# HYDROCHLOROTHIAZIDE: high blood pressure and fluid retention(edema); Prescription needed; Thiazide
# ETHINYL ESTRADIOL: oral contraceptive; endogenous estrogen
# SODIUM CHLORIDE: Intravenous injection; a source of water and electrolytes; USP priming solution in hemodialysis procedure; USP is a sterile, nonpyrogenic solution for fluid and electrolyte replenishment in single dose containers for intravenous administration 
# IBUPROFEN: nonsteroidal anti-inflammatory drug (NSAID); treating pain, fever, and inflammation
# POTASSIUM CHLORIDE: Intravenous injection ; USP + mineral potassium used to prevent or to treat low blood levels of potassium (hypokalemia)
# HYDROCODONE BITARTRATE: treat pain; Prescription needed
# DEXTROSE: Intravenous injection; the dextrorotatory form of glucose; increase a person’s blood sugar
# METFORMIN HYDROCHLORIDE: type 2 diabetes
# OXYCODONE HYDROCHLORIDE: pain relievers that act on the central nervous system. Like all narcotics, they may become habit-forming if used over long periods.