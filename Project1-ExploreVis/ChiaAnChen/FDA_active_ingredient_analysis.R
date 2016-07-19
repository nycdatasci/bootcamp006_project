# NYC data science academy
# Project 1: Exploratory Visualization Project 
# Chia-An Chen
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
library(googleVis)

# Mannually convert .txt into .csv if needed for easier reading, and read data into R
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

# trend of total approval
approval_case_ai = summarise(group_by(active_ingr, year), approved_cases = n())
total_approval = ggplot(approval_case_ai, aes(x=year, y=approved_cases)) + geom_point(size = 5, alpha = .5) + geom_smooth(color ="black") +
  labs (y="number of approved cases", title = "Number of Approved New Drug Applications vs. Year") + 
  theme_bw() + scale_fill_manual(color = "blue")
total_approval

# get a general idea of which ingredient is often used w/o grouping by year
# get string of all active ingredients
df_active = data.frame(activeingred = unlist(strsplit(active_ingr$activeingred, split="; ")))
# find the active ingredient with highest frequency
sort_active = arrange(summarise(group_by(df_active, activeingred), count=n()), desc(count))
# list top 1o active ingredients and their application
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
  theme_bw() + theme(axis.text.y = element_text(size = 15), plot.title = element_text(size = 20)) + 
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
sorted_actives_pie
## How Top 10 Active Ingredients Changes Over Years
final_ai_plot
# pain reliver bar plot, raw count w/o normalization
pain_bar
# pain reliver bar plot, normalized w/ total approval cases
pain_bar_norm

#############################################################################
#############################################################################
###################### Data or plots not used ###############################
#############################################################################
#############################################################################
total_approval_bar = ggplot(approval_case_ai, aes(x = year, y = approved_cases)) + geom_bar(stat="identity") +
  labs (y="number of approved cases", title = "Number of Approved New Drug Applications vs. Year") + theme_bw() + 
  geom_smooth(color ="black") + scale_fill_brewer(palette = "BrBG")

# Other types of plots for active ingredient analysis are tried
# pie chart for top ingredients
pie_label = paste(as.character(sort_active_for_plot$activeingred), as.character(sort_active_for_plot$Application), sep = ": ")
sorted_actives_pie = ggplot(sort_active_for_plot, aes(x="", y=count, fill = activeingred)) + 
  geom_bar(width = 1, stat ="identity", alpha =.8) + coord_polar("y", start = 0) + 
  theme_void() + theme(axis.text.x = element_blank(), plot.title = element_text(size = 20, face = "bold", hjust =0.5), 
                       legend.title = element_text(size = 20, face = "bold"),
                       legend.text = element_text(size = 20)) +
  labs (title = "Top 10 Active Ingredients and Their Applications") +
  scale_fill_brewer(palette = "BrBG", name="ACTIVE INGREDIENT: Application", 
                    breaks = sort_active_for_plot$activeingred, labels = pie_label)

# googleVis interactive pie chart
pie = gvisPieChart(sort_active_for_plot)
plot(pie)

# line plot
pain_line = ggplot(pain_to_plot, aes(x = year, y = norm_count, fill = active_ingr, group = active_ingr)) + 
  labs(y = "Percentage of Approved Cases", x = "Year", title = "Pain Reliever - Percentage of Approved Cases vs. Year") +
  geom_point(alpha = .5) + geom_smooth(color = "grey") + facet_grid(.~active_ingr) +
  scale_y_continuous(labels = percent, limits = c(0, NA)) +
  scale_fill_brewer(palette = "BrBG", name="Active Ingredient", labels = pain_label)
pain_line

# normalized pain_to_plot count w/ approval cases of just four active ingredients 
pain_to_plot2 = summarise(group_by(pain_to_plot, year), "case_count" = sum(count)) %>% 
  inner_join(pain_to_plot) %>% select(year, active_ingr, count, case_count) %>%
  mutate(norm_count = count/case_count)
pain_bar2 = ggplot(pain_to_plot2, aes(x=strptime(year, format = "%Y"), y=norm_count, fill = active_ingr, group = active_ingr)) + 
  labs(y = "Percentage of Approved Cases", x = "Year") +
  geom_bar(stat="identity") + scale_y_continuous(labels = percent, limits = c(0, NA)) +
  geom_smooth(color = "black") + theme_bw() + facet_grid(.~active_ingr) +
  scale_fill_brewer(palette = "BrBG", name="Active Ingredient", labels = pain_label)

# line plot
norm_pain_plot = plot_active_fun(pain_to_plot, pain_to_plot$norm_count) + labs(y = "Percentage of Approved Cases", x = "Year", title = "Pain Reliever - Percentage of Approved Cases vs. Year") +
  scale_colour_brewer(palette = "BrBG", name="Active Ingredient", labels = pain_label) + scale_y_continuous(labels=percent/100)

# group by every 10 years
master_ai_10 = arrange(distinct(select(active_ingr, year_10)), year_10) %>% mutate(year_10 = as.factor(year_10))
for (i in 1:n) {
  df = data.frame(table(active_ingr[grep(sort_active$activeingred[i], active_ingr$activeingr), "year_10"])) %>% rename("year_10" = Var1)
  master_ai_10 = left_join(master_ai_10, df, by = "year_10")
}
master_ai_10[is.na(master_ai_10)] = 0
names(master_ai_10) = c("year_10",as.character(sort_active$activeingred)[1:n])
ai_to_plot_10 = gather(master_ai_10, "year_10", "drug", 2:(n+1)) 
names(ai_to_plot_10) = c("year_10","active_ingr", "count")
ai_to_plot_10 = transmute(ai_to_plot_10, year_10 = as.integer(year_10), active_ingr=active_ingr, count=count)

# plot all top 10 active ingredients, count vs. every 10 year
norm_g_ai_bar = ggplot(ai_to_plot_10, aes(x=strptime(year_10, format = "%Y"), y=count, fill = active_ingr, group = active_ingr)) + 
  geom_bar(stat="identity") + facet_grid(.~active_ingr) + geom_smooth(se=FALSE, color ="black")
norm_g_ai_bar

# pie chart with only drugs w/ ACETAMINOPHEN as active ingredient 
ai_to_plot_ace = filter (ai_to_plot_10, active_ingr == "ACETAMINOPHEN")
norm_ai_ace_polar = ggplot(ai_to_plot_ace, aes(x=year_10, y=count, fill = active_ingr, group = active_ingr)) + 
  geom_bar(stat="identity") + facet_grid(.~active_ingr) + coord_polar()

# pain reliver bar plot, raw count w/o normalization
pain_bar = bar_active_fun(pain_to_plot, pain_to_plot$count, pain_to_plot$active_ingr, pain_label) +
  labs(title = "Pain Reliever - Number of Approved Cases vs. Year", 
       y = "Number of Approved Cases", x = "Year") + 
  facet_grid(.~active_ingr)

# Another category with trend: USP for intravenous injection
# subset USP for plotting
usp_names = c("SODIUM CHLORIDE", "POTASSIUM CHLORIDE","DEXTROSE")
usp_label = filter(ai_label, activeingred %in% usp_names)
usp_to_plot = filter(ai_to_plot, active_ingr %in% usp_names, year >=1960)

usp_bar = bar_active_fun(usp_to_plot, usp_to_plot$norm_count, usp_to_plot$active_ingr, usp_label) +
  labs(title = "USP - Percentage of Approved Cases vs. Year") + facet_grid(.~active_ingr)
g_usp = plot_active_fun(usp_to_plot, usp_to_plot$norm_count) + labs(y = "Percentage of Approved Cases", x = "Year") +
  scale_colour_brewer(palette = "Blues") 

###################
# dosage and form #
###################
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
# group, rank, and sort type
master_type = group_by(form_df, year, type) %>% summarise(count = n())
sort_type = arrange(summarise(group_by(form_df, type), count=n()), desc(count))

# form
# manually pick top 10 forms via as.character(sort_form$form[1:10])
form_list = c("ORAL", "INJECTION", "TOPICAL", "OPHTHALMIC", "INHALATION", "INTRAVENOUS", "IV (INFUSION)", "NASAL", "SUBCUTANEOUS", "TRANSDERMAL", "VAGINAL")
form_to_plot = data.frame(year = numeric(), form = factor(), count = numeric())
n = 10
for (i in 1:n) { # zoom in the graph by setting 3:10 in the for loop if needed
  sub = filter(master_form, form == form_list[i])
  form_to_plot = full_join(form_to_plot, sub)
}
# normalize data with total case each year
approval_case_sum_f = summarise(group_by(form_df, year), form_cases = n())
form_to_plot = inner_join(form_to_plot, approval_case_sum_f) %>% mutate(norm_count = count/form_cases)
# plot: year vs. count of different forms
g_f = ggplot(form_to_plot, aes(x=strptime(year, format = "%Y"), y=count, colour = form, group = form)) + geom_smooth(se = FALSE)
norm_g_f = ggplot(form_to_plot, aes(x=strptime(year, format = "%Y"), y=norm_count, colour = form, group = form)) + geom_smooth(se = FALSE)
plot_form = plot_grid(g_f, norm_g_f, labels=c("standard form", "normalized form"), ncol = 2, nrow = 1)

#######################################################################
# Trend found:                                                        #
# Oral dominate the form of drug --> then injection --> then others   #
# noramalized data suggests injection is in the trend of declining    #
#######################################################################

# type
# manually pick top 10 types via as.character(sort_type$type[1:15])
type_list = c("TABLET", "INJECTABLE", "CAPSULE", "SOLUTION", "CREAM","SYRUP", "OINTMENT", "SUSPENSION", "POWDER", "GEL")
type_to_plot = data.frame(year = numeric(), type = factor(), count = numeric())
n = 10
for (i in 1:n) {
  sub = filter(master_type, type == type_list[i])
  type_to_plot = full_join(type_to_plot, sub)
}

# normalize data with total case each year
type_to_plot = inner_join(type_to_plot, approval_case_sum_f) %>% mutate(norm_count = count/form_cases)
g_t = ggplot(type_to_plot, aes(x=strptime(year, format = "%Y"), y=count, colour = type, group = type)) + geom_smooth(se = FALSE)
norm_g_t = ggplot(type_to_plot, aes(x=strptime(year, format = "%Y"), y=norm_count, colour = type, group = type)) + geom_smooth(se = FALSE)
plot_type = plot_grid(g_t, norm_g_t, labels=c("standard type", "normalized type"), ncol = 2, nrow = 1)

################
# review class #
################
# P	Priority review drug: A drug that appears to represent an advance over available therapy
# S	Standard review drug: A drug that appears to have therapeutic qualities similar to those of an already marketed drug
# O	Orphan drug - a product that treats a rare disease affecting fewer than 200,000 Americans
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
g_ps = ggplot(review_to_plot, aes(x=strptime(year, format = "%Y"), y=count, colour = potential, group = potential)) + geom_smooth(se = FALSE)
norm_g_ps = ggplot(review_to_plot, aes(x=strptime(year, format = "%Y"), y=norm_count, colour = potential, group = potential)) + geom_smooth(se = FALSE)
plot_ps = plot_grid(g_ps, norm_g_ps, labels=c("standard review", "normalized review"), ncol = 2, nrow = 1)

# Orphan drugs
orphan = select(application_partial, ApplNo, Orphan_Code) %>% filter (Orphan_Code != "") %>% inner_join(id_date) %>% transmute (year = date, orphan= as.character(Orphan_Code))
orphan_to_plot = summarise(group_by(orphan, year), count = n()) %>% inner_join(review_case_sum) %>% mutate(norm_count = count/review_cases, potential = "O")
total_review_to_plot = full_join(review_to_plot, orphan_to_plot)
g_r = ggplot(total_review_to_plot, aes(x=strptime(year, format = "%Y"), y=count, colour = potential, group = potential)) + geom_line() + geom_smooth(se = FALSE)
norm_g_r = ggplot(total_review_to_plot, aes(x=strptime(year, format = "%Y"), y=norm_count, colour = potential, group = potential)) + geom_line()  + geom_smooth(se = FALSE)
plot_r = plot_grid(g_r, norm_g_r, labels=c("standard review", "normalized review"), ncol = 2, nrow = 1)
# nothing surprising....but the retio of priority and standard drugs remain steady for decades 

##########
# Others #
##########
# chem_type = read.csv("/Users/annecool37/Dropbox/DataScience/Project1/drugsatfda/ChemTypeLookup.csv", header = TRUE, sep = ",")
# review_class = read.csv("/Users/annecool37/Dropbox/DataScience/Project1/drugsatfda/ReviewClass_Lookup.csv", header = TRUE, sep = ",")

# sth may be interested to look into
# chemtype 7: drug in market w/o DFA approval
# check indredient and potential
drug_with_no_approval = select(application_partial, ApplNo, Chemical_Type) %>% filter(Chemical_Type ==7) %>%
  inner_join(product_partial) 
sum = summarise(group_by(drug_with_no_approval, activeingred),count = n())
# however, no clear trend was found

# see how many approvals did each company get (sorted result)
# and the first few ones seem to focus on generic drugs
company_approvals = arrange(summarise(group_by(active_ingr, SponsorApplicant), count = n()), desc(count))
# see how many companies got approvals each year
company_count = distinct(select(active_ingr, year, SponsorApplicant)) %>% group_by(year) %>% summarise(company_count = n())

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
