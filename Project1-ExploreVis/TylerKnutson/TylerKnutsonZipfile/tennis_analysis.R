# Load packages
library(dplyr)
library(ggplot2)
library(latticeExtra)
library(waterfall)
library(RColorBrewer)
library(sqldf)
library(ggthemes)
library(scales)


# Set global parameters for upset type and class
p_st = 1.1
p_hf = 1.4
p_cf = 1.85
p_sf = 2.0
p_sd = 2.6
p_cd = 3.5
p_hd = 5.0


# Import 2014 odds data and check structure
odds_2014 <- read.csv('2014_odds.csv', header = TRUE, stringsAsFactors = FALSE)
#head(odds_2014, 15)
#class(odds_2014)
#str(odds_2014)

# Import 2013 odds data
odds_2013 <- read.csv('2013_odds.csv', header = TRUE, stringsAsFactors = FALSE)
#str(odds_2013)

# For some reason a few fields come in as factors in this file only -- fixing it
odds_2013$B365W <- as.numeric(as.character(odds_2013$B365W))
odds_2013$EXW <- as.numeric(as.character(odds_2013$EXW))

# Import 2012 odds data
odds_2012 <- read.csv('2012_odds.csv', header = TRUE, stringsAsFactors = FALSE)
#str(odds_2012)

# Import 2014 atp match data
atp_2014 <- read.csv('atp_matches_2014.csv', header = TRUE, stringsAsFactors = FALSE)
#head(atp_2014, 2)
#class(atp_2014)
#str(atp_2014)

# Import 2013 atp match data
atp_2013 <- read.csv('atp_matches_2013.csv', header = TRUE, stringsAsFactors = FALSE)
#str(atp_2013)

# Import 2012 atp match data
atp_2012 <- read.csv('atp_matches_2012.csv', header = TRUE, stringsAsFactors = FALSE)
#str(atp_2012)

# Combine atp match data into single data frame
atp_all <- rbind(atp_2012, atp_2013)
atp_all <- rbind(atp_all, atp_2014)
#glimpse(atp_all)

# Combine odds data into single data frame, remove walkovers, retirements, and DQs
# Add average odds and standard deviation odds
# Add classification for dog and favorite
odds_all <- rbind(odds_2012, odds_2013)
odds_all <- rbind(odds_all, odds_2014)
odds_all <- filter(odds_all, Comment == "Completed")

# avg winner odds
a = select(odds_all, B365W,EXW, LBW, PSW, SJW)
a = t(a)
a = as.data.frame(a)
#dim(a)
#glimpse(a)
b = sapply(a, mean, na.rm = TRUE)
#dim(b)
#glimpse(b)
odds_all$avg_odds = b

rm(a)
rm(b)

# avg winner sd of odds
c = select(odds_all, B365W,EXW, LBW, PSW, SJW)
c = t(c)
c = as.data.frame(c)
#dim(c)
#glimpse(c)
d = sapply(c, sd, na.rm = TRUE)
#dim(d)
#glimpse(d)
odds_all$odds_sd = d

rm(c)
rm(d)
#glimpse(odds_all)
#rm(odds_all)

# avg loser odds
e = select(odds_all, B365L,EXL, LBL, PSL, SJL)
e = t(e)
e = as.data.frame(e)
#dim(e)
#glimpse(e)
f = sapply(e, mean, na.rm = TRUE)
#dim(f)
#glimpse(f)
odds_all$avg_odds_l = f

rm(e)
rm(f)

# avg loser sd of odds
g = select(odds_all, B365L,EXL, LBL, PSL, SJL)
g = t(g)
g = as.data.frame(g)
#dim(g)
#glimpse(g)
h = sapply(g, sd, na.rm = TRUE)
#dim(h)
#glimpse(h)
odds_all$odds_sd_l = h

rm(g)
rm(h)

# classify dog and favorite based on avg odds
# winner
odds_all$winner_class = (
ifelse(odds_all$avg_odds
       < p_st, "Sure Thing",ifelse(odds_all$avg_odds
          < p_hf, "Heavy Favorite", ifelse(odds_all$avg_odds
            < p_cf, "Clear Favorite", ifelse(odds_all$avg_odds
              < p_sf, "Slight Favorite", ifelse(odds_all$avg_odds
                < p_sd, "Slight Dog", ifelse(odds_all$avg_odds
                  < p_cd, "Clear Dog", ifelse(odds_all$avg_odds
                    < p_hd, "Heavy Dog", ifelse(odds_all$avg_odds
                      > p_hd, "No Hope", "NA"
       )))))))))


# loser
odds_all$loser_class = (
  ifelse(odds_all$avg_odds_l
      < p_st, "Sure Thing",ifelse(odds_all$avg_odds_l
        < p_hf, "Heavy Favorite", ifelse(odds_all$avg_odds_l
          < p_cf, "Clear Favorite", ifelse(odds_all$avg_odds_l
            < p_sf, "Slight Favorite", ifelse(odds_all$avg_odds_l
              < p_sd, "Slight Dog", ifelse(odds_all$avg_odds_l
                < p_cd, "Clear Dog", ifelse(odds_all$avg_odds_l
                 < p_hd, "Heavy Dog", ifelse(odds_all$avg_odds_l
                   > p_hd, "No Hope", "NA"
        )))))))))



# Import stats overview
stats_overview <- read.csv('charting-m-stats-Overview.csv', header = TRUE, stringsAsFactors = FALSE)
#head(stats_overview, 2)
#str(stats_overview)

# Import shot types
shot_types <- read.csv('charting-m-stats-ShotTypes.csv', header = TRUE, stringsAsFactors = FALSE)
#head(shot_types, 2)
#str(shot_types)

# Import return depth
return_depth <- read.csv('charting-m-stats-ReturnDepth.csv', header = TRUE, , stringsAsFactors = FALSE)
#head(return_depth, 2)
#str(return_depth)

# Import serve_basics
serve_basics <- read.csv('charting-m-stats-ServeBasics.csv', header = TRUE, stringsAsFactors = FALSE)
#head(serve_basics, 2)
#str(serve_basics)

# Find atp_keys for all upsets and put into 'upset_atp_keys' dataframes
# "Upset" defined as any major bookmaker with winner odds greater than even money (2.0 British system)
# Omit any walkovers, DQs, or retiremetns as aberrations 
odds_upsets_all <- filter(odds_all, (B365W > 2.0 | EXW > 2.0 | LBW > 2.0 | PSW > 2.0 | SJW > 2.0))
#glimpse(odds_upsets_all) # So, there were at least 2,084 upsets between 2012 and 2014

# Join atp_all to odds_upsets_all to eliminate atp matches without upset
atp_upsets_all <- semi_join(atp_all, odds_upsets_all, by = "atp_key")
#glimpse(atp_upsets_all)
# Good, same number of observations 2,084

# Create a master data frame with all odds and match data combined (not just upsets)
c_odds_atp_all <- inner_join(atp_all, odds_all, by = "atp_key")
c_odds_atp_all$Date = as.Date(c_odds_atp_all$Date, "%m/%d/%Y")
#head(c_odds_atp_all,3)

# Create a master data frame with upsets only odds and match data combined
c_odds_atp_upsets <- inner_join(atp_upsets_all, odds_upsets_all, by = "atp_key")
c_odds_atp_upsets$Date = as.Date(c_odds_atp_upsets$Date, "%m/%d/%Y")

# Create a master data frame with non-upsets only odds and match data combined
odds_all_non_upsets <- filter(odds_all, (B365W <= 2.0 & EXW <= 2.0 &  LBW <= 2.0 & PSW <= 2.0 & SJW <= 2.0))
c_odds_atp_nonupsets <- inner_join(atp_all, odds_all_non_upsets, by = "atp_key")
c_odds_atp_nonupsets$Date = as.Date(c_odds_atp_nonupsets$Date, "%m/%d/%Y")
#head(c_odds_atp_nonupsets,1)

# Add column to upsets master that groups upset severity ("upset_type")
c_odds_atp_upsets <- mutate(c_odds_atp_upsets, upset_type = 
      ifelse(avg_odds < p_sd, "Common", ifelse(avg_odds < p_cd, "Slightly Unusual", ifelse(avg_odds < p_hd, "Unusual", ifelse( avg_odds < 10000, "Major Upset", "NA")))))

# For consistency, add "upset_type" column to other c_ tables
c_odds_atp_nonupsets$upset_type = "Favorite Won"
c_odds_atp_all$upset_type = "Favorite Won"

# create smaller subset so it's easier to remove duplicate columns
upset_subset = select(c_odds_atp_upsets, atp_key, upset_type)

#head(upset_subset,15)
#head(c_odds_atp_all,1)
#head(left_join(c_odds_atp_all, upset_subset, by = "atp_key"),10)

lft_join = mutate(left_join(c_odds_atp_all, upset_subset, by = "atp_key"), upset_type = ifelse(is.na(upset_type.y), upset_type.x, upset_type.y))
lft_join$upset_type.x = NULL
lft_join$upset_type.y = NULL
rm(upset_subset)


c_odds_atp_all = lft_join
rm(lft_join)


#--------#



# Hypothesis Section

# 1: Do odds vary more pre-match when there's an upset vs all mathches, vs no upset?

# summary(c_odds_atp_upsets$odds_sd)
# summary(c_odds_atp_nonupsets$odds_sd)
# summary(c_odds_atp_all$odds_sd)
#head(c_odds_atp_all,1)


# Need a summary table of average odds by class
# Then can add a comparison directly to c_odds_atp_all table (maybe another view) to see how much variance from standard variance

c_odds_atp_all$upset_flag = ifelse(c_odds_atp_all$upset_type == "Favorite Won", "F", "U")
a = c_odds_atp_all
bind_class_upsetflag = select(a, class = winner_class, upset_flag, avg_odds, odds_sd) 
by_class_upsetflag = group_by(rbind(bind_class_upsetflag,select(a, class = loser_class, upset_flag, avg_odds = avg_odds_l, odds_sd = odds_sd_l)), class, upset_flag)

bind_class = select(a, class = winner_class, avg_odds, odds_sd) 
by_class = group_by(rbind(bind_class,select(a, class = loser_class, avg_odds = avg_odds_l, odds_sd = odds_sd_l)), class)

h1_class_summary = summarize(by_class, avg_odds = mean(avg_odds, na.rm = TRUE), odds_sd = mean(odds_sd, na.rm = TRUE), count = n())
h1_class_summary = h1_class_summary[!is.na(h1_class_summary$class)&h1_class_summary$class!="NA",]
h1_class_summary$class = as.factor(h1_class_summary$class)
h1_class_summary$class = factor(h1_class_summary$class)
h1_class_summary$class = factor(h1_class_summary$class, levels = levels(h1_class_summary$class)[c(5, 3, 1, 6, 7, 2, 4, 8)])
#h1_class_summary$class 

rm(a)

h1_class_uf_summary = summarize(by_class_upsetflag, avg_odds = mean(avg_odds, na.rm = TRUE), odds_sd = mean(odds_sd, na.rm = TRUE), count = n())
h1_class_uf_summary = h1_class_uf_summary[!is.na(h1_class_uf_summary$class)&h1_class_uf_summary$class!="NA",]
h1_class_uf_summary$class = as.factor(h1_class_uf_summary$class)
h1_class_uf_summary$class = factor(h1_class_uf_summary$class)
h1_class_uf_summary$class = factor(h1_class_uf_summary$class, levels = levels(h1_class_uf_summary$class)[c(5, 3, 1, 6, 7, 2, 4, 8)])
#h1_class_uf_summary$class 

rm(a)

p <-
sqldf('
  select
uf.class,
uf.upset_flag,
uf.avg_odds as match_odds,
uf.odds_sd as match_sd,
c.avg_odds as class_odds,
c.odds_sd as class_sd,
uf.avg_odds - c.avg_odds as odds_dif,
uf.odds_sd - c.odds_sd as sd_dif,
(uf.avg_odds - c.avg_odds)/c.avg_odds as odds_pct_dif,
(uf.odds_sd - c.odds_sd)/c.odds_sd as sd_pct_dif,
sum(uf.count) as match_count

  from h1_class_uf_summary uf 
  inner join h1_class_summary c on uf.class = c.class
  group by
uf.class,
uf.upset_flag,
uf.avg_odds,
uf.odds_sd,
c.avg_odds,
c.odds_sd,
uf.avg_odds - c.avg_odds,
uf.odds_sd - c.odds_sd,
(uf.avg_odds - c.avg_odds)/c.avg_odds,
(uf.odds_sd - c.odds_sd)/c.odds_sd
')


p$class = as.factor(p$class)
p$class = factor(p$class, levels = levels(p$class)[c(5, 3, 1, 6, 7, 2, 4, 8)])

# ----- PLOTS -------#

t = by_class_upsetflag #individual records
p #aggregate performance grouped by class + upset flag


# First we show the overall relationship between odds and standard deviation
g1 = ggplot(t, aes(x = avg_odds, y = odds_sd))
g1 + 
  #ylim(0,0.5) +
  xlim(0, 40) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank())+
  scale_colour_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("ATP Matches 2012-2014: Upsets vs Non-Upsets")+
  labs(x = "Average Match Odds", y = "Odds Standard Deviation") +
  geom_point(aes(col = upset_flag), size = 0.90) + 
  geom_smooth(se = FALSE, aes(group = upset_flag, col = upset_flag)) +
  geom_vline(xintercept = p_sf, linetype = "dotted", color = "blue", size = 1.0) 

ggsave("Upsets vs Non.wmf", height = 3.83, width = 10.8)
  


# Then we double click --> 1 for everything besides no hope (faceted), then another for no hope
g2 = ggplot(t[t$class != "No Hope",], aes(x = avg_odds, y = odds_sd))
g2 + 
  ylim(0,0.5) + 
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  scale_x_continuous(labels = c("1.0", "2.0", "3.0", "4.0", "5.0")) +
  theme(legend.title=element_blank())+
  scale_colour_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("ATP Matches 2012-2014: Upsets vs Non-Upsets\n(Excluding Dogs > 5.0 Odds)")+
  labs(x = "Average Match Odds", y = "Odds Standard Deviation") +
  geom_point(aes(col = upset_flag), size = 0.90) + 
  geom_smooth(se = FALSE, aes(group = upset_flag, col = upset_flag)) +
  geom_vline(xintercept = p_st, linetype = "dotted", color = "red", size = 1.0) +
  geom_vline(xintercept = p_hf, linetype = "dotted", color = "red", size = 1.0) +
  geom_vline(xintercept = p_cf, linetype = "dotted", color = "red", size = 1.0) +
  geom_vline(xintercept = p_sf, linetype = "dotted", color = "red", size = 1.0) +
  geom_vline(xintercept = p_sd, linetype = "dotted", color = "red", size = 1.0) +
  geom_vline(xintercept = p_cd, linetype = "dotted", color = "red", size = 1.0) +
  geom_vline(xintercept = p_hd, linetype = "dotted", color = "red", size = 1.0)

ggsave("Exclude No Hope.wmf", height = 3.83, width = 10.8)


# g3 = ggplot(t[t$class == "No Hope",], aes(x = avg_odds, y = odds_sd))
# g3 + 
#   #ylim(0,3) + 
#   xlim(5.0, 60) +
#   theme_hc() +
#   theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
#   #scale_x_continuous(labels = c("1.0", "2.0", "3.0", "4.0", "5.0")) +
#   theme(legend.title=element_blank())+
#   scale_colour_hc(labels = c("Non-Upset", "Upset")) +
#   ggtitle("ATP Matches 2012-2014: Upsets vs Non-Upsets\n(Dogs > 5.0 Odds Only)")+
#   labs(x = "Average Match Odds", y = "Odds Standard Deviation") +
#   geom_point(aes(col = upset_flag), size = 0.90) + 
#   geom_smooth(se = FALSE, aes(group = upset_flag, col = upset_flag))# +
#   # geom_vline(xintercept = p_st, linetype = "dotted", color = "blue", size = 1.0) +
#   # geom_vline(xintercept = p_hf, linetype = "dotted", color = "blue", size = 1.0) +
#   # geom_vline(xintercept = p_cf, linetype = "dotted", color = "blue", size = 1.0) +
#   # geom_vline(xintercept = p_sf, linetype = "dotted", color = "blue", size = 1.0) +
#   # geom_vline(xintercept = p_sd, linetype = "dotted", color = "blue", size = 1.0) +
#   # geom_vline(xintercept = p_cd, linetype = "dotted", color = "blue", size = 1.0) +
#   # geom_vline(xintercept = p_hd, linetype = "dotted", color = "blue", size = 1.0)

g3 + 
  ylim(0,3) + 
  xlim(5.0, 10) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  #scale_x_continuous(labels = c("1.0", "2.0", "3.0", "4.0", "5.0")) +
  theme(legend.title=element_blank())+
  scale_colour_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("ATP Matches 2012-2014: Upsets vs Non-Upsets\n(Dogs > 5.0 Odds Only)")+
  labs(x = "Average Match Odds", y = "Odds Standard Deviation") +
  geom_point(aes(col = upset_flag), size = 0.90) + 
  geom_smooth(se = FALSE, aes(group = upset_flag, col = upset_flag))

ggsave("No Hope Only.wmf", height = 3.83, width = 10.8)



# g = ggplot(p, aes(x = match_odds, y = match_sd))+geom_point()
# g + geom_density(aes(color = class)) +  xlim(-2,2)



# head(t,1)


# show waterfall of all matches in the dataset (upset breakdown, non upsets)
by_utype = group_by(c_odds_atp_all, upset_type)

  
utype_summary = summarize(by_utype, n = n())[c(1,4,5,3,2),]
utype_summary$subtotal = c(rep("Upset",4), "Total")
utype_summary$upset_type = as.factor(utype_summary$upset_type)
utype_summary$subtotal = as.factor(utype_summary$subtotal)
#str(utype_summary)
utype_summary$subtotal = factor(utype_summary$subtotal, levels = levels(utype_summary$subtotal)[2:1])

# update color settings
mysettings = list(superpose.polygon=list(col=mycolors[2:5], border="transparent"),
                  strip.background=list(col=mycolors[6]),
                  strip.border=list(col="black"))
# build waterfall
waterfallchart(
 utype_summary$n~utype_summary$upset_type,
 data = utype_summary,
 groups=utype_summary$subtotal,
 main = "2012-2014 ATP Match Breakdown",
 xlab = "Match Result Type",
 ylab = "Match Count",
 par.settings = mysettings
  )
?lattice

# build faceted area charts --> 4 for upset type, then 3 for all, all non upsets, and all upsets

##unique(c_odds_atp_all$upset_type)
h1_data = c_odds_atp_all %>% group_by()....
g <- ggplot(data = c_odds_atp_all[c_odds_atp_all$upset_type != "Favorite Won",], aes(x = odds_sd))
#g <- ggplot(data = c_odds_atp_all, aes(x = odds_sd))
#g + geom_histogram(binwidth = 0.01) + coord_cartesian(xlim = c(0,1.0))
#g + geom_boxplot()
g + geom_density(aes(color = upset_type))+ coord_cartesian(xlim = c(0, 1.2)) + facet_grid(~upset_type)
# maybe box plots here
# is it possible to graph SD against probability of underdog to win?  How many wins vs losses for UD Y axis, x = SD
#write.csv(c_odds_atp_all, file='h1c_odds_atp_all.csv', row.names = T)

# 2: How much was underdog playing recently?  How much was favorite playing?
# Maybe there's a "sweet spot" where they're not playing too many sets to be fatigued, but enough to be in form

# cast date from string to date type
h2_odds_all <- odds_all
h2_odds_all$Date = as.Date.character(h2_odds_all$Date, "%m/%d/%Y")
#glimpse(h2_odds_all)

# add a column with number of sets played in prior d days
  # first add total sets played in match
h2_odds_all = mutate(h2_odds_all, Tsets = (Wsets + Lsets))

d = 30 
#head(h2_odds_all)


# this doesn't work: mutate(h2_odds_all, newcol = sum(h2_odds_all$Tsets[(h2_odds_all$Date < Date) & (h2_odds_all$Date > (Date - d))]))
h2_odds_all$recent_sets_w <- sapply(seq_len(nrow(h2_odds_all)), function(i) with(h2_odds_all, 
                              sum(Tsets[
                                Date >= (Date[i]-d) & (Date < Date[i]) & (atp_winner_id[i] == atp_winner_id | atp_winner_id[i] == atp_loser_id)
                                ])))
# write.csv(h2_odds_all, file='h2oddscheck.csv', row.names = T)
# check that it works in excel:
# ={SUM(IF((($F:$F<F2)*($F:$F>(F2-60)))*((AT2=$AT:$AT)+(AT2=$AU:$AU)),$AY:$AY,0))}

h2_odds_all$recent_sets_l <- sapply(seq_len(nrow(h2_odds_all)), function(i) with(h2_odds_all, 
                               sum(Tsets[
                                 Date >= (Date[i]-d) & (Date < Date[i]) & (atp_loser_id[i] == atp_winner_id | atp_loser_id[i] == atp_loser_id)
                                 ])))

# before plotting, need to remove first d days of year otherwise there's a 0 recent sets skew

#g <- ggplot(h2_odds_all, aes(x = recent_sets_w))
#g + geom_bar(aes(fill = recent_sets_w))

#g <- ggplot(h2_odds_all, aes(x = recent_sets_l))
#g + geom_bar(aes(fill = recent_sets_l))



# 3: How did underdog's key statistics compare to their individual prior average (last 6 months)?  Same question for favorite?

# figure out where the winner id is on the combined table; take all columns from stats, add winner id only, inner join
c_date_pos <- grep("Date", colnames(c_odds_atp_all))
c_win_pos <- grep("atp_winner_id", colnames(c_odds_atp_all))
c_wclass_pos <- grep("winner_class", colnames(c_odds_atp_all))
c_lclass_pos <- grep("loser_class", colnames(c_odds_atp_all))
c_utype_pos <- grep("upset_type", colnames(c_odds_atp_all))
h3_cols <- ncol(stats_overview)
stats_overview$atp_key = as.integer(stats_overview$atp_key)

h3_all <- select(inner_join(stats_overview, c_odds_atp_all, by = "atp_key"), c(1:h3_cols, h3_cols+c_date_pos-1, h3_cols+c_win_pos-1, h3_cols+c_wclass_pos-1, h3_cols+c_lclass_pos-1, h3_cols+c_utype_pos-1))
#glimpse(h3_all)


h3_nonupset <- select(inner_join(stats_overview, c_odds_atp_nonupsets, by = "atp_key"), c(1:h3_cols, h3_cols+c_date_pos-1, h3_cols+c_win_pos-1, h3_cols+c_wclass_pos-1, h3_cols+c_lclass_pos-1, h3_cols+c_utype_pos-1))
#glimpse(h3_nonupset)

h3_upset <- select(inner_join(stats_overview, c_odds_atp_upsets, by = "atp_key"), c(1:h3_cols, h3_cols+c_date_pos-1, h3_cols+c_win_pos-1, h3_cols+c_wclass_pos-1, h3_cols+c_lclass_pos-1, h3_cols+c_utype_pos-1))
#glimpse(h3_upset)

#glimpse(c_odds_atp_all)
# now figure out whether player 1 or 2 was winner, adjust stats accordingly

h3_upset$p1_id <- as.numeric(h3_upset$p1_id)
h3_upset$p2_id <- as.numeric(h3_upset$p2_id)

h3_all$p1_id <- as.numeric(h3_all$p1_id)
h3_all$p2_id <- as.numeric(h3_all$p2_id)

h3_nonupset$p1_id <- as.numeric(h3_nonupset$p1_id)
h3_nonupset$p2_id <- as.numeric(h3_nonupset$p2_id)

# add a column with a flag for winner vs loser "wflag"

h3_upset$wflag = ifelse(
  h3_upset$player == 1,ifelse(h3_upset$p1_id == h3_upset$atp_winner_id, "W", "L"), # if indicator on row is P1...
  ifelse(h3_upset$p2_id == h3_upset$atp_winner_id, "W", "L")) # otherwise same logic if indicator is P2

h3_all$wflag = ifelse(
  h3_all$player == 1,ifelse(h3_all$p1_id == h3_all$atp_winner_id, "W", "L"),
  ifelse(h3_all$p2_id == h3_all$atp_winner_id, "W", "L"))

h3_nonupset$wflag = ifelse(
  h3_nonupset$player == 1,ifelse(h3_nonupset$p1_id == h3_nonupset$atp_winner_id, "W", "L"),
  ifelse(h3_nonupset$p2_id == h3_nonupset$atp_winner_id, "W", "L"))

# filter accordingly, create stats columns, select relevant columns only, store in new df

tfilter = filter(h3_all, set == "Total")
#glimpse(tfilter)
tmutate = mutate(tfilter, ace_pct = (aces / serve_pts), df_pct = (dfs / serve_pts), firstin_pct = (first_in / serve_pts), firstwon_pct = (first_won / first_in), secondwon_pct = (second_won / second_in), bkfcd_pct = (bk_pts / serve_pts), bksvd_pct = (bp_saved / bk_pts), retwon_pct = (return_pts_won / return_pts), win_uf_ratio = (winners / unforced))
#glimpse(tmutate)

h3_all = tmutate
h3_all$player_id = ifelse(h3_all$player == 1, h3_all$p1_id, h3_all$p2_id)
h3_all$class = ifelse(h3_all$wflag == "W", h3_all$winner_class, h3_all$loser_class)


# dont use p1_id, use player indicator

t_upset = h3_upset[h3_upset$set == "Total",c("atp_key","player")]
t_upset$joincol = paste(t_upset$atp_key, t_upset$player)
t_h3all = h3_all
t_h3all$joincol = paste(t_h3all$atp_key, t_h3all$player)
h3_all$upset_flag = 
  ifelse(is.na(left_join(t_h3all, t_upset, by = "joincol")[,"atp_key.y"]),"F","U")
#glimpse(h3_all)
#glimpse(h3_upset)
# create h3...player, match, date, w/l, other stats

h3_player =
  select(
    h3_all, atp_key, player_id, Date, wflag, class, ace_pct, df_pct, firstin_pct, firstwon_pct, secondwon_pct, bkfcd_pct, bksvd_pct, retwon_pct, win_uf_ratio
  )
#glimpse(h3_player)
#write.csv(h3_all, "h3_check.csv", row.names = TRUE)

#-------
# Old Way
# # upsets mutate
# tfilter = filter(h3_upset, wflag == "W", set == "Total")
# tmutate = mutate(tfilter, ace_pct_w = (aces / serve_pts), df_pct_w = (dfs / serve_pts), firstin_pct_w = (first_in / serve_pts), firstwon_pct_w = (first_won / first_in), secondwon_pct_w = (second_won / second_in), bkfcd_pct_w = (bk_pts / serve_pts), bksvd_pct_w = (bp_saved / bk_pts), retwon_pct_w = (return_pts_won / return_pts), win_uf_ratio_w = (winners / unforced))
# #glimpse(tfilter)
# #glimpse(tmutate)
# #glimpse(h3_upset)
# #glimpse(h3_upsetstats_w)
# h3_upsetstats_w = tmutate
# 
# tfilter = filter(h3_upset, wflag == "L", set == "Total")
# tmutate = mutate(tfilter, ace_pct_l = (aces / serve_pts), df_pct_l = (dfs / serve_pts), firstin_pct_l = (first_in / serve_pts), firstwon_pct_l = (first_won / first_in), secondwon_pct_l = (second_won / second_in), bkfcd_pct_l = (bk_pts / serve_pts), bksvd_pct_l = (bp_saved / bk_pts), retwon_pct_l = (return_pts_won / return_pts), win_uf_ratio_l = (winners / unforced))
# h3_upsetstats_l = tmutate
# 
# # all
# tfilter = filter(h3_all, wflag == "W", set == "Total")
# tmutate = mutate(tfilter, ace_pct_w = (aces / serve_pts), df_pct_w = (dfs / serve_pts), firstin_pct_w = (first_in / serve_pts), firstwon_pct_w = (first_won / first_in), secondwon_pct_w = (second_won / second_in), bkfcd_pct_w = (bk_pts / serve_pts), bksvd_pct_w = (bp_saved / bk_pts), retwon_pct_w = (return_pts_won / return_pts), win_uf_ratio_w = (winners / unforced))
# h3_allstats_w = tmutate
# 
# tfilter = filter(h3_all, wflag == "L", set == "Total")
# tmutate = mutate(tfilter, ace_pct_l = (aces / serve_pts), df_pct_l = (dfs / serve_pts), firstin_pct_l = (first_in / serve_pts), firstwon_pct_l = (first_won / first_in), secondwon_pct_l = (second_won / second_in), bkfcd_pct_l = (bk_pts / serve_pts), bksvd_pct_l = (bp_saved / bk_pts), retwon_pct_l = (return_pts_won / return_pts), win_uf_ratio_l = (winners / unforced))
# h3_allstats_l = tmutate
# 
# # nonupsets
# tfilter = filter(h3_nonupset, wflag == "W", set == "Total")
# tmutate = mutate(tfilter, ace_pct_w = (aces / serve_pts), df_pct_w = (dfs / serve_pts), firstin_pct_w = (first_in / serve_pts), firstwon_pct_w = (first_won / first_in), secondwon_pct_w = (second_won / second_in), bkfcd_pct_w = (bk_pts / serve_pts), bksvd_pct_w = (bp_saved / bk_pts), retwon_pct_w = (return_pts_won / return_pts), win_uf_ratio_w = (winners / unforced))
# h3_nonupsetstats_w = tmutate
# 
# tfilter = filter(h3_nonupset, wflag == "L", set == "Total")
# tmutate = mutate(tfilter, ace_pct_l = (aces / serve_pts), df_pct_l = (dfs / serve_pts), firstin_pct_l = (first_in / serve_pts), firstwon_pct_l = (first_won / first_in), secondwon_pct_l = (second_won / second_in), bkfcd_pct_l = (bk_pts / serve_pts), bksvd_pct_l = (bp_saved / bk_pts), retwon_pct_l = (return_pts_won / return_pts), win_uf_ratio_l = (winners / unforced))
# h3_nonupsetstats_l = tmutate

# now create 2 tables showing how ratios compared to prior 30 days for winner and loser, respectively
# create empty df with right column names, in for loop use rbind to create new rows, then later cbind to broader df
# think need a simple table with player, date, atpid,  and all stats; then use the function to call that table and figure out % relativity
#-------

#-------
# didn't finish this, would have worked...not enough data for most competitors
#  priordays_func = function(days = 30, data) {
#    n = length(data)
#    df <- data.frame(ace = as.numeric(), df = as.numeric(), first = as.numeric(), fwon = as.numeric(), second = as.numeric(), bkfcd = as.numeric(), bksvd = as.numeric(), ret = as.numeric(), uf = as.numeric())
#    for(i in 1:n){
#      #  bind =  if((data$atp_winner_id == data[i,data$atp_winner_id])&((data$Date < data$Date[i])&(data$Date >= (data$Date[i]-days))) {
#      #    bind = c()
#      #    mean(data$ace_pct_w, na.rm = TRUE)
#      #    mean(data$df_pct_w, na.rm = TRUE)
#      #    mean(data$firstin_pct_w, na.rm = TRUE)
#      #    mean(data$firstwon_pct_w, na.rm = TRUE)
#      #    mean(data$secondwon_pct_w, na.rm = TRUE)
#      #    mean(data$bkfcd_pct_w, na.rm = TRUE)
#      #    mean(data$bksvd_pct_w, na.rm = TRUE)
#      #    mean(data$retwon_pct_w, na.rm = TRUE)
#      #    mean(data$win_uf_ratio_w, na.rm = TRUE)
#      # }) df = rbind(df, bind)
#      i = 125
#      data = h3_all
#      days = 90
#      idx = data$player_id == data$player_id[i] & (data$Date < data$Date[i]) & (data$Date >= (data$Date[i]-days))
#      bind = c(mean(data$ace_pct_w[idx], na.rm = TRUE))
#    } return(df)
#  }
#  write.csv(h3_all, "h3allcheck.csv", row.names = TRUE)
# 
# data$Date[1] < data$Date[2]
# cbimh3_upsetstats_w$new_col <- priordays_func(30, h3_upsetstats_w)
# 
#    head(h3_all,1)
#    glimpse(h3_all)
#    
#    string = as.vector(strsplit(toString(h3_upsetstats_w$tourney_date),", "))
#    as.Date(
#    substr(string, 1,4)  
#      , "%m/%d/%Y")
# strsplit(string,", ")
# str(string)

# now need do find good visualization that compares all these stats, maybe grouped by upset type

#head(h3_upsetstats_w,2)
#write.csv(h3_all, "test_h3all.csv", row.names = TRUE)
#------

# create a summary table grouped by "class" (dog / favorite status)

h3_class_stats = h3_all %>% group_by(class) %>% 
  summarize(avg_ace_pct = (sum(aces) / sum(serve_pts)), avg_df_pct = (sum(dfs) / sum(serve_pts)), avg_firstin_pct = (sum(first_in)/sum(serve_pts)),
            avg_firstwon_pct = (sum(first_won)/sum(first_in)), avg_secondwon_pct = (sum(second_won)/sum(second_in)), avg_bkfcd_pct = 
            (sum(bk_pts)/sum(serve_pts)), avg_bksvd_pct = (sum(bp_saved)/sum(bk_pts)), avg_retwon_pct = (sum(return_pts_won)/sum(return_pts)),
            avg_win_uf_ratio = (sum(winners)/sum(unforced)))



#head(h3_all, 2)
#head(h3_class_stats)
#glimpse(a)

# add "dif"s, positive % means better performance relative to average for same class
h3_all_joinclass = left_join(h3_all, h3_class_stats, by = "class")
a = h3_all_joinclass
h3_all_joinclass = mutate(a,
      ace_dif = (ace_pct - avg_ace_pct)/avg_ace_pct,
      df_dif = (avg_df_pct - df_pct)/avg_df_pct,
      firstin_dif = (firstin_pct - avg_firstin_pct)/avg_firstin_pct,
      firstwon_dif = (firstwon_pct - avg_firstwon_pct)/avg_firstwon_pct,
      secondwon_dif = (secondwon_pct - avg_secondwon_pct)/avg_secondwon_pct,
      bkfcd_dif = (avg_bkfcd_pct - bkfcd_pct)/avg_bkfcd_pct,
      bksvd_dif = (bksvd_pct - avg_bksvd_pct)/avg_bksvd_pct,
      retwon_dif = (retwon_pct - avg_retwon_pct)/avg_retwon_pct,
      win_uf_dif = (win_uf_ratio - avg_win_uf_ratio)/avg_win_uf_ratio
      )

#head(h3_all_joinclass,1)
#write.csv(h3_all_joinclass, "joinclasstest.csv", row.names =  TRUE)



h3_all_joinclass$class = as.factor(h3_all_joinclass$class)
#levels(h3_all_joinclass$class)
h3_all_joinclass$class = factor(h3_all_joinclass$class, levels = levels(h3_all_joinclass$class)[c(5, 3, 1, 6, 7, 2, 4, 8)])
h3_all_joinclass$class_ind = ifelse(h3_all_joinclass$class %in% c("No Hope", "Heavy Dog", "Clear Dog", "Slight Dog"), "Dog", "Favorite")
h3_all_joinclass$indx = c(1:length(h3_all_joinclass$player))


# Create a summary table of each stat's total avg, upset avg, and nonupset avgs grouped by stat, upset indicator

h3_subset = select(h3_all_joinclass, indx, class_ind, upset_flag, ace_pct, df_pct, firstin_pct, firstwon_pct, secondwon_pct, bkfcd_pct, bksvd_pct, retwon_pct, win_uf_ratio)

groupby = h3_subset
  #filter(h3_subset, upset_flag == "F")
groupby = group_by(groupby, class_ind)
stat_summary = summarize(groupby, statvalue_all = mean(ace_pct, na.rm = TRUE)) #add first stat for all matches
stat_summary = rbind(stat_summary, summarize(groupby, statvalue_all = mean(df_pct, na.rm = TRUE)))
stat_summary = rbind(stat_summary, summarize(groupby, statvalue_all = mean(firstin_pct, na.rm = TRUE)))
stat_summary = rbind(stat_summary, summarize(groupby, statvalue_all = mean(firstwon_pct, na.rm = TRUE)))
stat_summary = rbind(stat_summary, summarize(groupby, statvalue_all = mean(secondwon_pct, na.rm = TRUE)))
stat_summary = rbind(stat_summary, summarize(groupby, statvalue_all = mean(bkfcd_pct, na.rm = TRUE)))
stat_summary = rbind(stat_summary, summarize(groupby, statvalue_all = mean(bksvd_pct, na.rm = TRUE)))
stat_summary = rbind(stat_summary, summarize(groupby, statvalue_all = mean(retwon_pct, na.rm = TRUE)))
stat_summary = rbind(stat_summary, summarize(groupby, statvalue_all = mean(win_uf_ratio, na.rm = TRUE)))

stat_summary$stat_type = rep(c("Ace Pct", "DF Pct", "1st In Pct", "1st Won Pct", "2nd Won Pct", "Break Faced Pct", "Break Saved Pct", "Return Won Pct", "Winner / Unforced Ratio"), c(2,2,2,2,2,2,2,2,2))

stat_summary = stat_summary[,c(3,1,2)] #re-order columns

#write.csv(stat_summary, "statsumcheck.csv", row.names = TRUE)

groupby = filter(groupby, upset_flag == "F")
stat_summaryA = summarize(groupby, statvalue_nonupset = mean(ace_pct, na.rm = TRUE))
stat_summaryA = rbind(stat_summaryA, summarize(groupby, statvalue_nonupset = mean(df_pct, na.rm = TRUE)))
stat_summaryA = rbind(stat_summaryA, summarize(groupby, statvalue_nonupset = mean(firstin_pct, na.rm = TRUE)))
stat_summaryA = rbind(stat_summaryA, summarize(groupby, statvalue_nonupset = mean(firstwon_pct, na.rm = TRUE)))
stat_summaryA = rbind(stat_summaryA, summarize(groupby, statvalue_nonupset = mean(secondwon_pct, na.rm = TRUE)))
stat_summaryA = rbind(stat_summaryA, summarize(groupby, statvalue_nonupset = mean(bkfcd_pct, na.rm = TRUE)))
stat_summaryA = rbind(stat_summaryA, summarize(groupby, statvalue_nonupset = mean(bksvd_pct, na.rm = TRUE)))
stat_summaryA = rbind(stat_summaryA, summarize(groupby, statvalue_nonupset = mean(retwon_pct, na.rm = TRUE)))
stat_summaryA = rbind(stat_summaryA, summarize(groupby, statvalue_nonupset = mean(win_uf_ratio, na.rm = TRUE)))

stat_summaryA$stat_type = rep(c("Ace Pct", "DF Pct", "1st In Pct", "1st Won Pct", "2nd Won Pct", "Break Faced Pct", "Break Saved Pct", "Return Won Pct", "Winner / Unforced Ratio"), c(2,2,2,2,2,2,2,2,2))

stat_summaryA = stat_summaryA[,c(3,1,2)] #re-order columns


# set groupby back to h3_all since last query filtered down to F only
groupby = h3_subset
groupby = group_by(groupby, class_ind)
groupby = filter(groupby, upset_flag == "U")
stat_summaryB = summarize(groupby, statvalue_upset = mean(ace_pct, na.rm = TRUE))
stat_summaryB = rbind(stat_summaryB, summarize(groupby, statvalue_upset = mean(df_pct, na.rm = TRUE)))
stat_summaryB = rbind(stat_summaryB, summarize(groupby, statvalue_upset = mean(firstin_pct, na.rm = TRUE)))
stat_summaryB = rbind(stat_summaryB, summarize(groupby, statvalue_upset = mean(firstwon_pct, na.rm = TRUE)))
stat_summaryB = rbind(stat_summaryB, summarize(groupby, statvalue_upset = mean(secondwon_pct, na.rm = TRUE)))
stat_summaryB = rbind(stat_summaryB, summarize(groupby, statvalue_upset = mean(bkfcd_pct, na.rm = TRUE)))
stat_summaryB = rbind(stat_summaryB, summarize(groupby, statvalue_upset = mean(bksvd_pct, na.rm = TRUE)))
stat_summaryB = rbind(stat_summaryB, summarize(groupby, statvalue_upset = mean(retwon_pct, na.rm = TRUE)))
stat_summaryB = rbind(stat_summaryB, summarize(groupby, statvalue_upset = mean(win_uf_ratio, na.rm = TRUE)))

stat_summaryB$stat_type = rep(c("Ace Pct", "DF Pct", "1st In Pct", "1st Won Pct", "2nd Won Pct", "Break Faced Pct", "Break Saved Pct", "Return Won Pct", "Winner / Unforced Ratio"), c(2,2,2,2,2,2,2,2,2))

stat_summaryB = stat_summaryB[,c(3,1,2)] #re-order columns

stat_summary$statvalue_nonupset = stat_summaryA$statvalue_nonupset
stat_summary$statvalue_upset = stat_summaryB$statvalue_upset

rm(stat_summaryA)
rm(stat_summaryB)

sum_group = group_by(stat_summary, stat_type, class_ind)
performance_summary = summarize(sum_group, upset_performance = (statvalue_upset - statvalue_all)/statvalue_all, nonupset_performance = (statvalue_nonupset - statvalue_all)/statvalue_all)

filter(performance_summary, class_ind == "Favorite")


# ----- PLOTS -------#


data = h3_all_joinclass
#head(data)


s1 <- ggplot(data, aes(x = ace_pct))
s2 <- ggplot(data, aes(x = df_pct))
s3 <- ggplot(data, aes(x = firstin_pct))
s4 <- ggplot(data, aes(x = firstwon_pct))
s5 <- ggplot(data, aes(x = secondwon_pct))
s6 <- ggplot(data, aes(x = bkfcd_pct))
s7 <- ggplot(data, aes(x = bksvd_pct))
s8 <- ggplot(data, aes(x = retwon_pct))
s9 <- ggplot(data, aes(x = win_uf_ratio))



#length(unique(h3_all_joinclass$atp_key)) #327 matches

s1 + 
  #ylim(0,0) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("Charted ATP Matches 2012-2014 (n = 327 Matches)\nAce % Distribution") +
  labs(x = "Ace %", y = "Density Distribution") +
  geom_density(aes(color = upset_flag)) + 
  facet_wrap(~class_ind) 

ggsave("Ace Pct Distribution.wmf", height = 3.83, width = 10.8)
  
s2 + 
  #ylim(0,0) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("Charted ATP Matches 2012-2014 (n = 327 Matches)\nDouble Fault % Distribution") +
  labs(x = "Double Fault %", y = "Density Distribution") +
  geom_density(aes(color = upset_flag)) + 
  facet_wrap(~class_ind) 

ggsave("DF Pct Distribution.wmf", height = 3.83, width = 10.8)

s3 + 
  #ylim(0,0) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("Charted ATP Matches 2012-2014 (n = 327 Matches)\n1st Serve In % Distribution") +
  labs(x = "1st Serve In %", y = "Density Distribution") +
  geom_density(aes(color = upset_flag)) + 
  facet_wrap(~class_ind)

ggsave("First Serve In Pct Distribution.wmf", height = 3.83, width = 10.8)

s4 + 
  #ylim(0,0) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("Charted ATP Matches 2012-2014 (n = 327 Matches)\n1st Serve Won % Distribution") +
  labs(x = "1st Serve Won %", y = "Density Distribution") +
  geom_density(aes(color = upset_flag)) + 
  facet_wrap(~class_ind) 

ggsave("First Serve Won Pct Distribution.wmf", height = 3.83, width = 10.8)

s5 + 
  #ylim(0,0) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("Charted ATP Matches 2012-2014 (n = 327 Matches)\n2nd Serve Won % Distribution") +
  labs(x = "2nd Serve Won %", y = "Density Distribution") +
  geom_density(aes(color = upset_flag)) + 
  facet_wrap(~class_ind) 

ggsave("Second Serve Won Pct Distribution.wmf", height = 3.83, width = 10.8)

s6 + 
  #ylim(0,0) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("Charted ATP Matches 2012-2014 (n = 327 Matches)\nBreak Pts Faced % Distribution") +
  labs(x = "Break Pts Faced %", y = "Density Distribution") +
  geom_density(aes(color = upset_flag)) + 
  facet_wrap(~class_ind) 

ggsave("Break Pts Faced Pct Distribution.wmf", height = 3.83, width = 10.8)

s7 + 
  #ylim(0,0) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("Charted ATP Matches 2012-2014 (n = 327 Matches)\nBreak Pts Saved % Distribution") +
  labs(x = "Break Pts Saved %", y = "Density Distribution") +
  geom_density(aes(color = upset_flag)) + 
  facet_wrap(~class_ind) 

ggsave("Break Pts Saved Pct Distribution.wmf", height = 3.83, width = 10.8)

s8 + 
  #ylim(0,0) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("Charted ATP Matches 2012-2014 (n = 327 Matches)\nReturn Pts Won % Distribution") +
  labs(x = "Return Pts Won %", y = "Density Distribution") +
  geom_density(aes(color = upset_flag)) + 
  facet_wrap(~class_ind) 

ggsave("Return Pts Won Pct Distribution.wmf", height = 3.83, width = 10.8)

s9 + 
  #ylim(0,0) +
  theme_hc() +
  theme(axis.text.x=element_text(size=10, vjust=0.5), axis.text.y=element_text(size=10, vjust=0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_hc(labels = c("Non-Upset", "Upset")) +
  ggtitle("Charted ATP Matches 2012-2014 (n = 327 Matches)\nWinners to Unforced Ratio Distribution") +
  labs(x = "Winners to Unforced Ratio", y = "Density Distribution") +
  geom_density(aes(color = upset_flag)) + 
  facet_wrap(~class_ind) 

ggsave("Winners to Unforced Distribution.wmf", height = 3.83, width = 10.8)








#g + geom_histogram(binwidth = 0.25) + facet_wrap(~class)





# 4: How was underdog's service game?  What's an average service game for a winner?



# 5: How was underdog's return game?  What's an average return game for a winner?
