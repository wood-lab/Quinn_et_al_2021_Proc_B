---
title: "Main statistical analyses and plots for Quinn et al. 2021"
author: "Chelsea Wood and Sarah Lee"
date: "updated 20 Nov 2020"
---
  
# Reference for analysis design: 
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction

# Load required packages

library(MASS)
library(lme4)
library(nlme)
library(skimr)
library(tidyverse)


# Read in clean data
# REMEMBER TO UPDATE THE DATE OF THE CSV FILE IF YOU'VE RECENTLY RE-DOWNLOADED AND CLEANED THE DATA!!!!

data01<-read_csv("data/raw_data.csv", col_types = cols(
  locality = col_factor(),
  sex = col_factor(),
  host_size = col_number(),
  count_portunion = col_number(),
  crab_spp = col_factor(),
  sex = col_factor(),
  data_type = col_factor()
))

# How many crabs are there in total?
length(data01$Line)

# How many crabs in the full dataset?
n_full <- data01 %>%
  group_by(data_type, sex) %>%
  summarise(n = n())

# Yes, there really are only 10 males in the historical dataset
n_thing <- data01 %>%
  filter(data_type == "hist", sex == "M")

# Dataset including sex = F only, no sex term in model
data02<-data01 %>%
  filter(sex == "F")

# How many of those crabs were female?
length(data02$Line)

# Include only localities that have sufficient sampling at both historical and contemporary time points (i.e., n = 10).

n_site <- data02 %>%
  group_by(locality, crab_spp, data_type) %>%
  summarise(n = n())
View(n_site)

sum(n_site$n)

# H. oregonensis sites: TN, NB, SP, #BM, YB, #YH, AL, #MB, #WT, #PO, FB, UC, #SB, #FP, #SS, #WR, #FH, #AP = 7
# H. nudus sites: TN, NB, SP, BM, #YB, YH, #AL, #MB, #WT, #PO, #FB, #UC, #SB, #FP, #SS, #WR, FH, AP = 7
# Full list of sites: TN, NB, SP, BM, YB, YH, AL, FB, UC, FH, AP = 11

data02_Ho <- data02 %>%
  filter(crab_spp == "H.o") %>%
  filter(locality == "TN"|locality =="NB"|locality =="SP"|locality =="YB"|locality =="AL"|locality =="FB"|locality =="UC")
skim(data02_Ho)
data02_Hn <- data02 %>%
  filter(crab_spp == "H.n") %>%
  filter(locality == "TN"|locality =="NB"|locality =="SP"|locality =="BM"|locality =="YH"|locality == "FH"|locality=="AP")
skim(data02_Hn)
data03<-bind_rows(data02_Hn, data02_Ho)

length(data03$Line)

# Verify that there are seven sites (3 in common, 8 not in common) for both Ho and Hn
site_n <- data03 %>%
  group_by(crab_spp, locality, data_type) %>%
  summarise(n = n())
View(site_n)

# Verify that sites were sampled at a similar time of year when comparing across historical/contemporary and within historical/
# contemporary

View(data03)

site_dates_01 <- data03 %>%
  group_by(data_type, locality, collection_date) %>%
  summarise(n = n())
View(site_dates_01)

# Spread dataset so that cont and hist are variables

site_dates_02<-spread(site_dates_01, data_type, collection_date)
View(site_dates_02)

# This will be Supplementary Information Table S3

write.csv(site_dates_02,"data/sampling_dates.csv")


######### Statistical models ##################

data<-data03
length(data$Line)

# Dataset including Hn only
Hn_data<-data %>%
  filter(crab_spp == "H.n")
length(Hn_data$Line)

# Dataset including Ho only
Ho_data<-data %>%
  filter(crab_spp == "H.o")
length(Ho_data$Line)

1035+755

# Start by doing some basic summary statistics.

# proportion of crabs infected with Portunion
tally(data[which(data$count_portunion>0),])
tally(data)
644/1790

tally(data[which(data$count_portunion>0 & data$data_type == "hist"),])
tally(data[which(data$data_type == "hist"),])
419/1204

tally(data[which(data$count_portunion>0 & data$data_type == "cont"),])
tally(data[which(data$data_type == "cont"),])
225/586

# proportion of H.o infected with Portunion
tally(data[which(data$count_portunion>0 & data$data_type == "hist" & data$crab_spp == "H.o"),])
tally(data[which(data$data_type == "hist" & data$crab_spp == "H.o"),])
311/747

tally(data[which(data$count_portunion>0 & data$data_type == "cont" & data$crab_spp == "H.o"),])
tally(data[which(data$data_type == "cont" & data$crab_spp == "H.o"),])
105/288

# number of Portunion in H.o

thing<-(data[which(data$count_portunion>0 & data$data_type == "hist" & data$crab_spp == "H.o"),])
sum(thing$count_portunion)
508

thing<-(data[which(data$count_portunion>0 & data$data_type == "cont" & data$crab_spp == "H.o"),])
sum(thing$count_portunion)
134

# proportion of H.n infected with Portunion
tally(data[which(data$count_portunion>0 & data$data_type == "hist" & data$crab_spp == "H.n"),])
tally(data[which(data$data_type == "hist" & data$crab_spp == "H.n"),])
108/457

tally(data[which(data$count_portunion>0 & data$data_type == "cont" & data$crab_spp == "H.n"),])
tally(data[which(data$data_type == "cont" & data$crab_spp == "H.n"),])
120/298

457+298

# number of Portunion in H.n

thing<-(data[which(data$count_portunion>0 & data$data_type == "hist" & data$crab_spp == "H.n"),])
sum(thing$count_portunion)
122

thing<-(data[which(data$count_portunion>0 & data$data_type == "cont" & data$crab_spp == "H.n"),])
sum(thing$count_portunion)
185

# proportion of crabs infected with acanths
tally(data[which(data$cystacanths>0 & data$crab_spp == "H.n" & data$data_type == "hist"),])
tally(data[which(data$crab_spp == "H.n" & data$data_type == "hist"),])
0/457

tally(data[which(data$cystacanths>0 & data$crab_spp == "H.n" & data$data_type == "cont"),])
tally(data[which(data$crab_spp == "H.n" & data$data_type == "cont"),])
1/298

1/755

tally(data[which(data$cystacanths>0 & data$crab_spp == "H.o"),])
tally(data[which(data$crab_spp == "H.o"),])
38/1035

tally(data[which(data$cystacanths>0 & data$crab_spp == "H.o" & data$data_type == "hist"),])
tally(data[which(data$crab_spp == "H.o" & data$data_type == "hist"),])
31/747

tally(data[which(data$cystacanths>0 & data$crab_spp == "H.o" & data$data_type == "cont"),])
tally(data[which(data$crab_spp == "H.o" & data$data_type == "cont"),])
7/288

# number of acanths in H.o

thing<-(data[which(data$cystacanths>0 & data$data_type == "hist" & data$crab_spp == "H.o"),])
sum(thing$cystacanths)
32

thing<-(data[which(data$cystacanths>0 & data$data_type == "cont" & data$crab_spp == "H.o"),])
sum(thing$cystacanths)
10

# number of acanths in H.n

thing<-(data[which(data$cystacanths>0 & data$data_type == "hist" & data$crab_spp == "H.n"),])
sum(thing$cystacanths)
0

thing<-(data[which(data$cystacanths>0 & data$data_type == "cont" & data$crab_spp == "H.n"),])
sum(thing$cystacanths)
1


# proportion of crabs infected with metacercariae - just count H. oregonensis (H.n have very few infections)
tally(data[which(data$metacerc_rel_abund>0 & data$crab_spp == "H.n"),])
tally(data[which(data$crab_spp == "H.n"),])
12/755

tally(data[which(data$metacerc_rel_abund>0 & data$data_type == "hist" & data$crab_spp == "H.n"),])
tally(data[which(data$data_type == "hist" & data$crab_spp == "H.n"),])
8/457

tally(data[which(data$metacerc_rel_abund>0 & data$data_type == "cont" & data$crab_spp == "H.n"),])
tally(data[which(data$data_type == "cont" & data$crab_spp == "H.n"),])
4/298

#make sure to count only the crabs that WERE NOT excluded (i.e., exclude 1970)
tally(data[which(data$metacerc_rel_abund>0 & data$crab_spp == "H.o" & data$year !=1970),])
tally(data[which(data$crab_spp == "H.o" & data$year !=1970),])
213/706

tally(data[which(data$metacerc_rel_abund>0 & data$data_type == "hist" & data$crab_spp == "H.o" & data$year == 1969),])
tally(data[which(data$data_type == "hist" & data$crab_spp == "H.o" & data$year == 1969),])
35/418

tally(data[which(data$metacerc_rel_abund>0 & data$data_type == "cont" & data$crab_spp == "H.o"),])
tally(data[which(data$data_type == "cont" & data$crab_spp == "H.o"),])
178/288

tally(data[which(data$metacerc_rel_abund>0 & data$data_type == "hist" & data$crab_spp == "H.n" & data$year == 1969),])
tally(data[which(data$data_type == "hist" & data$crab_spp == "H.n"  & data$year == 1969),])
0/320

tally(data[which(data$metacerc_rel_abund>0 & data$data_type == "cont" & data$crab_spp == "H.n"),])
tally(data[which(data$data_type == "cont" & data$crab_spp == "H.n"),])
4/298

# proportion of crabs infected with metacercariae IN EACH YEAR - just count H. oregonensis (H.n have very few infections)
tally(data[which(data$metacerc_rel_abund>0 & data$crab_spp == "H.o" & data$year == 1969),])
tally(data[which(data$crab_spp == "H.o" & data$year == 1969),])
35/418

tally(data[which(data$metacerc_rel_abund>0 & data$crab_spp == "H.o" & data$year == 2018),])
tally(data[which(data$crab_spp == "H.o" & data$year == 2018),])
91/164

tally(data[which(data$metacerc_rel_abund>0 & data$crab_spp == "H.o" & data$year == 2019),])
tally(data[which(data$crab_spp == "H.o" & data$year == 2019),])
72/96

tally(data[which(data$metacerc_rel_abund>0 & data$crab_spp == "H.o" & data$year == 2020),])
tally(data[which(data$crab_spp == "H.o" & data$year == 2020),])
15/28


# tally up metacercarial prevalence for each site

data$if_else_metacerc<-if_else(data$metacerc_rel_abund>0,1,0)

metacerc_sites <- data %>%
  group_by(data_type, locality) %>%
  summarise(positive = mean(if_else_metacerc), n = n())
View(metacerc_sites)

final_metacerc_sites<-metacerc_sites %>%
  pivot_wider(names_from = data_type, values_from = positive)
View(final_metacerc_sites)




# PORTUNION

# First, run the data_type models.

data$year<-as.factor(data$year)
str(data)

model_port_data_type<-count_portunion ~ data_type*crab_spp + host_size + (1|locality) + (1|data_type/year)

# Fit a normal distribution
model_norm_port_dt<-lmer(formula = model_port_data_type, data=data)
summary(model_norm_port_dt)	
AIC(model_norm_port_dt)
#AIC = 4503.27

model_pois_port_dt<-glmer(formula = model_port_data_type, data=data, family="poisson")
summary(model_pois_port_dt)	
AIC(model_pois_port_dt)
#AIC = 3047.381

model_nbinom_port_dt<-glmer.nb(formula = model_port_data_type, data=data)
summary(model_nbinom_port_dt)	
AIC(model_nbinom_port_dt)
#AIC = 3035.389
#SELECT THIS MODEL (lowest AIC)


#Now sub H.n into the reference position
data$crab_spp_Ho<-gsub("H.o","aaH.o",data$crab_spp)
model_port_data_type_Hn<-count_portunion ~ data_type*crab_spp_Ho + host_size + (1|locality) + (1|data_type/year)
model_nbinom_port_dt_Hn<-glmer.nb(formula = model_port_data_type_Hn, data=data)
summary(model_nbinom_port_dt_Hn)


library(ggeffects)
library(sjstats)
pr_port_dt<-ggeffect(model_nbinom_port_dt,c("data_type","crab_spp"),ci.lvl=0.95)

portunion_plot<-ggplot(pr_port_dt,aes(x,predicted,color=group),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(size=4)+
  geom_errorbar(data=pr_port_dt,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  geom_line(aes(group=group))+
  xlab("")+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=14),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=rev(levels(pr_port_dt$x)),labels=c("historical","contemporary"))+
  theme(legend.position="none")
portunion_plot


# Now, run the year models.

model_port_year<-count_portunion ~ year*crab_spp + host_size + (1|locality) + (1|data_type/year)

# Fit a normal distribution
model_norm_port_year<-lmer(formula = model_port_year, data=data)
summary(model_norm_port_year)	
AIC(model_norm_port_year)
#AIC = 4518.184

model_pois_port_year<-glmer(formula = model_port_year, data=data, family="poisson")
summary(model_pois_port_year)	
AIC(model_pois_port_year)
#AIC = 3042.594

model_nbinom_port_year<-glmer.nb(formula = model_port_year, data=data)
summary(model_nbinom_port_year)	
AIC(model_nbinom_port_year)
#AIC = 3031.7
#SELECT THIS MODEL (lowest AIC)


#Now sub 1970 into the reference position
data$year_1970<-gsub("1970","001970",data$year)
model_port_year_1970<-count_portunion ~ year_1970*crab_spp + host_size + (1|locality) + (1|data_type/year)
model_nbinom_port_year_1970<-glmer.nb(formula = model_port_year_1970, data=data)
summary(model_nbinom_port_year_1970)

#Now sub 2018 into the reference position
data$year_2018<-gsub("2018","002018",data$year)
model_port_year_2018<-count_portunion ~ year_2018*crab_spp + host_size + (1|locality) + (1|data_type/year)
model_nbinom_port_year_2018<-glmer.nb(formula = model_port_year_2018, data=data)
summary(model_nbinom_port_year_2018)

#Now sub H.o into the reference position
data$crab_spp_Ho<-gsub("H.o","aaH.o",data$crab_spp)
model_port_year_Ho<-count_portunion ~ year*crab_spp_Ho + host_size + (1|locality) + (1|data_type/year)
model_nbinom_port_year_Ho<-glmer.nb(formula = model_port_year_Ho, data=data)
summary(model_nbinom_port_year_Ho)

#Now sub 1970 into the reference position
data$year_1970<-gsub("1970","001970",data$year)
model_port_year_1970_Ho<-count_portunion ~ year_1970*crab_spp_Ho + host_size + (1|locality) + (1|data_type/year)
model_nbinom_port_year_1970_Ho<-glmer.nb(formula = model_port_year_1970_Ho, data=data)
summary(model_nbinom_port_year_1970_Ho)

#Now sub 2018 into the reference position
data$year_2018<-gsub("2018","002018",data$year)
model_port_year_2018_Ho<-count_portunion ~ year_2018*crab_spp_Ho + host_size + (1|locality) + (1|data_type/year)
model_nbinom_port_year_2018_Ho<-glmer.nb(formula = model_port_year_2018_Ho, data=data)
summary(model_nbinom_port_year_2018_Ho)


library(ggeffects)
library(sjstats)
pr_port_year<-ggeffect(model_nbinom_port_year,c("year","crab_spp"),ci.lvl=0.95)

pr_port_year$x<-as.numeric(as.character(pr_port_year$x))

portunion_plot_year<-ggplot(pr_port_year,aes(x,predicted,color=group),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(size=2)+
  geom_errorbar(data=pr_port_year,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  #geom_line(aes(group=group))+
  xlab("")+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=10),axis.title.y=element_text(size=14),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_continuous(name="year",limits=c(1960,2028))+
  theme(legend.position="none")
portunion_plot_year


#Now make one big plot that has the data_type and year predictions.

pr_port_dt$x<-c(2019,1969.5,2019,1969.5)

portunion_plot<-ggplot(pr_port_dt,aes(x,predicted,color=group),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"),name="crab species",labels=c(expression(paste(italic("H. nudus"))),
                                                                                expression(paste(italic("H. oregonensis")))))+
  geom_point(size=5,alpha=0.5)+
  geom_errorbar(data=pr_port_dt,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=1,size=1)+
  geom_line(aes(group=group))+
  geom_point(data=pr_port_year,mapping=aes(x=x))+
  geom_errorbar(data=pr_port_year,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  xlab("")+
  ylab(expression(atop("predicted abundance of",italic("P. conformis"))))+
  theme_minimal()+
  theme(legend.title=element_text(size=18),legend.text=element_text(size=18),plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=14),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_continuous(name="",limits=c(1960,2028))+
  theme(legend.position="top")
portunion_plot


# We need to figure out whether the magnitude of long-term change is greater than the magnitude of short-term change.
# For each site, find the difference between all year combinations and ask: are adjacent years less different than distant years?

str(data)

data$unique_ID<-paste(data$crab_spp,data$locality,data$year,sep="_")
data$metacerc_y_n<-gsub("2", "1", data$metacerc_rel_abund)
data$metacerc_y_n<-gsub("3", "1", data$metacerc_y_n)
aggregated_data<-aggregate(cbind(data$count_portunion,data$cystacanths,as.numeric(data$metacerc_y_n),
                                 as.numeric(as.character(data$year))),by=list(data$unique_ID),FUN=mean,na.rm=TRUE)
names(aggregated_data)<-c("unique_ID","count_portunion","cystacanths","metacerc_rel_abund","year")

# Also aggregate the categorical variables you need

#crab_spp
aggregate(cbind((data$crab_spp)),by=list(data$unique_ID),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_crab_spp<-aggregate(cbind((data$crab_spp)),by=list(data$unique_ID),function(x){
  levels(data$crab_spp)[max(as.numeric(x))]})

#locality
aggregate(cbind((data$locality)),by=list(data$unique_ID),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_locality<-aggregate(cbind((data$locality)),by=list(data$unique_ID),function(x){
  levels(data$locality)[max(as.numeric(x))]})

final_data<-cbind.data.frame(aggregated_data,agg_crab_spp$V1,agg_locality$V1)
names(final_data)<-c("unique_ID","count_portunion","cystacanths","metacerc_rel_abund","year","crab_spp","locality")
final_data$unique_crab_locality<-paste(final_data$crab_spp,final_data$locality,sep="_")

# Now you can ask whether contrasts between years differ

library(tidyr)
final_data_wide<-spread(final_data,year,count_portunion)
names(final_data_wide)<-c("unique_ID","cystacanths","metacerc_rel_abund","crab_spp","locality","unique_crab_locality","y1969","y1970","y2018","y2019","y2020")

final_data_condensed<-aggregate(cbind(final_data_wide$y1969,final_data_wide$y1970,final_data_wide$y2018,final_data_wide$y2019,final_data_wide$y2020),
                                by=list(final_data_wide$unique_crab_locality),FUN=median,na.rm=TRUE)
names(final_data_condensed)<-c("unique_crab_locality","y1969","y1970","y2018","y2019","y2020")

#crab_spp
aggregate(cbind((final_data$crab_spp)),by=list(final_data$unique_crab_locality),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_crab_spp<-aggregate(cbind((final_data$crab_spp)),by=list(final_data$unique_crab_locality),function(x){
  levels(final_data$crab_spp)[max(as.numeric(x))]})

#locality
aggregate(cbind((final_data$locality)),by=list(final_data$unique_crab_locality),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_locality<-aggregate(cbind((final_data$locality)),by=list(final_data$unique_crab_locality),function(x){
  levels(final_data$locality)[max(as.numeric(x))]})

final_final_data<-cbind.data.frame(final_data_condensed,agg_crab_spp$V1,agg_locality$V1)
names(final_final_data)<-c("unique_crab_locality","y1969","y1970","y2018","y2019","y2020","crab_spp","crab_locality")


# Find the differences for each site/crab combination

final_final_data$y1969.1970<-abs((final_final_data$y1970)-(final_final_data$y1969))
final_final_data$y1969.2018<-abs((final_final_data$y2018)-(final_final_data$y1969))
final_final_data$y1969.2019<-abs((final_final_data$y2019)-(final_final_data$y1969))
final_final_data$y1969.2020<-abs((final_final_data$y2020)-(final_final_data$y1969))
final_final_data$y1970.2018<-abs((final_final_data$y2018)-(final_final_data$y1970))
final_final_data$y1970.2019<-abs((final_final_data$y2019)-(final_final_data$y1970))
final_final_data$y1970.2020<-abs((final_final_data$y2020)-(final_final_data$y1970))
final_final_data$y2018.2019<-abs((final_final_data$y2019)-(final_final_data$y2018))
final_final_data$y2018.2020<-abs((final_final_data$y2020)-(final_final_data$y2018))
final_final_data$y2019.2020<-abs((final_final_data$y2020)-(final_final_data$y2019))

diffs<-gather(final_final_data,contrast,difference,y1969.1970:y2019.2020,factor_key=TRUE)

diffs$time_diff

for(i in 1:length(diffs$unique_crab_locality)){
  if(diffs$contrast[i]=="y1969.1970" | diffs$contrast[i]=="y2018.2019" | diffs$contrast[i]=="y2019.2020" | diffs$contrast[i]=="y2018.2020")
    diffs$time_diff[i]<-"interannual"
  else
    diffs$time_diff[i]<-"longterm"
}

hist(diffs$difference)
hist(diffs$difference^(1/4))

diffs$trans_difference<-(diffs$difference^(1/4))

model<-lmer(difference~time_diff*crab_spp+(1|crab_locality),data=diffs)
summary(model)
AIC(model)
# AIC = 75.28064

# extract coefficients
coefs <- data.frame(coef(summary(model)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs


# Make ESM Figure S2

port_timediff_plot_1969_1970<-ggplot(final_final_data,aes(y1969,y1970,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y1969),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab(expression(paste(italic("P. conformis")," abundance in 1969")))+
  ylab(expression(paste(italic("P. conformis")," abundance in 1970")))+
  theme_minimal()+
  xlim(0,2.2)+
  ylim(0,2)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
port_timediff_plot_1969_1970

port_timediff_plot_2018_2019<-ggplot(final_final_data,aes(y2018,y2019,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y2018),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab(expression(paste(italic("P. conformis")," abundance in 2018")))+
  ylab(expression(paste(italic("P. conformis")," abundance in 2019")))+
  theme_minimal()+
  xlim(0,2.2)+
  ylim(0,2)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
port_timediff_plot_2018_2019

port_timediff_plot_2019_2020<-ggplot(final_final_data,aes(y2019,y2020,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y2019),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab(expression(paste(italic("P. conformis")," abundance in 2019")))+
  ylab(expression(paste(italic("P. conformis")," abundance in 2020")))+
  theme_minimal()+
  xlim(0,2.2)+
  ylim(0,2)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
port_timediff_plot_2019_2020

port_timediff_plot_1969_2018<-ggplot(final_final_data,aes(y1969,y2018,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y1969),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab(expression(paste(italic("P. conformis")," abundance in 1969")))+
  ylab(expression(paste(italic("P. conformis")," abundance in 2018")))+
  theme_minimal()+
  xlim(0,2.2)+
  ylim(0,2)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
port_timediff_plot_1969_2018

port_timediff_plot_1969_2019<-ggplot(final_final_data,aes(y1969,y2019,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y1969),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab(expression(paste(italic("P. conformis")," abundance in 1969")))+
  ylab(expression(paste(italic("P. conformis")," abundance in 2019")))+
  theme_minimal()+
  xlim(0,2.2)+
  ylim(0,2)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
port_timediff_plot_1969_2019

port_timediff_plot_1970_2020<-ggplot(final_final_data,aes(y1970,y2020,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y1970),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab(expression(paste(italic("P. conformis")," abundance in 1970")))+
  ylab(expression(paste(italic("P. conformis")," abundance in 2020")))+
  theme_minimal()+
  xlim(0,2.2)+
  ylim(0,2)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
port_timediff_plot_1970_2020

port_timediff_plot_1970_2018<-ggplot(final_final_data,aes(y1970,y2018,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y1970),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab(expression(paste(italic("P. conformis")," abundance in 1970")))+
  ylab(expression(paste(italic("P. conformis")," abundance in 2018")))+
  theme_minimal()+
  xlim(0,2.2)+
  ylim(0,2)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
port_timediff_plot_1970_2018

port_timediff_plot_1970_2019<-ggplot(final_final_data,aes(y1970,y2019,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y1970),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab(expression(paste(italic("P. conformis")," abundance in 1970")))+
  ylab(expression(paste(italic("P. conformis")," abundance in 2019")))+
  theme_minimal()+
  xlim(0,2.2)+
  ylim(0,2)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
port_timediff_plot_1970_2019

library(cowplot)

ggdraw(plot=NULL,xlim=c(0,30),ylim=c(0,30))+
  draw_plot(port_timediff_plot_1969_1970,x=0,y=20,width=9.5,height=9)+
  draw_plot(port_timediff_plot_2018_2019,x=10,y=20,width=9.5,height=9)+
  draw_plot(port_timediff_plot_2019_2020,x=20,y=20,width=9.5,height=9)+
  draw_plot(port_timediff_plot_1969_2018,x=0,y=10,width=9.5,height=9)+
  draw_plot(port_timediff_plot_1969_2019,x=10,y=10,width=9.5,height=9)+
  draw_plot(port_timediff_plot_1970_2018,x=0,y=0,width=9.5,height=9)+
  draw_plot(port_timediff_plot_1970_2019,x=10,y=0,width=9.5,height=9)+
  draw_plot(port_timediff_plot_1970_2020,x=20,y=0,width=9.5,height=9)+
  draw_label("(a)",x=1,y=29.75,size=20)+
  draw_label("(b)",x=11,y=29.75,size=20)+
  draw_label("(c)",x=21,y=29.75,size=20)+
  draw_label("(d)",x=1,y=19.75,size=20)+
  draw_label("(e)",x=11,y=19.75,size=20)+
  draw_label("(f)",x=1,y=9.75,size=20)+
  draw_label("(g)",x=11,y=9.75,size=20)+
  draw_label("(h)",x=21,y=9.75,size=20)






############ ACANTHOCEPHALANS ############ 

data$year<-as.factor(data$year)
str(data)

model_cyst<-cystacanths ~ data_type*crab_spp + host_size + (1|locality) + (1|data_type/year)

# Cystacanths were never observed among H.n historically.

data_cyst_cont<-data %>%
  filter(crab_spp == "H.n", data_type == "cont")

data_cyst_cont_positive<-data %>%
  filter(crab_spp == "H.n", data_type == "cont", cystacanths>0)
head(data_cyst_cont_positive)
length(data_cyst_cont_positive$cystacanths)

# Looks like cystacanths were only observed ONCE in H.n, at the contemporary time point. Exclude H.n from acanth models.

# Make a dataset with no H.n

data_cyst<-data %>%
  filter(crab_spp == "H.o")

data_cyst_count<-data_cyst %>%
  filter(cystacanths >= 0)
length(data_cyst_count$cystacanths)

hist(data_cyst$cystacanths)

# Make a model with no host species term

model_cyst<-cystacanths ~ data_type + host_size + (1|locality) + (1|data_type/year)

model_norm_cyst<-glmer(formula = model_cyst, data=data_cyst, family="gaussian")
summary(model_norm_cyst)	
AIC(model_norm_cyst)
#AIC = -162.2459

model_pois_cyst<-glmer(formula = model_cyst, data=data_cyst, family="poisson")
summary(model_pois_cyst)	
AIC(model_pois_cyst)
#AIC = 347.6661

model_nbinom_cyst<-glmer.nb(formula = model_cyst, data=data_cyst)
summary(model_nbinom_cyst)	
AIC(model_nbinom_cyst)
#AIC = 345.7905

# extract coefficients
coefs <- data.frame(coef(summary(model_norm_cyst)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

library(ggeffects)
pr_cyst<-ggeffect(model_norm_cyst,c("data_type [cont, hist]"))

cyst_plot<-ggplot(pr_cyst,aes(x,predicted,color=group))+
  scale_color_manual(values=c("#ca0020"))+
  geom_point(size=5,alpha=0.5)+
  geom_errorbar(data=pr_cyst,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03,size=1)+
  geom_line(aes(group=group))+
  xlab("")+
  ylab("predicted abundance of\nacanthocephalans")+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=13),axis.text.x=element_text(size=16),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=rev(levels(pr_cyst$x)),labels=c("historical","contemporary"))+
  theme(legend.position="none")
cyst_plot


# Now, run the year models.

model_cyst_year<-cystacanths ~ year + host_size + (1|locality) + (1|data_type/year)

# Make sure it is recognizing year as a factor, not a numeric

str(data)

# Fit a normal distribution
model_norm_cyst_year<-lmer(formula = model_cyst_year, data=data)
summary(model_norm_cyst_year)	
AIC(model_norm_cyst_year)
#AIC = -1209.188
#SELECT THIS MODEL (lowest AIC)

model_pois_cyst_year<-glmer(formula = model_cyst_year, data=data, family="poisson")
summary(model_pois_cyst_year)	
AIC(model_pois_cyst_year)
#AIC = 371.8176

model_nbinom_cyst_year<-glmer.nb(formula = model_cyst_year, data=data)
summary(model_nbinom_cyst_year)	
AIC(model_nbinom_cyst_year)
#AIC = 370.0195


#N ow sub 1970 into the reference position
data$year_1970<-gsub("1970","001970",data$year)
model_cyst_year_1970<-cystacanths ~ year_1970 + host_size + (1|locality) + (1|data_type/year)
model_nbinom_cyst_year_1970<-glmer.nb(formula = model_cyst_year_1970, data=data)
summary(model_nbinom_cyst_year_1970)

# Now sub 2018 into the reference position
data$year_2018<-gsub("2018","002018",data$year)
model_cyst_year_2018<-cystacanths ~ year_2018 + host_size + (1|locality) + (1|data_type/year)
model_nbinom_cyst_year_2018<-glmer.nb(formula = model_cyst_year_2018, data=data)
summary(model_nbinom_cyst_year_2018)

# Now sub H.o into the reference position
data$crab_spp_Ho<-gsub("H.o","aaH.o",data$crab_spp)
model_cyst_year_Ho<-cystacanths ~ year + host_size + (1|locality) + (1|data_type/year)
model_nbinom_cyst_year_Ho<-glmer.nb(formula = model_cyst_year_Ho, data=data)
summary(model_nbinom_cyst_year_Ho)

# Now sub 1970 into the reference position
data$year_1970<-gsub("1970","001970",data$year)
model_cyst_year_1970_Ho<-cystacanths ~ year_1970 + host_size + (1|locality) + (1|data_type/year)
model_nbinom_cyst_year_1970_Ho<-glmer.nb(formula = model_cyst_year_1970_Ho, data=data)
summary(model_nbinom_cyst_year_1970_Ho)

# Now sub 2018 into the reference position
data$year_2018<-gsub("2018","002018",data$year)
model_cyst_year_2018_Ho<-cystacanths ~ year_2018 + host_size + (1|locality) + (1|data_type/year)
model_nbinom_cyst_year_2018_Ho<-glmer.nb(formula = model_cyst_year_2018_Ho, data=data)
summary(model_nbinom_cyst_year_2018_Ho)


library(ggeffects)
library(sjstats)
pr_cyst_year<-ggeffect(model_norm_cyst_year,c("year"),ci.lvl=0.95)

pr_cyst_year$x<-as.numeric(as.character(pr_cyst_year$x))

cyst_plot_year<-ggplot(pr_cyst_year,aes(x,predicted,color=group),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(size=2)+
  geom_errorbar(data=pr_cyst_year,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  #geom_line(aes(group=group))+
  xlab("")+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=18),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_continuous(name="year",limits=c(1960,2028))+
  theme(legend.position="none")
cyst_plot_year


# Now make one big plot that has the data_type and year predictions.

pr_cyst$x<-c(1969.5,2019)
pr_cyst_year$x

cyst_plot_final<-ggplot(pr_cyst,aes(x,predicted,color=group),grouping=year)+
  scale_color_manual(values=c("#ca0020","#ca0020"))+
  geom_point(size=10,alpha=0.5)+
  geom_errorbar(data=pr_cyst,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=1,size=1)+
  geom_line(aes(group=group))+
  geom_point(data=pr_cyst_year,mapping=aes(x=x))+
  geom_errorbar(data=pr_cyst_year,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  xlab("")+
  ylab("predicted abundance of acanthocephalans")+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=18),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_continuous(name="",limits=c(1960,2028))+
  theme(legend.position="none")
cyst_plot_final


# We need to figure out whether the magnitude of long-term change is greater than the magnitude of short-term change.
# For each site, find the difference between all year combinations and ask: are adjacent years less different than distant years?

# Now you can ask whether contrasts between years differ

library(tidyr)
final_data_wide<-spread(final_data,year,cystacanths)
names(final_data_wide)<-c("unique_ID","count_portunion","metacerc_rel_abund","crab_spp","locality","unique_crab_locality","y1969","y1970","y2018","y2019","y2020")

final_data_condensed<-aggregate(cbind(final_data_wide$y1969,final_data_wide$y1970,final_data_wide$y2018,final_data_wide$y2019,final_data_wide$y2020),
                                by=list(final_data_wide$unique_crab_locality),FUN=median,na.rm=TRUE)
names(final_data_condensed)<-c("unique_crab_locality","y1969","y1970","y2018","y2019","y2020")

#crab_spp
aggregate(cbind((final_data$crab_spp)),by=list(final_data$unique_crab_locality),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_crab_spp<-aggregate(cbind((final_data$crab_spp)),by=list(final_data$unique_crab_locality),function(x){
  levels(final_data$crab_spp)[max(as.numeric(x))]})

#locality
aggregate(cbind((final_data$locality)),by=list(final_data$unique_crab_locality),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_locality<-aggregate(cbind((final_data$locality)),by=list(final_data$unique_crab_locality),function(x){
  levels(final_data$locality)[max(as.numeric(x))]})

final_final_data<-cbind.data.frame(final_data_condensed,agg_crab_spp$V1,agg_locality$V1)
names(final_final_data)<-c("unique_crab_locality","y1969","y1970","y2018","y2019","y2020","crab_spp","crab_locality")


# Find the differences for each site/crab combination

final_final_data$y1969.1970<-abs((final_final_data$y1970)-(final_final_data$y1969))
final_final_data$y1969.2018<-abs((final_final_data$y2018)-(final_final_data$y1969))
final_final_data$y1969.2019<-abs((final_final_data$y2019)-(final_final_data$y1969))
final_final_data$y1969.2020<-abs((final_final_data$y2020)-(final_final_data$y1969))
final_final_data$y1970.2018<-abs((final_final_data$y2018)-(final_final_data$y1970))
final_final_data$y1970.2019<-abs((final_final_data$y2019)-(final_final_data$y1970))
final_final_data$y1970.2020<-abs((final_final_data$y2020)-(final_final_data$y1970))
final_final_data$y2018.2019<-abs((final_final_data$y2019)-(final_final_data$y2018))
final_final_data$y2018.2020<-abs((final_final_data$y2020)-(final_final_data$y2018))
final_final_data$y2019.2020<-abs((final_final_data$y2020)-(final_final_data$y2019))

diffs<-gather(final_final_data,contrast,difference,y1969.1970:y2019.2020,factor_key=TRUE)

diffs$time_diff

for(i in 1:length(diffs$unique_crab_locality)){
  if(diffs$contrast[i]=="y1969.1970" | diffs$contrast[i]=="y2018.2019")
    diffs$time_diff[i]<-"interannual"
  else
    diffs$time_diff[i]<-"longterm"
}

model<-lmer(difference~time_diff+crab_spp+(1|crab_locality),data=diffs)
summary(model)
AIC(model)

# extract coefficients
coefs <- data.frame(coef(summary(model)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

hist(diffs$difference)



############ METACERCARIAE ############ 

# The problem here is that we have three categories for metacercarial abundance:  
# If no metacercariae were present, a ‘–‘ was recorded. 
# If 1 to 5 metacercariae were found in the crab, a ‘+’ was recorded. 
# If 6–25 metacercariae were found in the crab, a ‘++’ was recorded. 
# For any number of metacercariae found in the crab greater than 25, a ‘+++’ was recorded. 
# This corresponds to four categories in the dataset: 0, 1, 2, 3

# We can't use normal glmers for ordinal data like these. We will instead use an ordinal logistic regression.

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

# Were metacerc observed enough in each species to look for change over time?

data_meta_cont_positive<-data %>%
  filter(crab_spp == "H.n", data_type == "cont", metacerc_rel_abund>0)
length(data_meta_cont_positive$metacerc_rel_abund)
# Only four observed in contemporary data among H.n

data_meta_cont_positive<-data %>%
  filter(crab_spp == "H.n", data_type == "hist", metacerc_rel_abund>0)
length(data_meta_cont_positive$metacerc_rel_abund)
# Only eight observed in historical data among H.n

# Let's count just H.o
data_meta_cont_positive<-data %>%
  filter(crab_spp == "H.o", data_type == "hist", metacerc_rel_abund>0)
length(data_meta_cont_positive$metacerc_rel_abund)

data_meta_cont_positive<-data %>%
  filter(crab_spp == "H.o", data_type == "cont", metacerc_rel_abund>0)
length(data_meta_cont_positive$metacerc_rel_abund)


# Make a dataset with no H.n

data_meta_01<-data %>%
  filter(crab_spp == "H.o")

hist(data_meta$metacerc_rel_abund)

# Make a dataset with no historical crabs from 1970 (bc Armand marked whole pages rather than individual crabs in that year)

data_meta<-data_meta_01 %>%
  filter(year != "1970")

hist(data_meta$year)

data_check_n<-data_meta %>%
  filter(metacerc_rel_abund != "NA")
length(data_check_n$Line)
levels(as.factor(data_meta$year))

# We need to do an ordinal linear regression with random effects.

library(ordinal)

str(data_meta)
data_meta$metacerc_rel_abund_factor<-as.factor(data_meta$metacerc_rel_abund)
data_meta$year<-as.factor(data_meta$year)
str(data_meta)

metacerc_dt<-clmm(metacerc_rel_abund_factor ~ data_type + host_size + (1|locality) + (1|year),
                  link="probit",data=data_meta,Hess=TRUE)
summary(metacerc_dt)

library(ggeffects)
pr_meta<-ggpredict(metacerc_dt,c("data_type [cont, hist]"))

pr_meta$response.level
pr_meta$x
pr_meta$predicted

meta_plot<-ggplot(pr_meta,aes(x,predicted,color=response.level))+
  scale_color_manual(values=c("#3C000A","#8D0017","#CF1938","#E47F90","#E47F90","#E47F90"),labels=c("0","+","++","+++"),
                     name="")+
  geom_point(size=4)+
  geom_errorbar(data=pr_meta,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  geom_line(aes(group=response.level))+
  xlab("")+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=10),axis.title.y=element_text(size=14),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=rev(levels(pr_meta_Ho$x)),labels=c("historical","contemporary"))
#theme(legend.position="none")
meta_plot

# Now deal with the problem of year in the metacerc dataset

str(data_meta)

metacerc_year<-clmm(metacerc_rel_abund_factor ~ year + host_size + (1|locality),
                    link="probit",data=data_meta,Hess=TRUE)
summary(metacerc_year)

library(ggeffects)
library(sjstats)
pr_meta_year<-ggpredict(metacerc_year,c("year"),ci.lvl=0.95)

pr_meta_year$x<-as.numeric(as.character(pr_meta_year$x))

meta_plot_year<-ggplot(pr_meta_year,aes(x,predicted,color=response.level))+
  scale_color_manual(values=c("#3C000A","#8D0017","#CF1938","#E47F90","#E47F90","#E47F90"),labels=c("0","+","++","+++"),
                     name="")+
  geom_point(size=4)+
  geom_errorbar(data=pr_meta_year,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  #geom_line(aes(group=response.level))+
  xlab("")+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=10),axis.title.y=element_text(size=14),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_continuous(name="year",limits=c(1960,2028))
#theme(legend.position="none")
meta_plot_year

# Now make one big plot that has the data_type and year predictions.

pr_meta$x<-c(2019,2019,2019,2019,1969,1969,1969,1969)
pr_meta_year$x

meta_plot_final<-ggplot(pr_meta,aes(x,predicted,color=response.level))+
  scale_color_manual(values=c("#3C000A","#8D0017","#CF1938","#E47F90","#E47F90","#E47F90"),labels=c("0","+","++","+++"),
                     name="")+
  geom_point(size=5,alpha=0.5)+
  geom_errorbar(data=pr_meta,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=1,size=1)+
  geom_line(aes(group=response.level))+
  geom_point(data=pr_meta_year,mapping=aes(x=x))+
  geom_errorbar(data=pr_meta_year,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  xlab("")+
  ylab("predicted likelihood of\nmetacercariae abundance category")+
  theme_minimal()+
  theme(legend.text=element_text(size=14),plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=13),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_continuous(name="",limits=c(1960,2028))
#theme(legend.position="none")
meta_plot_final


# We need to figure out whether the magnitude of long-term change is greater than the magnitude of short-term change.
# For each site, find the difference between all year combinations and ask: are adjacent years less different than distant years?

# Now you can ask whether contrasts between years differ

library(tidyr)

#Make a dataset with no H.n

final_data_meta<-final_data %>%
  filter(crab_spp == "H.o")

final_data_meta<-final_data_meta %>%
  filter(year != "1970")

final_data_wide<-spread(final_data_meta,year,metacerc_rel_abund)
names(final_data_wide)<-c("unique_ID","count_portunion","cystacanths","crab_spp","locality","unique_crab_locality","y1969","y2018","y2019","y2020")

final_data_condensed<-aggregate(cbind(final_data_wide$y1969,final_data_wide$y1970,final_data_wide$y2018,final_data_wide$y2019,final_data_wide$y2020),
                                by=list(final_data_wide$unique_crab_locality),FUN=median,na.rm=TRUE)
names(final_data_condensed)<-c("unique_crab_locality","y1969","y2018","y2019","y2020")

#crab_spp
aggregate(cbind((final_data$crab_spp)),by=list(final_data$unique_crab_locality),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_crab_spp<-aggregate(cbind((final_data$crab_spp)),by=list(final_data$unique_crab_locality),function(x){
  levels(final_data$crab_spp)[max(as.numeric(x))]})

#locality
aggregate(cbind((final_data$locality)),by=list(final_data$unique_crab_locality),function(x){
  ux<-unique(x)
  ux[which.max(tabulate(match(x,ux)))]})
agg_locality<-aggregate(cbind((final_data$locality)),by=list(final_data$unique_crab_locality),function(x){
  levels(final_data$locality)[max(as.numeric(x))]})

final_final_data<-cbind.data.frame(final_data_condensed,agg_crab_spp$V1,agg_locality$V1)
names(final_final_data)<-c("unique_crab_locality","y1969","y2018","y2019","y2020","crab_spp","crab_locality")


# Find the differences for each site/crab combination

final_final_data$y1969.2018<-abs((final_final_data$y2018)-(final_final_data$y1969))
final_final_data$y1969.2019<-abs((final_final_data$y2019)-(final_final_data$y1969))
final_final_data$y1969.2020<-abs((final_final_data$y2020)-(final_final_data$y1969))
final_final_data$y2018.2019<-abs((final_final_data$y2019)-(final_final_data$y2018))
final_final_data$y2018.2020<-abs((final_final_data$y2020)-(final_final_data$y2018))
final_final_data$y2019.2020<-abs((final_final_data$y2020)-(final_final_data$y2019))

diffs<-gather(final_final_data,contrast,difference,y1969.2018:y2019.2020,factor_key=TRUE)

diffs$time_diff

for(i in 1:length(diffs$unique_crab_locality)){
  if(diffs$contrast[i]=="y2018.2019" | diffs$contrast[i]=="y2019.2020" | diffs$contrast[i]=="y2018.2020")
    diffs$time_diff[i]<-"interannual"
  else
    diffs$time_diff[i]<-"longterm"
}

hist(diffs$difference)

model<-lmer(difference~time_diff+(1|crab_locality),data=diffs)
summary(model)
AIC(model)
# AIC = 6.840449

# extract coefficients
coefs <- data.frame(coef(summary(model)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs


# Make ESM Figure S3

meta_timediff_plot_2018_2019<-ggplot(final_final_data,aes(y2018,y2019,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y2018),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab("metacercariae prevalence in 2018")+
  ylab("metacercariae prevalence in 2019")+
  theme_minimal()+
  xlim(0,1)+
  ylim(0,1)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
meta_timediff_plot_2018_2019

meta_timediff_plot_2019_2020<-ggplot(final_final_data,aes(y2019,y2020,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y2019),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab("metacercariae prevalence in 2019")+
  ylab("metacercariae prevalence in 2020")+
  theme_minimal()+
  xlim(0,1)+
  ylim(0,1)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
meta_timediff_plot_2019_2020

meta_timediff_plot_1969_2018<-ggplot(final_final_data,aes(y1969,y2018,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y1969),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab("metacercariae prevalence in 1969")+
  ylab("metacercariae prevalence in 2018")+
  theme_minimal()+
  xlim(0,1)+
  ylim(0,1)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
meta_timediff_plot_1969_2018

meta_timediff_plot_1969_2019<-ggplot(final_final_data,aes(y1969,y2019,color=crab_spp),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(data=final_final_data,mapping=aes(x=y1969),size=5,alpha=0.5)+
  #geom_errorbar(data=subset_data,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.15,size=1)+
  geom_abline(intercept=0,slope=1,linetype="solid")+
  xlab("metacercariae prevalence in 1969")+
  ylab("metacercariae prevalence in 2019")+
  theme_minimal()+
  xlim(0,1)+
  ylim(0,1)+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=14),axis.title.y=element_text(size=12),axis.text.x=element_text(size=14),axis.title.x=element_text(size=14),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  #scale_x_continuous(name="year",limits=c(2017.5,2019.5))+
  theme(legend.position="none")
meta_timediff_plot_1969_2019


library(cowplot)

ggdraw(plot=NULL,xlim=c(0,20),ylim=c(0,20))+
  draw_plot(meta_timediff_plot_2018_2019,x=0,y=10,width=9.5,height=9)+
  draw_plot(meta_timediff_plot_2019_2020,x=10,y=10,width=9.5,height=9)+
  draw_plot(meta_timediff_plot_1969_2018,x=0,y=0,width=9.5,height=9)+
  draw_plot(meta_timediff_plot_1969_2019,x=10,y=0,width=9.5,height=9)+
  draw_label("(a)",x=1,y=19.75,size=20)+
  draw_label("(b)",x=11,y=19.75,size=20)+
  draw_label("(c)",x=1,y=9.75,size=20)+
  draw_label("(d)",x=11,y=9.75,size=20)




# Make Figure 2

ggdraw(plot=NULL,xlim=c(0,20),ylim=c(0,30))+
  draw_plot(portunion_plot,x=0,y=19,width=19.5,height=11)+
  draw_plot(cyst_plot,x=0,y=9.5,width=19.5,height=9)+
  draw_plot(meta_plot_final,x=0,y=0,width=19.5,height=9)+
  draw_label("(a)",x=1.6,y=26.9,size=20)+
  draw_label("(b)",x=1.6,y=17.55,size=20)+
  draw_label("(c)",x=1.6,y=8.05,size=20)



