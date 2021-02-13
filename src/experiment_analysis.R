---
title: "Statistical analyses and plots related to the experiment comparing parasite burdens of fixed and frozen crabs
for Quinn et al. 2021"
author: "Chelsea Wood"
date: "updated 19 Feb 2020"

# See Electronic Supplementary Material (ESM) Text S1 for methods and results of this experiment #

---

# Load required packages
library(googledrive)
library(skimr)
library(lubridate)
library(tidyverse)
library(clm)
library(ordinal)


# Verify that tidyverse is up to date
tidyverse_update()


# Read in data
data<-read_csv("data/experiment_data.csv")


# Count everything up

port_data<-data %>%
  filter(count_portunion >= 0)
length(port_data$count_portunion)

# How many crabs in the full dataset?
n_full <- data %>%
  group_by(treatment, crab_spp) %>%
  summarise(n = n())

# Number of H.o
59+50

# Number of H.n
64+51

########### PORTUNION ########### 

port_model<-count_portunion ~ treatment*crab_spp + carapace_width

# Fit a normal distribution
model_norm_port<-glm(formula = port_model, data=data)
summary(model_norm_port)	
AIC(model_norm_port)
#AIC = 183.2185
#SELECT THIS MODEL (lowest AIC)

model_pois_port<-glm(formula = port_model, data=data, family="poisson")
summary(model_pois_port)	
AIC(model_pois_port)
#AIC = 205.1443

model_nbinom_port<-glm.nb(formula = port_model, data=data)
summary(model_nbinom_port)	
AIC(model_nbinom_port)
#AIC = 207.1491

expt_port<-ggpredict(model_norm_port,c("treatment", "crab_spp"))


########### ACANTHOCEPHALANS ########### 

data_cyst <- data %>%
  filter(crab_spp == "H.o")

cyst_data<-data_cyst %>%
  filter(cystacanths >= 0)
length(cyst_data$cystacanths)

cyst_model<-cystacanths ~ treatment + carapace_width

# Fit a normal distribution
model_norm_cyst<-glm(formula = cyst_model, data=data_cyst)
summary(model_norm_cyst)	
AIC(model_norm_cyst)
#AIC = -198.8903
#SELECT THIS MODEL (lowest AIC)

model_pois_cyst<-glm(formula = cyst_model, data=data_cyst, family="poisson")
summary(model_pois_cyst)	
AIC(model_pois_cyst)
#AIC = 10.19722

model_nbinom_cyst<-glm.nb(formula = cyst_model, data=data_cyst)
summary(model_nbinom_cyst)	
AIC(model_nbinom_cyst)
#AIC = 12.19724

expt_cyst<-ggpredict(model_norm_cyst,c("treatment"))


########### METACERCARIAE ########### 

data$metacerc_rel_abund_factor<-as.factor(data$metacerc_rel_abund)
levels(data$metacerc_rel_abund_factor)

data_meta <- data %>%
  filter(crab_spp == "H.o")

meta_data<-data_meta %>%
  filter(metacerc_rel_abund >= 0)
length(meta_data$metacerc_rel_abund)

meta_model<-clm(metacerc_rel_abund_factor ~ treatment + carapace_width,
                link="probit",data=data_meta,Hess=TRUE)
summary(meta_model)	

expt_meta<-ggpredict(meta_model,c("treatment"))
expt_meta$x


# NOW PLOT EVERYTHING

portunion_plot<-ggplot(expt_port,aes(x,predicted,color=group),grouping=crab_spp,color=crab_spp)+
  scale_color_manual(values=c("#0571b0","#ca0020"))+
  geom_point(size=4)+
  geom_errorbar(data=expt_port,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  geom_line(aes(group=group))+
  xlab("")+
  ylab(expression(paste("predicted abundance of ",italic("P. conformis"))))+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=10),axis.title.y=element_text(size=9),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=rev(levels(expt_port$x)),labels=c("fresh","frozen"))+
  theme(legend.position="none")
portunion_plot

cyst_plot<-ggplot(expt_cyst,aes(x,predicted,color=group))+
  scale_color_manual(values=c("#ca0020"))+
  geom_point(size=4)+
  geom_errorbar(data=expt_cyst,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  geom_line(aes(group=group))+
  xlab("")+
  ylab("predicted abundance of acanthocephalans")+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=10),axis.title.y=element_text(size=9),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=rev(levels(expt_cyst$x)),labels=c("fresh","frozen"))+
  theme(legend.position="none")
cyst_plot

meta_plot<-ggplot(expt_meta,aes(x,predicted,color=response.level))+
  scale_color_manual(values=c("#3C000A","#8D0017","#CF1938","#E47F90","#E47F90","#E47F90"),labels=c("0","+","++","+++"),
                     name="")+
  geom_point(size=4)+
  geom_errorbar(data=expt_meta,mapping=aes(x=x,ymin=conf.low,ymax=conf.high),width=0.03)+
  geom_line(aes(group=response.level))+
  xlab("")+
  ylab("predicted likelihood of metacercariae abundance category")+
  theme_minimal()+
  theme(plot.title=element_text(size=14,hjust=0.5,face="plain"),axis.text.y=element_text(size=10),axis.title.y=element_text(size=9),axis.text.x=element_text(size=14),axis.title.x=element_text(size=10),panel.background=element_rect(fill="white",color="black"),panel.grid.major=element_line(color="grey95"),panel.grid.minor=element_line(color=NA),plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_discrete(limits=rev(levels(expt_meta$x)),labels=c("fresh","frozen"))+
  theme(legend.position="top")
meta_plot


# Make ESM Figure S1

ggdraw(plot=NULL,xlim=c(0,20),ylim=c(0,30))+
  draw_plot(portunion_plot,x=0,y=20.75,width=19.5,height=9)+
  draw_plot(cyst_plot,x=0,y=11.75,width=19.5,height=9)+
  draw_plot(meta_plot,x=0,y=0,width=19.5,height=12)+
  draw_label("(a)",x=2,y=29,size=20)+
  draw_label("(b)",x=2,y=20,size=20)+
  draw_label("(c)",x=2,y=9.25,size=20)
