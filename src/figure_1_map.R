---
title: "Figure 1 / map of sampling sites for Quinn et al. 2021"
author: "Chelsea Wood"
date: "updated 7 May 2020"
---

library(maps)
library(mapdata)
library(maptools) #for shapefiles
library(scales) #for transparency

sites<-read.csv("data/site_coords.csv")
names(sites)<-c("site_name","lat","long")

#Need a higher res map

library(rgdal)
library(maps)
library(mapdata)
library(maptools) #for shapefiles
library(scales) #for transparency

# Include only sites that we were able to analyze:
# H.o: TN, NB, SP, BM, YB, AL, UC, FB
# H.n: TN, NB, SP, BM, YB, AP, YH, FH

sites_incl<-sites[which(sites$site_name=="TN" | sites$site_name=="NB" | sites$site_name=="SP" | sites$site_name=="BM" | 
                          sites$site_name=="YB" | sites$site_name=="AL" | sites$site_name=="UC" | sites$site_name=="FB" | 
                          sites$site_name=="AP" | sites$site_name=="YH" | sites$site_name=="FH"),]

sites_excl<-sites[which(sites$site_name=="SS" | sites$site_name=="DB" | sites$site_name=="FP" | sites$site_name=="SB" | 
                          sites$site_name=="OS" | sites$site_name=="MB" | sites$site_name=="PI" | sites$site_name=="PO" | 
                          sites$site_name=="WC" | sites$site_name=="WR" | sites$site_name=="WT"),]

library(ggmap)
library(ggsn)

bounds<-c(left=-131 , bottom=42 , right=-121 , top=50)
get_stamenmap(bounds, zoom=7, maptype = "terrain-background") %>% ggmap()+
  geom_point(data=sites_incl, aes(x=sites_incl$long,y=jitter(sites_incl$lat,factor=20)),size=3,shape=21,color="black",fill="white")+
  xlab("longitude (°W)")+
  ylab("latitude (°N)")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=16))


# Make some inset plots that you can splice in

bounds<-c(left=-130 , bottom=20 , right=-70 , top=54)
get_stamenmap(bounds, zoom=3, maptype = "toner-lite") %>% ggmap()+
  geom_rect(xmin=-128 , ymin=42 , xmax=-121 , ymax=50, alpha=0.1)+
  xlab("")+
  ylab("")+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=16))





