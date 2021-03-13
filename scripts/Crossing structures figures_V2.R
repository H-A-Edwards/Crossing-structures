#Script to run produce figure 2-5 for the ms. Shows the total number of counts by 
#time of day, month and year for big/small ungualtes and carnivores.

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")

#----------------Load libraries----------------------------
library(ggplot2)
library(ggpubr)
library(plyr)

#----------------Load datasets----------------------------

smallungulates.day <- read.csv("Smallungulates hourly daynight.csv")
smallungulates.season <- read.csv("Smallungulates seasonal.csv")
smallungulates.annual <- read.csv("Smallungulates annual.csv")

#Separate out by crossing structure type
smallungulates.day.under<-subset(smallungulates.day, smallungulates.day$Underpass.type != "Jumpout")
smallungulates.day.jump<-subset(smallungulates.day, smallungulates.day$Underpass.type != "Underpass")
smallungulates.season.under<-subset(smallungulates.season, smallungulates.season$Underpass.type != "Jumpout")
smallungulates.season.jump<-subset(smallungulates.season, smallungulates.season$Underpass.type != "Underpass")
smallungulates.annual.under<-subset(smallungulates.annual, smallungulates.annual$Underpass.type != "Jumpout")
smallungulates.annual.jump<-subset(smallungulates.annual, smallungulates.annual$Underpass.type != "Underpass")

bigungulates.day <- read.csv("Bigungulates hourly daynight.csv")
bigungulates.season <- read.csv("Bigungulates seasonal.csv")
bigungulates.annual <- read.csv("Bigungulates annual.csv")

bigungulates.day.under<-subset(bigungulates.day, bigungulates.day$Underpass.type != "Jumpout")
bigungulates.day.jump<-subset(bigungulates.day, bigungulates.day$Underpass.type != "Underpass")
bigungulates.season.under<-subset(bigungulates.season, bigungulates.season$Underpass.type != "Jumpout")
bigungulates.season.jump<-subset(bigungulates.season, bigungulates.season$Underpass.type != "Underpass")
bigungulates.annual.under<-subset(bigungulates.annual, bigungulates.annual$Underpass.type != "Jumpout")
bigungulates.annual.jump<-subset(bigungulates.annual, bigungulates.annual$Underpass.type != "Underpass")

smallcarnivores.day <- read.csv("Smallcarnivores hourly daynight.csv")
smallcarnivores.season <- read.csv("Smallcarnivores seasonal.csv")
smallcarnivores.annual <- read.csv("Smallcarnivores annual.csv")

#Separate out by crossing structure type
smallcarnivores.day.under<-subset(smallcarnivores.day, smallcarnivores.day$Underpass.type != "Jumpout")
smallcarnivores.day.jump<-subset(smallcarnivores.day, smallcarnivores.day$Underpass.type != "Underpass")
smallcarnivores.season.under<-subset(smallcarnivores.season, smallcarnivores.season$Underpass.type != "Jumpout")
smallcarnivores.season.jump<-subset(smallcarnivores.season, smallcarnivores.season$Underpass.type != "Underpass")
smallcarnivores.annual.under<-subset(smallcarnivores.annual, smallcarnivores.annual$Underpass.type != "Jumpout")
smallcarnivores.annual.jump<-subset(smallcarnivores.annual, smallcarnivores.annual$Underpass.type != "Underpass")

bigcarnivores.day <- read.csv("Bigcarnivores hourly daynight.csv")
bigcarnivores.season <- read.csv("Bigcarnivores seasonal.csv")
bigcarnivores.annual <- read.csv("Bigcarnivores annual.csv")

#Separate out by crossing structure type
#Did not run for big carnivore jumpout sample size small use circular plots as evidence
bigcarnivores.day.under<-subset(bigcarnivores.day, bigcarnivores.day$Underpass.type != "Jumpout")
#bigcarnivores.day.jump<-subset(bigcarnivores.day, bigcarnivores.day$Underpass.type != "Underpass")
bigcarnivores.season.under<-subset(bigcarnivores.season, bigcarnivores.season$Underpass.type != "Jumpout")
#bigcarnivores.season.jump<-subset(bigcarnivores.season, bigcarnivores.season$Underpass.type != "Underpass")
bigcarnivores.annual.under<-subset(bigcarnivores.annual, bigcarnivores.annual$Underpass.type != "Jumpout")
bigcarnivores.annual.jump<-subset(bigcarnivores.annual, bigcarnivores.annual$Underpass.type != "Underpass")


#----------------------------Figure 2-small ungulate counts
smallungulates.day.under[smallungulates.day.under$daynight == "crep"] = "crepuscular"

smallungulates.day.under$daynight<-revalue(smallungulates.day.under$daynight, c("crep"="crepuscular"))

smallungdayund<-ggplot(data=smallungulates.day.under, aes(x=daynight, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "a)")

smallungulates.season.under$Season <- ordered(smallungulates.season.under$Season,
                levels = c("Spring", "Summer", "Autumn", "Winter"))

smallungseasonund<-ggplot(data=smallungulates.season.under, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "b)")

smallungannund<-ggplot(data=smallungulates.annual.under, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "c)")

smallungulates.day.jump$daynight<-revalue(smallungulates.day.jump$daynight, c("crep"="crepuscular"))

smallungdayjump<-ggplot(data=smallungulates.day.jump, aes(x=daynight, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "d)")

smallungulates.season.jump$Season <- ordered(smallungulates.season.jump$Season,
                                              levels = c("Spring", "Summer", "Autumn", "Winter"))


smallungseasonjump<-ggplot(data=smallungulates.season.jump, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "e)")

smallungannjump<-ggplot(data=smallungulates.annual.jump, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "f)")

Fig2<-ggarrange(smallungdayund,smallungdayjump, smallungseasonund, 
                smallungseasonjump,smallungannund, smallungannjump,ncol = 2, 
                nrow = 3, common.legend=TRUE, legend="right")
annotate_figure(Fig2, top="Small ungulate count")


#----------------------------Figure 3-big ungulate counts
bigungulates.day.under$daynight<-revalue(bigungulates.day.under$daynight, c("crep"="crepuscular"))

bigungdayund<-ggplot(data=bigungulates.day.under, aes(x=daynight, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "a)")

bigungulates.season.under$Season <- ordered(bigungulates.season.under$Season,
                                              levels = c("Spring", "Summer", "Autumn", "Winter"))

bigungseasonund<-ggplot(data=bigungulates.season.under, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "b)")

bigungannund<-ggplot(data=bigungulates.annual.under, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "c)")

bigungulates.day.jump$daynight<-revalue(bigungulates.day.jump$daynight, c("crep"="crepuscular"))

bigungdayjump<-ggplot(data=bigungulates.day.jump, aes(x=daynight, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "d)")

bigungulates.season.jump$Season <- ordered(bigungulates.season.jump$Season,
                                            levels = c("Spring", "Summer", "Autumn", "Winter"))


bigungseasonjump<-ggplot(data=bigungulates.season.jump, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "e)")

bigungannjump<-ggplot(data=bigungulates.annual.jump, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "f)")

Fig3<-ggarrange(bigungdayund,bigungdayjump, bigungseasonund,
                bigungseasonjump,bigungannund,bigungannjump,
                ncol = 2, nrow = 3, common.legend=TRUE, legend="right")
annotate_figure(Fig3, top="Large ungulate count")

#----------------------------Figure 4-small carnivore counts
smallcarnivores.day.under$daynight<-revalue(smallcarnivores.day.under$daynight, c("crep"="crepuscular"))

smallcardayund<-ggplot(data=smallcarnivores.day.under, aes(x=daynight, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "a)")

smallcarnivores.season.under$Season <- ordered(smallcarnivores.season.under$Season,
                                            levels = c("Spring", "Summer", "Autumn", "Winter"))

smallcarseasonund<-ggplot(data=smallcarnivores.season.under, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "b)")

smallcarannund<-ggplot(data=smallcarnivores.annual.under, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "c)")

smallcarnivores.day.jump$daynight<-revalue(smallcarnivores.day.jump$daynight, c("crep"="crepuscular"))

smallcardayjump<-ggplot(data=smallcarnivores.day.jump, aes(x=daynight, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "d)")

smallcarnivores.season.jump$Season <- ordered(smallcarnivores.season.jump$Season,
                                               levels = c("Spring", "Summer", "Autumn", "Winter"))

smallcarseasonjump<-ggplot(data=smallcarnivores.season.jump, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "e)")

smallcarannjump<-ggplot(data=smallcarnivores.annual.jump, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "f)")

Fig4<-ggarrange(smallcardayund,smallcardayjump, smallcarseasonund,
                smallcarseasonjump,smallcarannund,smallcarannjump,
                ncol = 2, nrow = 3, common.legend=TRUE, legend="right")
annotate_figure(Fig4, top="Small carnivore count")

#----------------------------Figure 5-big carnivore counts
bigcarnivores.day.under$daynight<-revalue(bigcarnivores.day.under$daynight, c("crep"="crepuscular"))

bigcardayund<-ggplot(data=bigcarnivores.day.under, aes(x=daynight, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "a)")

bigcarnivores.season.under$Season <- ordered(bigcarnivores.season.under$Season,
                                               levels = c("Spring", "Summer", "Autumn", "Winter"))


bigcarseasonund<-ggplot(data=bigcarnivores.season.under, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "b)")

bigcarannund<-ggplot(data=bigcarnivores.annual.under, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at underpass")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "c)")


bigcarannjump<-ggplot(data=bigcarnivores.annual.jump, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "d)")

Fig5<-ggarrange(bigcardayund, bigcarseasonund,
                bigcarannund,bigcarannjump,
                ncol = 2, nrow = 3, common.legend=TRUE, legend="right")
annotate_figure(Fig5, top="Large carnivore count")

