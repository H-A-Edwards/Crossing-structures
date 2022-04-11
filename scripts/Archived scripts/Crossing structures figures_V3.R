#Script to produce figure 2-5 for the ms. Shows the total number of counts by 
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

#---------------------------Figure 2-collision figure
# collision data
collisions = read.csv("Collision_data_1991_2014.csv")
str(collisions)

ggplot(data=collisions, aes(x=Year, y=Collision.count.per.year))+
  geom_line()+
  geom_point()+
  ylab("Annual wildlife-vehcile collision count")+
  xlab("")+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  theme(text=element_text(size=10))+
  theme(axis.text=element_text(size=10))+
  geom_vline(xintercept=1999, linetype="dotted")+
  geom_vline(xintercept=2004, linetype="dashed")


#----------------------------Figure 3-seasonal ungulates

smallungulates.season.under$Season <- ordered(smallungulates.season.under$Season,
                levels = c("Spring", "Summer", "Autumn", "Winter"))

smallungseasonund<-ggplot(data=smallungulates.season.under, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total small ungulate count")+
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
  ylab("Total large ungulate count")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "b)")

smallungulates.season.jump$Season <- ordered(smallungulates.season.jump$Season,
                                             levels = c("Spring", "Summer", "Autumn", "Winter"))


smallungseasonjump<-ggplot(data=smallungulates.season.jump, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total small ungulate count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "c)")

bigungulates.season.jump$Season <- ordered(bigungulates.season.jump$Season,
                                           levels = c("Spring", "Summer", "Autumn", "Winter"))


bigungseasonjump<-ggplot(data=bigungulates.season.jump, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total large ungulate count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "d)")


Fig3<-ggarrange(smallungseasonund, bigungseasonund, ncol = 2, 
                nrow = 1, common.legend=TRUE, legend="right")
annotate_figure(Fig3)


#------------------------------Fig 4-annual ungulates

smallungannund<-ggplot(data=smallungulates.annual.under, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total small ungulate count")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "a)")

bigungannund<-ggplot(data=bigungulates.annual.under, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total large ungulate count")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "b)")


smallungannjump<-ggplot(data=smallungulates.annual.jump, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total small ungulate count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "c)")

bigungannjump<-ggplot(data=bigungulates.annual.jump, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total large ungulate count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "d)")

Fig4<-ggarrange(smallungannund,bigungannund,
                ncol = 2, nrow = 1, common.legend=TRUE, legend="right")
annotate_figure(Fig4)

#----------------------------Figure 5-seasonal carnivore

smallcarnivores.season.under$Season <- ordered(smallcarnivores.season.under$Season,
                                            levels = c("Spring", "Summer", "Autumn", "Winter"))

smallcarseasonund<-ggplot(data=smallcarnivores.season.under, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total small carnivore count")+
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
  ylab("Total large carnivore count")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "b)")


smallcarnivores.season.jump$Season <- ordered(smallcarnivores.season.jump$Season,
                                              levels = c("Spring", "Summer", "Autumn", "Winter"))

smallcarseasonjump<-ggplot(data=smallcarnivores.season.jump, aes(x=Season, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total small carnivore count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "c)")


Fig5<-ggarrange(smallcarseasonund, bigcarseasonund,
                ncol = 2, nrow = 1, common.legend=TRUE, legend="right")
annotate_figure(Fig5)


#-------------------------------Fig 6-Annual carnivore

smallcarannund<-ggplot(data=smallcarnivores.annual.under, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total small carnivore count")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "a)")


bigcarannund<-ggplot(data=bigcarnivores.annual.under, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total large carnivore count")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "b)")

smallcarannjump<-ggplot(data=smallcarnivores.annual.jump, aes(x=Year, y=Total))+
  geom_bar(stat="identity")+
  xlab("")+
  ylab("Total small carnivore count at jumpout")+
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
  ylab("Total large carnivore count at jumpout")+
  scale_fill_grey(start=0.8, end=0.2)+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black', size=0.4))+
  theme(axis.text=element_text(size=11))+
  theme(legend.title=element_blank(), legend.position="right")+
  labs(title = "d)")

Fig6<-ggarrange(smallcarannund, bigcarannund,
                ncol = 2, nrow = 1, common.legend=TRUE, legend="right")
annotate_figure(Fig6)

#-------------------------Fig S1-prehunting pop counts

####Plot the trend over time with species
####data set
Pop<-read.csv("Historical Pop Estimates Request_2001-2016.csv",header=T)

Pop1<-aggregate(Estimate~Year +Taxon.ID, data=Pop, FUN='mean')
Pop1$Ungulatetype[Pop1$Taxon.ID == "White-tailed Deer"] = "Small ungulate"
Pop1$Ungulatetype[Pop1$Taxon.ID == "Moose"] = "Large ungulate"
Pop1$Ungulatetype[Pop1$Taxon.ID == "Mule Deer"] = "Large ungulate"
Pop1$Ungulatetype[Pop1$Taxon.ID == "Elk"] = "Large ungulate"
Pop1<-rename(Pop1, c("Ungulatetype"="Ungulate.size"))

plot2<-ggplot(Pop1)+
  geom_line(aes(x=Year,y=Estimate,linetype=Ungulate.size, colour=Taxon.ID))+
  ylab("Annual species count")+
  xlab("Year")+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  theme(text=element_text(size=10))+
  theme(axis.text=element_text(size=10))+
  scale_color_manual(values=c('blue', 'red', 'green', 'purple', 'pink'))+
  guides(colour = guide_legend(override.aes = list(linetype = 1, shape = 3)))
plot2

#---------------------------Fig S2-vehicle density fig
# Annual average Daily traffic for traffic counter 5km E1 & 1A Canmore WJ
trafficdata<- read.csv("5kmE1ACanmore_1983-2021.csv")
#Cut off data at 2018 (when the data is collected in the study)
trafficdata2<-trafficdata[which(trafficdata$Year<2018), ]

ggplot(trafficdata2, aes(x=Year, y=X5.0.KM.E.1...1A.CANMORE.WJ))+
  geom_point()+
  geom_line(data=trafficdata2[!is.na(trafficdata2$X5.0.KM.E.1...1A.CANMORE.WJ),])+
  ylab("Average annual daily traffic")+
  xlab("Year")+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  theme(text=element_text(size=10))+
  theme(axis.text=element_text(size=10))
  
