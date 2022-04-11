rm(list=ls())

# Import merged wildlife and traffic data
newdata = read.csv("newdata.csv", header = TRUE)


# Descriptive statistics
sum(newdata$Total) # total = 27 003 (ungulates and carnivores)
sum(newdata$Total[newdata$Underpass.type == "Underpass"]) # total = 21 181
sum(newdata$Total[newdata$Underpass.type == "Jumpout"]) # total = 5822
sum(newdata$Total[newdata$Species.grouped == "Carnivores"]) # total = 1209
sum(newdata$Total[newdata$Species.grouped == "Ungulates"]) # total = 22 014



ungulates = newdata[newdata$Species.grouped == "Ungulates",]


install.packages("viridis")
library(viridis)
library(ggplot2)

pre.pie = ggplot(newdata[newdata$Species.grouped != "Human",]) +
  geom_bar(aes(x = "", y = Total,
               fill = as.factor(Underpass.type)),
           stat = "summary", fun.y = "sum", position = "fill",
           width = 1) +
  guides(fill = guide_legend(title = NULL)) +
  theme_classic() +
  scale_color_viridis_c() +
  ylab("Total crossings") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = 24)) +
  guides(fill = guide_legend(title = NULL))

pie.type = pre.pie +coord_polar("y", start = 0)
pie.type




# Total traffic per year
millions = c("2", "4", "6", "8")
ggplot(trafficdata,
       aes(x = Year, y = Two.way)) +
  geom_bar(fill = "salmon",
           stat = "summary", fun.y = "sum") +
  theme_classic() +
  ylab("Total two-way traffic\n") +
  xlab("\nYear") +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16)) +
  scale_y_continuous(labels = function(n){format (n, scientific = FALSE)})



# Total animal crossings per year with guilds
# carnivores = grey40
# ungulates = grey70
ggplot(newdata[newdata$Species.grouped != "Human",]) +
  geom_bar(aes(x = Year, y = Total,
               fill = as.factor(Species.grouped)),
           stat = "summary", fun.y = "sum") +
  guides(fill = guide_legend(title = NULL)) +
  theme_classic() +
  scale_fill_manual(values = c("grey40", "grey70")) +
  ylab("Total crossing events\n") +
  xlab("\nYear") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))



# Total animal crossings per year by location
newdata$Species.grouped[newdata$Species.grouped == "Ungulate"] = "Ungulates"
newdata$Species.grouped[newdata$Species.grouped == "Predator"] = "Carnivores"

ggplot(newdata[newdata$Species.grouped != "Human",]) +
  geom_bar(aes(x = Year, y = Total,
               fill = as.factor(Location2)),
           stat = "summary", fun.y = "sum") +
  guides(fill = guide_legend(title = NULL)) +
  theme_classic() +
  scale_fill_manual(values = c("skyblue3", "dodgerblue4", "palegreen2")) +
  ylab("Total crossings")


# Total animal crossings per hour per guild
#newdata$Species.grouped[newdata$Species.grouped == "Ungulate"] = "Ungulates"
#newdata$Species.grouped[newdata$Species.grouped == "Predator"] = "Carnivores"

ggplot(newdata[newdata$Species.grouped != "Human",]) +
  geom_bar(aes(x = HourEnding, y = Total,
               fill = as.factor(Species.grouped)),
           stat = "summary", fun.y = "sum") +
  guides(fill = guide_legend(title = NULL)) +
  theme_classic() +
  scale_fill_manual(values = c("grey40", "grey70")) +
  ylab("Total crossing events\n") +
  xlab("\nHour") +
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(6, 12, 18, 24))





######################################################################
######################################################################

# Total ungulate crossings per hour
ggplot(newdata[newdata$Species.grouped == "Ungulates",]) +
  geom_bar(aes(x = HourEnding, y = Total),
           stat = "summary", fun.y = "sum",
           fill = "grey70") +
  stat_summary(aes(x = HourEnding,
               y = Total),
               geom = "errorbar", fun.data = mean_se) +
  theme_classic() +
  ylab("Total ungulate crossings\n") +
  xlab("\nHour") +
  theme(text = element_text(size = 20)) +
  scale_x_discrete(breaks = c(6,12,24,18))

  

# Total carnivores per hour
ggplot(newdata[newdata$Species.grouped == "Carnivores",]) +
  geom_bar(aes(x = HourEnding, y = Total),
             stat = "summary", fun.y = "sum",
             fill = "grey40") +
  stat_summary(aes(x = HourEnding,
                     y = Total),
                 geom = "errorbar", fun.data = mean_se) +
    theme_classic() +
    ylab("Total carnivore crossings\n") +
    xlab("\nHour") +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(6,12,18,24))


# Total carnivores per month
ggplot(newdata[newdata$Species.grouped == "Carnivores",]) +
  geom_bar(aes(x = Month, y = Total),
           stat = "summary", fun.y = "sum",
           fill = "grey40") +
  stat_summary(aes(x = Month,
                   y = Total),
               geom = "errorbar", fun.data = mean_se) +
  theme(text = element_text(size = 12)) +
  theme_classic() +
  ylab("Total carnivore crossings\n") +
  xlab("\nMonth") +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(2,4,6,8, 10, 12))


# Total carnivores per year
ggplot(newdata[newdata$Species.grouped == "Carnivores",]) +
  geom_bar(aes(x = Year, y = Total),
           stat = "summary", fun.y = "sum",
           fill = "grey40") +
  stat_summary(aes(x = Year,
                   y = Total),
               geom = "errorbar", fun.data = mean_se) +
  theme(text = element_text(size = 12)) +
  theme_classic() +
  ylab("Total carnivore crossings\n") +
  xlab("\nYear") +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))

# Total ungulates per hour
ggplot(newdata[newdata$Species.grouped == "Ungulates",]) +
  geom_bar(aes(x = HourEnding, y = Total),
           stat = "summary", fun.y = "sum",
           fill = "grey70") +
  stat_summary(aes(x = HourEnding,
                   y = Total),
               geom = "errorbar", fun.data = mean_se) +
  theme_classic() +
  ylab("Total ungulate crossings\n") +
  xlab("\nHour") +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(6,12,18,24))


# Total ungulates per month
ggplot(newdata[newdata$Species.grouped == "Ungulates",]) +
  geom_bar(aes(x = Month, y = Total),
           stat = "summary", fun.y = "sum",
           fill = "grey70") +
  stat_summary(aes(x = Month,
                   y = Total),
               geom = "errorbar", fun.data = mean_se) +
  theme(text = element_text(size = 12)) +
  theme_classic() +
  ylab("Total ungulate crossings\n") +
  xlab("\nMonth") +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(2,4,6,8, 10, 12))


# Total ungulates per year
ggplot(newdata[newdata$Species.grouped == "Ungulates",]) +
  geom_bar(aes(x = Year, y = Total),
           stat = "summary", fun.y = "sum",
           fill = "grey70") +
  stat_summary(aes(x = Year,
                   y = Total),
               geom = "errorbar", fun.data = mean_se) +
  theme(text = element_text(size = 12)) +
  theme_classic() +
  ylab("Total ungulate crossings\n") +
  xlab("\nYear") +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018))



# Total traffic per hour
ggplot(trafficdata) +
  geom_bar(aes(x = HourEnding, y = Two.way),
           stat = "summary", fun.y = "sum",
           fill = "salmon") +
  stat_summary(aes(x = HourEnding,
                   y = Two.way),
               geom = "errorbar", fun.data = mean_se) +
  theme_classic() +
  ylab("Total vehicle traffic\n") +
  xlab("\nHour") +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks = c(6,12,18,24)) +
  scale_y_continuous(labels = function(n){format (n, scientific = FALSE)})




# Total traffic per hour - all
ggplot(newdata) +
  geom_bar(aes(x = HourEnding, y = Two.way),
           stat = "summary", fun.y = "sum",
           fill = "salmon") +
  stat_summary(aes(x = HourEnding,
                   y = Total),
               geom = "errorbar", fun.data = mean_se) +
  theme_classic() +
  ylab("Total two-way traffic (all)\n") +
  xlab("\nHour") +
  theme(text = element_text(size = 20)) +
  scale_x_discrete(breaks = c(6,12,24,18))






# collision data
collisions = read.csv("Collision_data_1991_2014.csv")
str(collisions)

ggplot(collisions,
       aes(Year, y = Collisioncount)) +
  geom_point(stat = "summary", fun.y = "sum") +
  geom_line(stat = "summary", fun.y = "sum") +
  theme_classic() +
  ylab("Annual vehicle-wildlife collisions \n") +
  xlab("\nYear") +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks = c(1992, 1996, 2000,
                              2004, 2008, 
                              2012)) +
  geom_vline(xintercept = 1999, linetype = "dotted", colour = "red") +
  geom_vline(xintercept = 2004, linetype = "dotted", colour = "red")





################ hourly crossings ###############

# Total hourly ungulate crossings

require(ggplot2)
ggplot(hourly[hourly$Species.grouped == "Ungulates",]) +
  geom_bar(aes(x = HourEnding, y = Crossings),
           stat = "summary", fun.y = "sum",
           fill = "purple") +
  stat_summary(aes(x = HourEnding,
                   y = Crossings),
               geom = "errorbar", fun.data = mean_se) +
  theme_classic() +
  ylab("Total hourly ungulate crossings\n") +
  xlab("\nHour") +
  theme(text = element_text(size = 20)) +
  scale_x_discrete(breaks = c(6,12,24,18))



# Total hourly carnivore crossings

ggplot(hourly[hourly$Species.grouped == "Carnivores",]) +
  geom_bar(aes(x = HourEnding, y = Crossings),
           stat = "summary", fun.y = "sum",
           fill = "grey40") +
  stat_summary(aes(x = HourEnding,
                   y = Crossings),
               geom = "errorbar", fun.data = mean_se) +
  theme_classic() +
  ylab("Total hourly carnivore crossings\n") +
  xlab("\nHour") +
  theme(text = element_text(size = 12)) +
  scale_x_discrete(breaks = c(6,12,24,18))



### Monthly crossings ###

require(ggplot2)

ggplot(hourly_totals[hourly_totals$Species.grouped == "Ungulates",]) +
  geom_bar(aes(x = Month, y = Hourly.inds),
           stat = "summary", fun.y = "sum",
           fill = "purple",
           na.rm = TRUE) +
  theme_classic() +
  ylab("Total ungulate crossings\n") +
  xlab("\nMonth") +
  theme(text = element_text(size = 20)) +
  scale_x_discrete(labels = month.abb)



ggplot(hourly_totals[hourly_totals$Species.grouped == "Carnivores",]) +
  geom_bar(aes(x = Month, y = Hourly.inds),
           stat = "summary", fun.y = "sum",
           fill = "orange",
           na.rm = TRUE) +
  theme_classic() +
  ylab("Total carnivore crossings\n") +
  xlab("\nMonth") +
  theme(text = element_text(size = 20)) +
  scale_x_discrete(labels = month.abb)


##### Model plots ######
# Figure 6A: Factors predicting hourly ungulate count
rm(list=ls())

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/adjusted P-values/")

Figure6a = read.csv("ungulate hourly summary table_adjusted P.csv", header=T)

#Figure6a = Figure6a[-c(1, 12),] # omit "Intercept" & "units" from $variable

Figure6a$variable = as.factor(Figure6a$variable)

levels(Figure6a$variable) = c("Structure age",
                              "Hour squared",
                              "Hour",
                              "Human presence",
                              "Location (Stewart Creek)",
                              "Location (Wind Valley)",
                              "Sampling effort",
                              "Two-way traffic",
                              "Crossing structure type",
                              "Year")


require(ggplot2)

ggplot(Figure6a, aes(x = variable, y = post.mean)) + 
  geom_point() +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) +
  geom_errorbar(aes(ymin=l.95..CI,
                    ymax=u.95..CI), width=0.2)+
  theme(axis.text=element_text(size=11)) +
  xlab("") +
  geom_hline(yintercept=0, size=0.15, linetype="dashed") +
  ylab("Hourly ungulate model parameter estimates") +
  coord_flip()

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/model figures/")




# Figure 6B: Factors predicting hourly CARNIVORE count

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/adjusted P-values/")

Figure6b = read.csv("carnivore hourly summary table_adjusted P.csv", header=T)

Figure6b = Figure6b[-c(1,12),] # omit "Intercept" & "units" from $variable

Figure6b$variable = as.factor(Figure6a$variable)

levels(Figure6b$variable) = c("Structure age",
                              "Hour squared",
                              "Hour",
                              "Human presence",
                              "Location (Stewart Creek)",
                              "Location (Wind Valley)",
                              "Sampling effort",
                              "Two-way traffic",
                              "Crossing structure type")


require(ggplot2)

ggplot(Figure6b, aes(x = variable, y = post.mean)) + 
  geom_point() +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) +
  geom_errorbar(aes(ymin=l.95..CI,
                    ymax=u.95..CI), width=0.2)+
  theme(axis.text=element_text(size=11)) +
  xlab("") +
  geom_hline(yintercept=0, size=0.15, linetype="dashed") +
  ylab("Hourly carnivore model parameter estimates") +
  coord_flip()

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/model figures/")




# Figure 5: Factors predicting annual ungulate count

fig5 <- read.csv("~/Dropbox/Kananaskis crossing structures/adjusted P-values/ungulate annual model table_adjusted P.csv")
#Figure6c = Figure6c[-c(1,9),] # omit "Intercept" & "units" from $variable

fig5$variable = as.factor(fig5$variable)

levels(fig5$variable) = c("Structure age",
                              "Human presence",
                              "Location (Stewart Creek)",
                              "Location (Wind Valley)",
                              "Sampling effort",
                              "Two-way traffic",
                              "Crossing structure type",
                              "Year")


require(ggplot2)

ggplot(fig5, aes(x = variable, y = post.mean)) + 
  geom_point() +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) +
  geom_errorbar(aes(ymin=l.95..CI,
                    ymax=u.95..CI), width=0.2)+
  theme(axis.text=element_text(size=11)) +
  xlab("") +
  geom_hline(yintercept=0, size=0.15, linetype="dashed") +
  ylab("Annual ungulate model parameter estimates") +
  coord_flip()

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/model figures/")




# Figure: Factors predicting annual CARNIVORE count
fig3 <- read.csv("~/Dropbox/Kananaskis crossing structures/adjusted P-values/carnivore annual model table_adjusted P.csv", header=TRUE)

#Figure6d = Figure6d[-c(1,9),] # omit "Intercept" & "units" from $variable

fig3$variable = as.factor(fig3$variable)

levels(fig3$variable) = c("Structure age",
                              "Human presence",
                              "Location (Stewart Creek)",
                              "Location (Wind Valley)",
                              "Sampling effort",
                              "Two-way traffic",
                              "Crossing structure type",
                              "Year")


require(ggplot2)

ggplot(fig3, aes(x = variable, y = post.mean)) + 
  geom_point() +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) +
  geom_errorbar(aes(ymin=l.95..CI,
                    ymax=u.95..CI), width=0.2)+
  theme(axis.text=element_text(size=11)) +
  xlab("") +
  geom_hline(yintercept=0, size=0.15, linetype="dashed") +
  ylab("Annual carnivore model parameter estimates") +
  coord_flip()

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/model figures/")



