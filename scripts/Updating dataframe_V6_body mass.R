#-----------------------------------------------------

# Script to to create the individual datasets for small/large carnivores and ungulates by day/season/year
# We use the data from traffic_data (from Alberta Transportation) to create (crep/day/night)/seasonal/annual
# averages of the two-way hourly traffic data and merge with the
# CSdata (from Alberta Parks)
# We group the data by carnivores, ungulates and humans
# We create a structure location, structure type, structure age (centred) and year built column
# We create a human count data from the total column to create (crep/day/night)/seasonal/annual
# averages of the human count
# The number of times a camera is serviced is calculated per year and on average per year
# in the annual effort and average effort column respectively
# We create crep/day/night counts of ungulates and carnivores from the total column, binned by peak activity
# We create seasonal counts of ungulates and carnivores from the total column, binned by peak activity
# We create annual counts of ungulates and carnivores from the total column

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")
#setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/")

#----------------Load libraries.-------------
library(readr)
library(dplyr)
library(pracma)
library(dplyr)
library(readxl)
library(forcats)
library(stringr)

# Camera data
csdata = read_excel("CSData.xlsx")
csdata$Date = as.Date(csdata$DateImage)
csdata$HourEnding = format(strptime(csdata$TimeImage,"%H:%M:%S") + 3600,'%H')
csdata$HourEnding = as.numeric(csdata$HourEnding)
csdata$HourEnding[csdata$HourEnding == "0"] = "24"
csdata$HourEnding = as.numeric(csdata$HourEnding)
csdata$HourEnding = as.factor(csdata$HourEnding)

# check levels of HourEnding
levels(as.factor(csdata$HourEnding))

# Traffic data
trafficdata = read.csv("traffic_data.csv")

# Change date format in trafficdata
trafficdata$Date = NA
trafficdata$Date = paste(trafficdata$Year,trafficdata$Month,trafficdata$Day, sep = "-")
trafficdata$Date = as.Date(trafficdata$Date)
trafficdata$HourEnding = as.factor(trafficdata$HourEnding)

newdata <- left_join(csdata, trafficdata, by= c("Date", "HourEnding"))
#Remove images with no traffic data e.g. images later than Nov 2018
newdata <- newdata[!is.na(newdata$Two.way),]

#-------------------------Create crepescular/day/night column--------------------------
#Make day/month col in newdata and sunrise/sunset datasets and merge
sundata <- read.csv("Canmore_Sunrise_Sunset.csv")
Day2<-str_pad(newdata$Day, 2, pad = "0")#Add zero to single digits
Month2<-str_pad(newdata$Month, 2, pad = "0")
newdata$Day.Month<- paste(Day2, Month2, sep="-")

Day3<-str_pad(sundata$Day, 2, pad = "0")
Month3<-str_pad(sundata$Month, 2, pad = "0")
sundata$Day.Month<- paste(Day3, Month3, sep="-")

newdata <- left_join(newdata, sundata, by= c("Day.Month"))
newdata<-subset(newdata, select = -c(Day.y, Month.y))#Drop unwanted cols from sundata
newdata<-rename(newdata, "Month"="Month.x", "Day"="Day.x")#Drop unwanted cols from sundata

newdata$daynight <- ifelse(newdata$TimeImage > newdata$Sunrise.hr.before & 
                             newdata$TimeImage < newdata$Sunrise.hr.after, 'crep',
                           ifelse(newdata$TimeImage > newdata$Sunset.hr.before & 
                                    newdata$TimeImage < newdata$Sunset.hr.after, 'crep',
                                  ifelse(newdata$TimeImage > newdata$Sunrise.hr.after &
                                           newdata$TimeImage < newdata$Sunset.hr.before, 'day','night')))

#-------------------------Create season col----------------------------------------------

newdata$Season = NA

newdata$Month = as.factor(newdata$Month)

newdata$Season[newdata$Month == "1"] = "Winter"
newdata$Season[newdata$Month == "2"] = "Winter"
newdata$Season[newdata$Month == "3"] = "Spring"
newdata$Season[newdata$Month == "4"] = "Spring"
newdata$Season[newdata$Month == "5"] = "Spring"
newdata$Season[newdata$Month == "6"] = "Summer"
newdata$Season[newdata$Month == "7"] = "Summer"
newdata$Season[newdata$Month == "8"] = "Summer"
newdata$Season[newdata$Month == "9"] = "Autumn"
newdata$Season[newdata$Month == "10"] = "Autumn"
newdata$Season[newdata$Month == "11"] = "Autumn"
newdata$Season[newdata$Month == "12"] = "Winter"

#-------------------------Average number of crep/day/night/ two way traffic-------------
Day4<-str_pad(trafficdata$Day, 2, pad = "0")
Month4<-str_pad(trafficdata$Month, 2, pad = "0")
trafficdata$Day.Month<- paste(Day4, Month4, sep="-")

trafficdata$Time<-str_pad(trafficdata$Time, 2, pad = "0")
trafficdata$Time<-paste(trafficdata$Time, trafficdata$Minutes, trafficdata$Seconds)
trafficdata$Time<-gsub(" ","", trafficdata$Time)

hourly.traffic <- left_join(trafficdata, sundata, by= c("Day.Month"))

hourly.traffic$daynight <- ifelse(hourly.traffic$Time > hourly.traffic$Sunrise.hr.before & 
                                    hourly.traffic$Time < hourly.traffic$Sunrise.hr.after, 'crep',
                                  ifelse(hourly.traffic$Time > hourly.traffic$Sunset.hr.before & 
                                           hourly.traffic$Time < hourly.traffic$Sunset.hr.after, 'crep',
                                         ifelse(hourly.traffic$Time > hourly.traffic$Sunrise.hr.after &
                                                  hourly.traffic$Time < hourly.traffic$Sunset.hr.before, 'day','night')))

hourly.traffic$daynight<-as.factor(hourly.traffic$daynight)
#Omit NA data for two way traffic on 15th March (only one datapoint)
hourly.traffic<-hourly.traffic[!is.na(hourly.traffic$Two.way), ]

daynight.traffic = (
        hourly.traffic   %>%
  group_by(daynight) %>%
 summarise(daynight.traffic = mean(Two.way))
)

newdata = left_join(newdata, daynight.traffic, by = c("daynight"))

#-------------------------Average number of seasonal two way traffic----------------------
#Make seasons in traffic data

trafficdata$Season = NA

trafficdata$Month = as.factor(trafficdata$Month)

trafficdata$Season[trafficdata$Month == "1"] = "Winter"
trafficdata$Season[trafficdata$Month == "2"] = "Winter"
trafficdata$Season[trafficdata$Month == "3"] = "Spring"
trafficdata$Season[trafficdata$Month == "4"] = "Spring"
trafficdata$Season[trafficdata$Month == "5"] = "Spring"
trafficdata$Season[trafficdata$Month == "6"] = "Summer"
trafficdata$Season[trafficdata$Month == "7"] = "Summer"
trafficdata$Season[trafficdata$Month == "8"] = "Summer"
trafficdata$Season[trafficdata$Month == "9"] = "Autumn"
trafficdata$Season[trafficdata$Month == "10"] = "Autumn"
trafficdata$Season[trafficdata$Month == "11"] = "Autumn"
trafficdata$Season[trafficdata$Month == "12"] = "Winter"

#Omit NA data for two way traffic on 15th March (only one datapoint)
trafficdata<-trafficdata[!is.na(trafficdata$Two.way), ]

seasonal.traffic = (
  trafficdata %>%
    group_by(Season) %>%
    summarise(seasonal.traffic = mean(Two.way))
)

newdata = left_join(newdata, seasonal.traffic, by = c("Season"))

#-------------------------Average number of annual two way traffic----------------------
annual.traffic = (
  trafficdata %>%
    group_by(Year) %>%
    summarise(annual.traffic = mean(Two.way))
)

newdata = left_join(newdata, annual.traffic, by = c("Year"))
                                                              
#Create col with species grouped by body mass
newdata$Species.body.mass = NA

newdata$Species = as.factor(newdata$Species)

newdata$Species.body.mass[newdata$Species == "Black bear"] = "Big carnivore"
newdata$Species.body.mass[newdata$Species == "Cougar"] = "Big carnivore"
newdata$Species.body.mass[newdata$Species == "Unknown Bear"] = "Big carnivore"
newdata$Species.body.mass[newdata$Species == "Wolf"] = "Big carnivore"
newdata$Species.body.mass[newdata$Species == "Bobcat"] = "Small carnivore"
newdata$Species.body.mass[newdata$Species == "Grizzly Bear"] = "Big carnivore"
newdata$Species.body.mass[newdata$Species == "Wolverine"] = "Big carnivore"
newdata$Species.body.mass[newdata$Species == "Lynx"] = "Small carnivore"
newdata$Species.body.mass[newdata$Species == "Coyote"] = "Small carnivore"
newdata$Species.body.mass[newdata$Species == "Marten"] = "Small carnivore"
newdata$Species.body.mass[newdata$Species == "Striped Skunk"] = "Small carnivore"
newdata$Species.body.mass[newdata$Species == "Red Fox"] = "Small carnivore"

newdata$Species.body.mass[newdata$Species == "Bighorn Sheep"] = "Small ungulate"
newdata$Species.body.mass[newdata$Species == "Elk"] = "Big ungulate"
newdata$Species.body.mass[newdata$Species == "Moose"] = "Big ungulate"
newdata$Species.body.mass[newdata$Species == "Mule Deer"] = "Big ungulate"
newdata$Species.body.mass[newdata$Species == "White-tailed Deer"] = "Small ungulate"
newdata$Species.body.mass[newdata$Species == "Unknown Deer"] = "Small ungulate"
newdata$Species.body.mass[newdata$Species == "Unknown Ungulate"] = "Small ungulate"


newdata$Species.body.mass[newdata$Species == "Hiker"] = "Human"
newdata$Species.body.mass[newdata$Species == "Angler"] = "Human"
newdata$Species.body.mass[newdata$Species == "Hunter"] = "Human"
newdata$Species.body.mass[newdata$Species == "Quad"] = "Human"
newdata$Species.body.mass[newdata$Species == "Runner"] = "Human"
newdata$Species.body.mass[newdata$Species == "Snowshoer"] = "Human"
newdata$Species.body.mass[newdata$Species == "Worker (Not Parks)"] = "Human"
newdata$Species.body.mass[newdata$Species == "Vehicle"] = "Human"
newdata$Species.body.mass[newdata$Species == "Biker"] = "Human"
newdata$Species.body.mass[newdata$Species == "Motorbike"] = "Human"
newdata$Species.body.mass[newdata$Species == "Worker (not Parks)"] = "Human"

#remove rows with Species.body.mass = NA
newdata<-subset(newdata, !is.na(newdata$Species.body.mass))
newdata$Species.body.mass = as.factor(newdata$Species.body.mass)

#remove unnecessary columns
newdata$ImageID = NULL
newdata$AdultFemale = NULL
newdata$AdultMale = NULL
newdata$AdultUnknown = NULL
newdata$Subadult = NULL
newdata$ReactionToCamera = NULL
newdata$CommentsImages = NULL
newdata$ImageName = NULL
newdata$ImageSequence = NULL
newdata$ImageNumber = NULL
newdata$AdultUnknown = NULL
newdata$YLY = NULL
newdata$YOY = NULL
newdata$BearID = NULL
newdata$`Sow&Cubs` = NULL
newdata$ColourMarkings = NULL
newdata = newdata[ which(newdata$Location != "Kananaskis River Underpass"), ]
newdata$EventEnd = NULL
newdata$EventStart = NULL
newdata$Duration = NULL

#new column for location data
newdata$Location = as.factor(newdata$Location)

newdata$Location2 = NA
newdata$Location2[newdata$Location == "Deadmans Northeast Jumpout"] = "Dead Man's"
newdata$Location2[newdata$Location == "Deadmans Northwest Jumpout"] = "Dead Man's"
newdata$Location2[newdata$Location == "Deadmans South Jumpout"] = "Dead Man's"
newdata$Location2[newdata$Location == "rundle Forebay"] = "Rundle Forebay"
newdata$Location2[newdata$Location == "Rundle Forebay"] = "Rundle Forebay"
newdata$Location2[newdata$Location == "stewart Creek Underpass"] = "Stewart Creek"
newdata$Location2[newdata$Location == "Stewart Creek Underpass"] = "Stewart Creek"
newdata$Location2[newdata$Location == "stewart North Jumpout"] = "Stewart Creek"
newdata$Location2[newdata$Location == "Stewart South Jumpout"] = "Stewart Creek"
newdata$Location2[newdata$Location == "Wind Valley Underpass"] = "Wind Valley"

newdata = newdata[ which(newdata$Location2 != "Rundle Forebay"), ]
newdata$Location2 = as.factor(newdata$Location2)

# Structure types
newdata$Underpass.type = NA
newdata$Underpass.type[newdata$Location == "Deadmans Northeast Jumpout"] = "Jumpout"
newdata$Underpass.type[newdata$Location == "Deadmans Northwest Jumpout"] = "Jumpout"
newdata$Underpass.type[newdata$Location == "Deadmans South Jumpout"] = "Jumpout"
newdata$Underpass.type[newdata$Location == "stewart Creek Underpass"] = "Underpass"
newdata$Underpass.type[newdata$Location == "Stewart Creek Underpass"] = "Underpass"
newdata$Underpass.type[newdata$Location == "stewart North Jumpout"] = "Jumpout"
newdata$Underpass.type[newdata$Location == "Stewart South Jumpout"] = "Jumpout"
newdata$Underpass.type[newdata$Location == "Wind Valley Underpass"] = "Underpass"

newdata$Underpass.type = as.factor(newdata$Underpass.type)

# Year structures built
newdata$Date.structure.built = NA
newdata$Date.structure.built[newdata$Location2 == "Stewart Creek"] = "1999"
newdata$Date.structure.built[newdata$Location2 == "Wind Valley"] = "2004"
newdata$Date.structure.built[newdata$Location2 == "Dead Man's"] = "2004"

# Structure age
newdata$Structure.age = NA
newdata$Date.structure.built = as.numeric(newdata$Date.structure.built)
newdata$Structure.age = newdata$Year - newdata$Date.structure.built

#-------------------------Average number of daynight humans----------------------
#Create new human dataset
Humanuse <- newdata[newdata$Species.body.mass=="Human",]

daynight.human = (
  Humanuse %>%
    group_by(daynight, Location2, Underpass.type) %>%
    summarise(daynight.human = mean(Total))
)

newdata = left_join(newdata, daynight.human, by = c("daynight", "Location2", "Underpass.type"))

#-------------------------Average number of seasonal humans----------------------
seasonal.human = (
  Humanuse %>%
    group_by(Season, Location2, Underpass.type) %>%
    summarise(seasonal.human = mean(Total))
)

newdata = left_join(newdata, seasonal.human, by = c("Season","Location2", "Underpass.type"))

#-------------------------Average number of annual humans----------------------
annual.human = (
  Humanuse %>%
    group_by(Year, Location2, Underpass.type) %>%
    summarise(annual.human = mean(Total))
)

newdata = left_join(newdata, annual.human, by = c("Year", "Location2", "Underpass.type"))
#Give years with no data a zero
newdata$annual.human[is.na(newdata$annual.human)] <- 0

# Centering age
newdata$Agecentred = NA
newdata$Agecentred = scale(newdata[,48], center = TRUE, scale = FALSE)

####Calculate sampling effort column
CSCameraEffort = read.csv("camera effort.csv")

CSCameraEffort$Year = as.factor(CSCameraEffort$Year)
CSCameraEffort$Location = as.factor(CSCameraEffort$Location)
CSCameraEffort = CSCameraEffort[ which(CSCameraEffort$Location != "Rundle Forebay"), ]
CSCameraEffort[ CSCameraEffort$Location == "Deadman",] = CSCameraEffort[ CSCameraEffort$Location != "Rundle Forebay",]

#----------Yearly sampling effort-------------------------
annual.effort = (
  CSCameraEffort %>%
    group_by(Year, Location) %>%
    summarise(no_rows_year = length(Year))
)

annual.effort$annual.effort = annual.effort$no_rows_year
annual.effort$Year = as.factor(annual.effort$Year)
newdata$Year = as.factor(newdata$Year)

newdata_effort = left_join(newdata, annual.effort, by = c("Year", "Location"))
#Drop unnecessary rows col
newdata_effort<-within(newdata_effort, rm("no_rows_year"))

#----------Average sampling effort across years for hour/month analysis------------------
  annual.loca.effort = (
  CSCameraEffort %>%
    group_by(Year,Location) %>%
    summarise(annual.loca.effort = length(Location))
)

  average.effort = (
    annual.loca.effort %>%
      group_by(Location) %>%
      summarise(average.effort = mean(annual.loca.effort))
  )
  

newdata_effort = left_join(newdata_effort, average.effort, by = c("Location"))

#Give locations not sampled a zero
newdata_effort$annual.effort[is.na(newdata_effort$annual.effort)] <- 0

write.csv(newdata_effort, "newdata_bodymass.csv", row.names=FALSE)

# Read in new dataframe
newdata = read.csv("newdata_bodymass.csv")

# Create subsets with humans included
Bigungulates_humans = subset(newdata, newdata$Species.body.mass == "Big ungulate")
Smallungulates_humans = subset(newdata, newdata$Species.body.mass != "Small carnivore")
Bigcarnivores_humans = subset(newdata, newdata$Species.body.mass != "Big carnivore")
Smallcarnivores_humans = subset(newdata, newdata$Species.body.mass != "Small carnivore")

# Since humans are accounted for in Human.total.x, omit them from Total
Bigungulates_humans$Total[Bigungulates_humans$Species.body.mass == "Human"] = "0"
Smallungulates_humans$Total[Smallungulates_humans$Species.body.mass == "Human"] = "0"
Bigcarnivores_humans$Total[Bigcarnivores_humans$Species.body.mass == "Human"] = "0"
Smallcarnivores_humans$Total[Smallcarnivores_humans$Species.body.mass == "Human"] = "0"

Bigungulates_humans$Total = as.numeric(Bigungulates_humans$Total)
Smallungulates_humans$Total = as.numeric(Smallungulates_humans$Total)
Bigcarnivores_humans$Total = as.numeric(Bigcarnivores_humans$Total)
Smallcarnivores_humans$Total = as.numeric(Smallcarnivores_humans$Total)

# Hourly big ungulates-use average sampling effort
Bigungulates.hourly <- aggregate(Total ~
                                daynight + Location2 +
                                Underpass.type+average.effort + 
                                daynight.traffic+daynight.human,
                             Bigungulates_humans, sum)

#Bigungulates.hourly$HourEnding = as.numeric(Bigungulates.hourly$HourEnding)
#Bigungulates.hourly$Hour.sq = (Bigungulates.hourly$HourEnding)^2
write.csv(Bigungulates.hourly, "Bigungulates hourly daynight.csv")

# Hourly small ungulates-use average sampling effort
Smallungulates.hourly <- aggregate(Total ~
                                daynight + Location2 +
                                Underpass.type+average.effort + 
                                daynight.traffic+daynight.human,
                                Smallungulates_humans, sum)

#Smallungulates.hourly$HourEnding = as.numeric(Smallungulates.hourly$HourEnding)
#Smallungulates.hourly$Hour.sq = (Smallungulates.hourly$HourEnding)^2
write.csv(Smallungulates.hourly, "Smallungulates hourly daynight.csv")

# Hourly big carnivores-use average sampling effort
Bigcarnivores.hourly = aggregate(Total ~
                                daynight + Location2 +
                                Underpass.type+average.effort + 
                                daynight.traffic+daynight.human,
                                Bigcarnivores_humans, sum)

#Bigcarnivores.hourly$HourEnding = as.numeric(Bigcarnivores.hourly$HourEnding)
#Bigcarnivores.hourly$Hour.sq = (Bigcarnivores.hourly$HourEnding)^2
write.csv(Bigcarnivores.hourly, "Bigcarnivores hourly daynight.csv")

# Hourly small carnivores-use average sampling effort
Smallcarnivores.hourly = aggregate(Total ~
                                daynight + Location2 +
                                Underpass.type+average.effort + 
                               daynight.traffic+daynight.human,
                                Smallcarnivores_humans, sum)

#Smallcarnivores.hourly$HourEnding = as.numeric(Smallcarnivores.hourly$HourEnding)
#Smallcarnivores.hourly$Hour.sq = (Smallcarnivores.hourly$HourEnding)^2
write.csv(Smallcarnivores.hourly, "Smallcarnivores hourly daynight.csv")


# Monthly big ungulates-use average sampling effort
Bigungulates.monthly = aggregate(Total ~
                                Season + Location2 +
                                Underpass.type+average.effort + 
                                seasonal.traffic+seasonal.human,
                                Bigungulates_humans, sum)

#Bigungulates.monthly$Month = as.numeric(Bigungulates.monthly$Month)
#Bigungulates.monthly$Month.sq = (Bigungulates.monthly$Month)^2
write.csv(Bigungulates.monthly, "Bigungulates seasonal.csv")

# Monthly ungulates-use average sampling effort
Smallungulates.monthly = aggregate(Total ~
                                Season + Location2 +
                                Underpass.type+average.effort + 
                                  seasonal.traffic+seasonal.human,
                                Smallungulates_humans, sum)

#Smallungulates.monthly$Month = as.numeric(Smallungulates.monthly$Month)
#Smallungulates.monthly$Month.sq = (Smallungulates.monthly$Month)^2
write.csv(Smallungulates.monthly, "Smallungulates seasonal.csv")

# Monthly big carnivores-use average sampling effort
Bigcarnivores.monthly = aggregate(Total ~
                                 Season + Location2 +
                                 Underpass.type+average.effort + 
                                seasonal.traffic+seasonal.human,
                                 Bigcarnivores_humans, sum)

#Bigcarnivores.monthly$Month = as.numeric(Bigcarnivores.monthly$Month)
#Bigcarnivores.monthly$Month.sq = (Bigcarnivores.monthly$Month)^2
write.csv(Bigcarnivores.monthly, "Bigcarnivores seasonal.csv")

# Monthly small carnivores-use average sampling effort
Smallcarnivores.monthly = aggregate(Total ~
                                 Season + Location2 +
                                 Underpass.type+average.effort + 
                                 seasonal.traffic+seasonal.human,
                                 Smallcarnivores_humans, sum)

#Smallcarnivores.monthly$Month = as.numeric(Smallcarnivores.monthly$Month)
#Smallcarnivores.monthly$Month.sq = (Smallcarnivores.monthly$Month)^2
write.csv(Smallcarnivores.monthly, "Smallcarnivores seasonal.csv")

# Annual big ungulates-use yearly sampling effort
Bigungulates.annual = aggregate(Total ~
                                Location + Year + Location2 + Underpass.type + annual.effort +
                               Agecentred + annual.traffic+ annual.human,
                               Bigungulates_humans, sum)
write.csv(Bigungulates.annual, "Bigungulates annual.csv")

# Annual small ungulates-use yearly sampling effort
Smallungulates.annual = aggregate(Total ~
                               Location + Year + Location2 + Underpass.type + annual.effort +
                               Agecentred + annual.traffic+ annual.human,
                               Smallungulates_humans, sum)
write.csv(Smallungulates.annual, "Smallungulates annual.csv")

# Annual big carnivores-use yearly sampling effort
Bigcarnivores.annual = aggregate(Total ~
                                 Location + Year + Location2 + Underpass.type + annual.effort 
                               +Agecentred + annual.traffic+ annual.human,
                               Bigcarnivores_humans, sum)
write.csv(Bigcarnivores.annual, "Bigcarnivores annual.csv")

# Annual small carnivores-use yearly sampling effort
Smallcarnivores.annual = aggregate(Total ~
                                Location + Year + Location2 + Underpass.type + annual.effort 
                              +Agecentred + annual.traffic+ annual.human,
                              Smallcarnivores_humans, sum)
write.csv(Smallcarnivores.annual, "Smallcarnivores annual.csv")

