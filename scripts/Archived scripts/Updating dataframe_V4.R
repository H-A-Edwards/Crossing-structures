#-----------------------------------------------------

# Script to to create the dataset newdata.
# We use the data from traffic_data (from Alberta Transportation) to create hourly/monthly/annual
# averages of the two-way hourly traffic data and merge with the
# CSdata (from Alberta Parks)
# We group the data by carnivores, ungulates and humans
# We create a structure location, structure type, structure age (centred) and year built column
# We create a human count data from the total column to create hourly/monthly/annual
# averages of the human count
# The number of times a camera is serviced is calculated per year and on average per year
# in the annual effort and average effort column respectively
# We create hourly counts of ungulates and carnivores from the total column, binned by peak activity
# We create monthly counts of ungulates and carnivores from the total column, binned by peak activity
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

#-------------------------Average number of hourly two way traffic----------------------
#Get the average from the traffic data and add to the new dataset, regardless of location

hourly.traffic = (
  newdata %>%
    group_by(HourEnding) %>%
    summarise(hourly.traffic = mean(Two.way))
)

newdata = left_join(newdata, hourly.traffic, by = c("HourEnding"))

#-------------------------Average number of monthly two way traffic----------------------
monthly.traffic = (
  trafficdata %>%
    group_by(Month) %>%
    summarise(monthly.traffic = mean(Two.way))
)

newdata = left_join(newdata, monthly.traffic, by = c("Month"))

#-------------------------Average number of annual two way traffic----------------------
annual.traffic = (
  newdata %>%
    group_by(Year) %>%
    summarise(annual.traffic = mean(Two.way))
)

newdata = left_join(newdata, annual.traffic, by = c("Year"))
                                                              
# Create new column for guild
newdata$Species.grouped = NA

newdata$Species = as.factor(newdata$Species)
newdata$Species.grouped[newdata$Species == "Black Bear"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Cougar"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Unknown Bear"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Wolf"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Bobcat"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Grizzly Bear"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Wolverine"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Lynx"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Coyote"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Marten"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Striped Skunk"] = "Carnivores"
newdata$Species.grouped[newdata$Species == "Red Fox"] = "Carnivores"


newdata$Species.grouped[newdata$Species == "Bighorn Sheep"] = "Ungulates"
newdata$Species.grouped[newdata$Species == "Elk"] = "Ungulates"
newdata$Species.grouped[newdata$Species == "Moose"] = "Ungulates"
newdata$Species.grouped[newdata$Species == "Mule Deer"] = "Ungulates"
newdata$Species.grouped[newdata$Species == "White-tailed Deer"] = "Ungulates"
newdata$Species.grouped[newdata$Species == "Unknown Deer"] = "Ungulates"
newdata$Species.grouped[newdata$Species == "Unknown Ungulate"] = "Ungulates"


newdata$Species.grouped[newdata$Species == "Hiker"] = "Human"
newdata$Species.grouped[newdata$Species == "Angler"] = "Human"
newdata$Species.grouped[newdata$Species == "Hunter"] = "Human"
newdata$Species.grouped[newdata$Species == "Quad"] = "Human"
newdata$Species.grouped[newdata$Species == "Runner"] = "Human"
newdata$Species.grouped[newdata$Species == "Snowshoer"] = "Human"
newdata$Species.grouped[newdata$Species == "Worker (Not Parks)"] = "Human"
newdata$Species.grouped[newdata$Species == "Vehicle"] = "Human"
newdata$Species.grouped[newdata$Species == "Biker"] = "Human"
newdata$Species.grouped[newdata$Species == "Motorbike"] = "Human"
newdata$Species.grouped[newdata$Species == "Worker (not Parks)"] = "Human"

#remove rows with Species.grouped = NA
newdata<-subset(newdata, !is.na(newdata$Species.grouped))
newdata$Species.grouped = as.factor(newdata$Species.grouped)

#group species by body weight
#newdata$Species.body.mass[newdata$Species == "Black bear"] = "Carn"
#newdata$Species.body.mass[newdata$Species == "Cougar"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Unknown Bear"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Wolf"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Bobcat"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Grizzly Bear"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Wolverine"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Lynx"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Coyote"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Marten"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Striped Skunk"] = "Carnivores"
#newdata$Species.body.mass[newdata$Species == "Red Fox"] = "Carnivores"



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

# Human.total.x column, number of annual human counts
#for (i in 1:nrow(newdata)) {
 # if (head(newdata$Species.grouped,i) == "Human") {
  #  newdata$Human.total.x = newdata$Total
   # }else{
    #  newdata$Human.total.x = 0
    #}
  #}
#newdata <- transform(newdata, Human.total.x = ifelse(Species.grouped == "Human", Total, 0))

#newdata<-newdata %>% 
#  rename(
#   annual.human=Human.total.x,)

#-------------------------Average number of hourly humans----------------------
#Create new human dataset
Humanuse <- newdata[newdata$Species.grouped=="Human",]

hourly.human = (
  Humanuse %>%
    group_by(HourEnding, Location2, Underpass.type) %>%
    summarise(hourly.human = mean(Total))
)

newdata = left_join(newdata, hourly.human, by = c("HourEnding", "Location2", "Underpass.type"))
#Give hours with no data a zero
newdata$hourly.human[is.na(newdata$hourly.human)] <- 0

#-------------------------Average number of monthly humans----------------------
monthly.human = (
  Humanuse %>%
    group_by(Month, Location2, Underpass.type) %>%
    summarise(monthly.human = mean(Total))
)

newdata = left_join(newdata, monthly.human, by = c("Month","Location2", "Underpass.type"))
#Give months with no data a zero
newdata$monthly.human[is.na(newdata$monthly.human)] <- 0

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
newdata$Agecentred = scale(newdata[,34], center = TRUE, scale = FALSE)

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

write.csv(newdata_effort, "newdata.csv", row.names=FALSE)

# Read in new dataframe
newdata = read.csv("newdata.csv")

# Create subsets with humans included
ungulates_humans = subset(newdata, newdata$Species.grouped != "Carnivores")
carnivores_humans = subset(newdata, newdata$Species.grouped != "Ungulates")

# Since humans are accounted for in Human.total.x, omit them from Total
ungulates_humans$Total[ungulates_humans$Species.grouped == "Human"] = "0"
carnivores_humans$Total[carnivores_humans$Species.grouped == "Human"] = "0"

ungulates_humans$Total = as.numeric(ungulates_humans$Total)
carnivores_humans$Total = as.numeric(carnivores_humans$Total)

#Create new column with hour binned into 3 hour chunks
ungulates_humans$HourEnding3 <- factor(ungulates_humans$HourEnding)
levels(ungulates_humans$HourEnding3) <- list("1"=c("1", "2", "3"), "2"=c("4", "5", "6"), "3"=c("7", "8", "9"),
                                   "4"=c("10", "11", "12"), "5"=c("13", "14", "15"), "6"=c("16", "17", "18"),
                                   "7"=c("19", "20", "21"), "8"=c("22", "23", "24"))

ungulates_humans$HourEnding<-as.factor(ungulates_humans$HourEnding)
ungulates_humans$HourEnding3<- fct_collapse(ungulates_humans$HourEnding, "1-3"=c("1", "2", "3"), "4-6"=c("4", "5", "6"),
                                            "7-9"=c("7", "8", "9"), "10-12"=c("10", "11", "12"), "13-15"=c("13", "14", "15"),
                                            "16-18"=c("16", "17", "18"),"19-21"=c("19", "20", "21"), "22-24"=c("22", "23", "24"))

# Hourly ungulates-use average sampling effort
ungulates.hourly <- aggregate(Total ~
                                HourEnding + Location2 +
                                Underpass.type+average.effort + 
                                hourly.traffic+hourly.human+Agecentred ,
                             ungulates_humans, sum)

ungulates.hourly$HourEnding = as.numeric(ungulates.hourly$HourEnding)
ungulates.hourly$Hour.sq = (ungulates.hourly$HourEnding)^2
write.csv(ungulates.hourly, "ungulates hourly.csv")

# Hourly ungulates-use average sampling effort and binned hours
#--------------------------------------Bin traffic by same hour chunks------------------------------------------------
#ungulates.hourly2 <- aggregate(Total ~
                                #HourEnding3 + Location2 +
                                #Underpass.type+average.effort + 
                                #hourly.traffic+hourly.human+Agecentred ,
                              #ungulates_humans, sum)
#----------------------------------------------------------------------------------------------------------------------

# Hourly carnivores-use average sampling effort
carnivores.hourly = aggregate(Total ~
                                HourEnding + Location2 +
                                Underpass.type+average.effort + 
                                hourly.traffic+hourly.human+Agecentred,
                              carnivores_humans, sum)
carnivores.hourly$HourEnding = as.numeric(carnivores.hourly$HourEnding)
carnivores.hourly$Hour.sq = (carnivores.hourly$HourEnding)^2
write.csv(carnivores.hourly, "carnivores hourly.csv")

# Monthly ungulates-use average sampling effort
ungulates.monthly = aggregate(Total ~
                                Month + Location2 +
                                Underpass.type+average.effort + 
                                monthly.traffic+monthly.human+Agecentred,
                              ungulates_humans, sum)
ungulates.monthly$Month = as.numeric(ungulates.monthly$Month)
ungulates.monthly$Month.sq = (ungulates.monthly$Month)^2
write.csv(ungulates.monthly, "ungulates monthly.csv")

# Monthly carnivores-use average sampling effort
carnivores.monthly = aggregate(Total ~
                                 Month + Location2 +
                                 Underpass.type+average.effort + 
                                 monthly.traffic+monthly.human+Agecentred,
                               carnivores_humans, sum)
carnivores.monthly$Month = as.numeric(carnivores.monthly$Month)
carnivores.monthly$Month.sq = (carnivores.monthly$Month)^2
write.csv(carnivores.monthly, "carnivores monthly.csv")

# Annual ungulates-use yearly sampling effort
ungulates.annual = aggregate(Total ~
                                Location + Year + Location2 + Underpass.type + annual.effort +
                               Agecentred + annual.traffic+ annual.human,
                              ungulates_humans, sum)
write.csv(ungulates.annual, "ungulates annual.csv")

# Annual carnivores-use yearly sampling effort
carnivores.annual = aggregate(Total ~
                                 Location + Year + Location2 + Underpass.type + annual.effort 
                               +Agecentred + annual.traffic+ annual.human,
                               carnivores_humans, sum)
write.csv(carnivores.annual, "carnivores annual.csv")
