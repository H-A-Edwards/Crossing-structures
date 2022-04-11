setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/")

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
library(readr)
trafficdata = read_delim("traffic data.txt", "\t", escape_double = FALSE, col_types = cols(Day = col_character(),
                                                                                           Month = col_character()), trim_ws = TRUE)

# Change date format in trafficdata
trafficdata$Date = NA
trafficdata$Date = paste(trafficdata$Year,trafficdata$Month,trafficdata$Day, sep = "-")
trafficdata$Date = as.Date(trafficdata$Date)
trafficdata$HourEnding = as.factor(trafficdata$HourEnding)

library(dplyr)
newdata = left_join(csdata, trafficdata, by= c("Date", "HourEnding"))


#Hannah's df for comparison
olddata = read.csv("Underpass_2008_2016.csv", header=T)



# Create new column for guild
newdata$Species.grouped = NA

newdata$Species = as.factor(newdata$Species)
newdata$Species.grouped[newdata$Species == "Black bear"] = "Carnivores"
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



# Human.total.x column
library(pracma)
for (i in 1:nrow(newdata)) {
  if (head(newdata$Species.grouped,i) == "Human") {
    newdata$Human.total.x = newdata$Total
  }else{
    newdata$Human.total.x = 0
  }
}
newdata <- transform(newdata, Human.total.x = ifelse(Species.grouped == "Human", Total, 0))


# Centering age
newdata$Agecentred = NA
newdata$Agecentred = scale(newdata[,31], center = TRUE, scale = FALSE)




####Calculate sampling effort column
CSCameraEffort = read.csv("camera effort.csv")

CSCameraEffort$Year = as.factor(CSCameraEffort$Year)
CSCameraEffort$Location = as.factor(CSCameraEffort$Location)
CSCameraEffort = CSCameraEffort[ which(CSCameraEffort$Location != "Rundle Forebay"), ]
CSCameraEffort[ CSCameraEffort$Location == "Deadman",] = CSCameraEffort[ CSCameraEffort$Location != "Rundle Forebay",]

library(dplyr)
effort = (
  CSCameraEffort %>%
    group_by(Year, Location) %>%
    summarise(no_rows = length(Year))
)

effort$Sampling.effort = effort$no_rows
effort$Year = as.factor(effort$Year)

newdata_effort = left_join(newdata, effort, by = c("Year", "Location"))
newdata_effort$Sampling.effort[is.na(newdata_effort$Sampling.effort)] <- 0


write.csv(newdata_effort, "newdata.csv")




# Omit rows with NA traffic

ungulates_traffic = ungulates_traffic[!is.na(newdata$Two.way),]
carnivores_traffic = carnivores_traffic[!is.na(carnivores_traffic$Two.way),]



# Create column with number of crossings per guild,
## per HourEnding, for each day + month + year
library(dplyr)
hourly.crossings = (
  newdata %>%
    group_by(Species.grouped, HourEnding, Day, Month, Year, Underpass.type,
             Location2, Structure.age, Human.total.x, Agecentred, Sampling.effort,
             Hour.sq,Month.sq,
             Two.way) %>%
    summarise(Crossings = length(Species.grouped))
)


hourly.crossings$X = NULL
hourly.crossings$HourEnding = as.factor(hourly.crossings$HourEnding)
hourly.crossings$Day = as.factor(hourly.crossings$Day)
hourly.crossings$Month = as.factor(hourly.crossings$Month)
hourly.crossings$Year = as.factor(hourly.crossings$Year)
hourly.crossings$HourEnding = as.factor(hourly.crossings$HourEnding)
hourly.crossings$Sampling.effort = as.factor(hourly.crossings$Sampling.effort)
hourly.crossings$Hour.sq = as.integer(hourly.crossings$Hour.sq)


write.csv(hourly.crossings, "hourly crossings.csv")


trafficdata$Year = as.factor(trafficdata$Year)
ungulates$Year = as.factor(ungulates$Year)
carnivores$Year = as.factor(carnivores$Year)
trafficdata$HourEnding = as.factor(trafficdata$HourEnding)
ungulates$HourEnding = as.factor(ungulates$HourEnding)
carnivores$HourEnding = as.factor(carnivores$HourEnding)
trafficdata$Month = as.factor(trafficdata$Month)
ungulates$Month = as.factor(ungulates$Month)
carnivores$Month = as.factor(carnivores$Month)
trafficdata$Day = as.factor(trafficdata$Day)
ungulates$Day = as.factor(ungulates$Day)
carnivores$Day = as.factor(carnivores$Day)


######## merge UNGULATES and TRAFFIC data #######
library(dplyr)

ungulates_traffic = left_join(ungulates, trafficdata, 
                   by = c("Year", "Month", "Day", "HourEnding"),
                   all = TRUE)

ungulates_traffic[is.na(ungulates_traffic)] = 0

ungulates_traffic = ungulates_traffic[apply(ungulates_traffic[7],1,function(z) !any(z==0)),] 


# Create subsets with humans included
ungulates_humans = subset(newdata, newdata$Species.grouped != "Carnivores")
carnivores_humans = subset(newdata, newdata$Species.grouped != "Ungulates")


# Since humans are accounted for in Human.total.x, omit them from Total
ungulates_humans$Total[ungulates_humans$Species.grouped == "Human"] = "0"
carnivores_humans$Total[carnivores_humans$Species.grouped == "Human"] = "0"

ungulates_humans$Total = as.numeric(ungulates_humans$Total)
carnivores_humans$Total = as.numeric(carnivores_humans$Total)



# Hourly ungulates
library(dplyr)

ungulates.hourly2 = aggregate(cbind(Total, Two.way, Human.total.x) ~
                                Year + Location + HourEnding + Location2 +
                               Agecentred + Underpass.type +
                               Sampling.effort,
                              ungulates_humans, sum)
ungulates.hourly2$HourEnding = as.numeric(ungulates.hourly$HourEnding)
ungulates.hourly2$Hour.sq = (ungulates.hourly2$HourEnding)^2
write.csv(ungulates.hourly2, "ungulates hourly2.csv")


# Hourly carnivores
carnivores.hourly = aggregate(cbind(Total, Two.way, Human.total.x) ~
                               Location + HourEnding + Location2 + Agecentred + Underpass.type,
                             carnivores_humans, sum)
carnivores.hourly$HourEnding = as.numeric(carnivores.hourly$HourEnding)
carnivores.hourly$Hour.sq = (carnivores.hourly$HourEnding)^2
write.csv(carnivores.hourly, "carnivores hourly.csv")


#### with year and sampling
carnivores.hourly2 = aggregate(cbind(Total, Two.way, Human.total.x) ~
                                Year + Sampling.effort + Location + HourEnding +
                                 Location2 + Agecentred + Underpass.type,
                              carnivores_humans, sum)
carnivores.hourly2$HourEnding = as.numeric(carnivores.hourly2$HourEnding)
carnivores.hourly2$Hour.sq = (carnivores.hourly2$HourEnding)^2
write.csv(carnivores.hourly2, "carnivores hourly2.csv")





# Monthly ungulates
ungulates.monthly = aggregate(cbind(Total, Two.way, Human.total.x) ~
                               Location + Month + Location2 + Agecentred + Underpass.type,
                             ungulates_humans, sum)
ungulates.monthly$Month = as.numeric(ungulates.monthly$Month)
ungulates.monthly$Month.sq = (ungulates.monthly$Month)^2
write.csv(ungulates.monthly, "ungulates monthly.csv")


### with year and sampling
ungulates.monthly2 = aggregate(cbind(Total, Two.way, Human.total.x) ~
                                Year + Sampling.effort + Location + Month + Location2 + Agecentred + Underpass.type,
                              ungulates_humans, sum)
ungulates.monthly2$Month = as.numeric(ungulates.monthly2$Month)
ungulates.monthly2$Month.sq = (ungulates.monthly2$Month)^2
write.csv(ungulates.monthly2, "ungulates monthly2.csv")


# Monthly carnivores
carnivores.monthly = aggregate(cbind(Total, Two.way, Human.total.x) ~
                                Location + Month + Location2 + Agecentred + Underpass.type,
                              carnivores_humans, sum)
carnivores.monthly$Month = as.numeric(carnivores.monthly$Month)
carnivores.monthly$Month.sq = (carnivores.monthly$Month)^2
write.csv(carnivores.monthly, "carnivores monthly.csv")


### with year and sampling effort
carnivores.monthly2 = aggregate(cbind(Total, Two.way, Human.total.x) ~
                                Year + Sampling.effort +  Location + Month + Location2 + Agecentred + Underpass.type,
                               carnivores_humans, sum)
carnivores.monthly2$Month = as.numeric(carnivores.monthly2$Month)
carnivores.monthly2$Month.sq = (carnivores.monthly2$Month)^2
write.csv(carnivores.monthly2, "carnivores monthly2.csv")




# Annual ungulates
ungulates.annual = aggregate(cbind(Total, Two.way, Human.total.x) ~
                                Location + Year + Location2 + Underpass.type + Sampling.effort + Agecentred,
                              ungulates_humans, sum)
write.csv(ungulates.annual, "ungulates annual.csv")



# Annual carnivores
carnivores.annual = aggregate(cbind(Total, Two.way, Human.total.x) ~
                                 Location + Year + Location2 + Underpass.type + Sampling.effort +Agecentred,
                               carnivores_humans, sum)
write.csv(carnivores.annual, "carnivores annual.csv")



