##################################################################################################################################
#Exploratory Data Analysis of Traffic Sensor Data

##################################################################################################################################
library(plyr)
traffic <- read.csv("trafficsensorlondon.csv")


length(unique(traffic$ID))

AMC28 <- subset(traffic, ID =="AMC28")
AMC28$date <- as.yearmon(paste(AMC28$YEAR, AMC28$MONTH), "%Y %m")

ggplot(aes(x= date, y = TOTAL.FLOW, color = DIRECTION), data =AMC28) + 
  geom_line()

AMC1 <- subset(traffic, ID == "AMC1")
AMC1$date <- as.yearmon(paste(AMC1$YEAR, AMC1$MONTH), "%Y %m")

ggplot(aes(x= date, y = TOTAL.FLOW, color = DIRECTION), data =AMC1) + 
  geom_line()


Trinity <- subset(traffic, ROAD == "Trinity Road")

Webber <- subset(traffic, ROAD == "Webber Street")

Portland <- subset(traffic, ROAD == "Portland Street")
 Edmund <- subset(traffic, ROAD == "Edmund Street")
 
 
 
Camberwell <- subset(traffic, ROAD == "Camberwell Road") 
Camberwell$date <- as.yearmon(paste(Camberwell$YEAR, Camberwell$MONTH), "%Y %m")
ggplot(aes(x= date, y = TOTAL.FLOW, color = DIRECTION), data =Camberwell) + 
  geom_line()


Newcomen <- subset(traffic, ROAD == "Newcomen Street")
Tanner <- subset(traffic, ROAD == "Tanner Street")
