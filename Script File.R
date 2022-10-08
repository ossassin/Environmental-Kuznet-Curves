#Group 13 R File

#___________________ Installing and Adding Relevant Packages

install.packages("plm")
library(plm)

install.packages("gplots")
library(gplots)

library(readr)
library(ggplot2)
library(dplyr)
library(lattice)

install.packages("jtools")
install.packages("ggstance")
library(jtools)
library(ggstance)

install.packages("broom.mixed")
library(broom.mixed)

install.packages("moments")
library(moments)

source("http://www.sthda.com/upload/rquery_cormat.r")
install.packages("corrplot")
library(corrplot)

#__________________ Importing Data File


#Note: You will need to use import data set since the directory might be different for you
GDP_CO2_Data <- read_csv("~/Downloads/GDP-CO2 Data.csv")
View(GDP_CO2_Data)

#Making data into Panel Data according to country and year
GDP_CO2_Panel_Data = pdata.frame(GDP_CO2_Data, index=c("Country","Year"))

#It'll tell you the number of countries, years and rows
pdim(GDP_CO2_Panel_Data)

#_______________________ Time Series Visualizations w.r.t all variables

#CO2
c <- ggplot(GDP_CO2_Panel_Data, aes(x=Year, y=CO2, group=Country, color=Country)) +
  geom_line() + 
  xlab("Year") +
  ylab("CO2")
c

#GDP
g <- ggplot(GDP_CO2_Panel_Data, aes(x=Year, y=GDP, group=Country, color=Country)) +
  geom_line() + 
  xlab("Year") +
  ylab("GDP")
g
ggplot(GDP_CO2_Data, aes(x=GDP, y=CO2)) + geom_point() + geom_smooth(method=lm)


#Population Growth
p <- ggplot(GDP_CO2_Panel_Data, aes(x=Year, y=PopGrowth, group=Country, color=Country)) +
  geom_line() + 
  xlab("Year") +
  ylab("Population Growth (%)")
p
ggplot(GDP_CO2_Data, aes(x=PopGrowth, y=CO2)) + geom_point() + geom_smooth(method=lm)


#forest area
f <- ggplot(GDP_CO2_Panel_Data, aes(x=Year, y=Forest_Area, group=Country, color=Country)) +
  geom_line() + 
  xlab("Year") +
  ylab("Forest Area")
f
ggplot(GDP_CO2_Data, aes(x=Forest_Area, y=CO2)) + geom_point() + geom_smooth(method=lm)


#Electric power
e <- ggplot(GDP_CO2_Panel_Data, aes(x=Year, y=Electric_Power, group=Country, color=Country)) +
  geom_line() + 
  xlab("Year") +
  ylab("Electric Power")
e
ggplot(GDP_CO2_Data, aes(x=Electric_Power, y=CO2)) + geom_point() + geom_smooth(method=lm)


#Mobile Subs
m <- ggplot(GDP_CO2_Panel_Data, aes(x=Year, y=Mobile_Cell_Subs, group=Country, color=Country)) +
  geom_line() + 
  xlab("Year") +
  ylab("Mobile Subs")
m
ggplot(GDP_CO2_Data, aes(x=Mobile_Cell_Subs, y=CO2)) + geom_point() + geom_smooth(method=lm)


#Domestic Credit
d <- ggplot(GDP_CO2_Panel_Data, aes(x=Year, y=Domestic_Credit, group=Country, color=Country)) +
  geom_line() + 
  xlab("Year") +
  ylab("Domestic Credit")
d
ggplot(GDP_CO2_Data, aes(x=Domestic_Credit, y=CO2)) + geom_point() + geom_smooth(method=lm)


#Renewable
r <- ggplot(GDP_CO2_Panel_Data, aes(x=Year, y=Renewable, group=Country, color=Country)) +
  geom_line() + 
  xlab("Year") +
  ylab("Renewable")
r
ggplot(GDP_CO2_Data, aes(x=Renewable, y=CO2)) + geom_point() + geom_smooth(method=lm)


#___________________________ Other Visualizations and Exploratory Analysis

#Scatterplots
xyplot(GDP_CO2_Panel_Data$CO2~GDP_CO2_Panel_Data$GDP, type=c("smooth", "p"), xlab = "Mobile Cellular Subscriptions", ylab = "CO2 emissions", )
xyplot(GDP_CO2_Panel_Data$CO2~GDP_CO2_Panel_Data$Electric_Power, type=c("smooth", "p"), xlab = "Mobile Cellular Subscriptions", ylab = "CO2 emissions", )
xyplot(GDP_CO2_Panel_Data$CO2~GDP_CO2_Panel_Data$Mobile_Cell_Subs, type=c("smooth", "p"), xlab = "Mobile Cellular Subscriptions", ylab = "CO2 emissions", )
xyplot(GDP_CO2_Panel_Data$CO2~GDP_CO2_Panel_Data$Renewable, type=c("smooth", "p"), xlab = "Mobile Cellular Subscriptions", ylab = "CO2 emissions", )

#Correlation Matrix
GDP = GDP_CO2_Panel_Data[,!(names(GDP_CO2_Panel_Data) %in% c("Country","Year"))]
rquery.cormat(GDP)

#Exploratory Analysis 
plotmeans(GDP~Year, data = GDP_CO2_Data)
plotmeans(CO2~Year, data = GDP_CO2_Data)
plotmeans(Forest_Area~Year, data = GDP_CO2_Data)
plotmeans(Mobile_Cell_Subs~Year, data = GDP_CO2_Data)
plotmeans(Domestic_Credit~Year, data = GDP_CO2_Data)
plotmeans(Electric_Power~Year, data = GDP_CO2_Data)
plotmeans(Renewable~Year, data = GDP_CO2_Data)
plotmeans(PopGrowth~Year, data = GDP_CO2_Data)

CO2<-GDP_CO2_Data$CO2
GDP<-GDP_CO2_Data$GDP
FA<-GDP_CO2_Data$Forest_Area
MCS<-GDP_CO2_Data$Mobile_Cell_Subs
DC<-GDP_CO2_Data$Domestic_Credit
EP<-GDP_CO2_Data$Electric_Power
R<-GDP_CO2_Data$Renewable
PG<-GDP_CO2_Data$PopGrowth
boxplot(GDP, xlab="GDP")
boxplot(FA, xlab="Forest Area")
boxplot(MCS, xlab="Mobile Cell Subscriptions")
boxplot(DC, xlab="Domestic Credit")
boxplot(EP, xlab="Electric Power")
boxplot(R, xlab="Renewable")
boxplot(PG, xlab="Population Growth")


summary(CO2)
skewness(CO2)
kurtosis(CO2)

summary(GDP)
skewness(GDP)
kurtosis(GDP)

summary(FA)
skewness(FA)
kurtosis(FA)

summary(MCS)
skewness(MCS)
kurtosis(MCS)

summary(DC)
skewness(DC)
kurtosis(DC)

summary(EP)
skewness(EP)
kurtosis(EP)

summary(R)
skewness(R)
kurtosis(R)

summary(PG)
skewness(PG)
kurtosis(PG)


#Calculate mean while ignoring empty cells
summary(GDP_CO2_Data)

#Calculating Standard Deviations
sd(CO2, na.rm = TRUE)
sd(GDP, na.rm = TRUE)
sd(PG, na.rm = TRUE)
sd(R, na.rm = TRUE)
sd(FA, na.rm = TRUE)
sd(EP, na.rm = TRUE) 
sd(MCS, na.rm = TRUE)
sd(DC, na.rm = TRUE)

var(CO2, na.rm = TRUE)
var(GDP, na.rm = TRUE)
var(PG, na.rm = TRUE)
var(R, na.rm = TRUE)
var(FA, na.rm = TRUE)
var(EP, na.rm = TRUE) 
var(MCS, na.rm = TRUE)
var(DC, na.rm = TRUE)

#____________________ Creating models for the panel fixed affects regression

panel1 = plm(log(CO2)~log(GDP) + PopGrowth + Renewable + Forest_Area + Electric_Power + Mobile_Cell_Subs + Domestic_Credit, data = GDP_CO2_Panel_Data, model = "within")
summary(panel1)

panel2 = plm(log(CO2)~log(GDP) + PopGrowth + Renewable + log(Forest_Area) + log(Electric_Power) + Mobile_Cell_Subs + Domestic_Credit, data = GDP_CO2_Panel_Data,na.rm = TRUE, model = "within")
summary(panel2)

panel3 = plm(log(CO2)~log(GDP) + I(log(GDP)^2) + PopGrowth + Renewable + log(Forest_Area) + log(Electric_Power) + Mobile_Cell_Subs + Domestic_Credit, data = GDP_CO2_Panel_Data,na.rm = TRUE, model = "within")
summary(panel3)

panel4 = plm(log(CO2)~log(GDP) + I(log(GDP)^2) + PopGrowth + log(Renewable+1) + Forest_Area + Electric_Power + Mobile_Cell_Subs + Domestic_Credit, data = GDP_CO2_Panel_Data,na.rm = TRUE, model = "within")
summary(panel4)

panel5 = plm(log(CO2)~log(GDP) + I(log(GDP)^2) + PopGrowth + Renewable + Forest_Area + Electric_Power + Mobile_Cell_Subs + Domestic_Credit +factor(Year), data = GDP_CO2_Panel_Data,na.rm = TRUE, model = "within")
summary(panel5)

plot_summs(panel1, panel2, panel3, panel4, panel5, panel6, scale = TRUE, plot.distributions = TRUE, rescale.distributions = TRUE)

#___________ The Final Model ________#
panel6 = plm(log(CO2)~log(GDP) + I(log(GDP)^2) + PopGrowth + Renewable + Forest_Area + Electric_Power + Mobile_Cell_Subs + Domestic_Credit, data = GDP_CO2_Panel_Data,na.rm = TRUE, model = "within")
summary(panel6)
