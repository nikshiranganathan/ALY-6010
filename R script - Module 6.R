#---------------------- Week_6_Module_6 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Week 6 Assignment - Module 6 R Practice")
print("Course Name -  ALY6010: Probability Theory and Introductory Statistics")

# Importing cars dataset
getwd()
cars<-read.csv("scrap price.csv")

# Installing and loading the libraries
library(psych)
library(skimr)
library(ggplot2)
library(dplyr)
library(naniar)
library(visdat)
library(corrplot)
library(corrgram)
library(Hmisc)
library(gridExtra)
library(wesanderson)
install.packages("ggiraph")
install.packages("ggiraphExtra")
install.packages("plyr")
require(ggiraph)
require(ggiraphExtra)
library(ggpmisc)
require(plyr)
library(ggpubr)
library(car)

# Visualization and Checking NA values
vis_dat(cars)
gg_miss_which(cars)
sum(is.na(cars))
sum(is.null(cars))

# Check for duplication
anyDuplicated(cars)
duplicated(cars)

# Changing the datatypes
cars$fueltypes<-as.factor(cars$fueltypes)
cars$fuelsystem<-as.factor(cars$fuelsystem)
cars$enginetype<-as.factor(cars$enginetype)
cars$enginelocation<-as.factor(cars$enginelocation)
cars$aspiration<-as.factor(cars$aspiration)
cars$carbody<-as.factor(cars$carbody)
cars$doornumbers<-as.factor(cars$doornumbers)
cars$drivewheels<-as.factor(cars$drivewheels)
cars$cylindernumber<-as.factor(cars$cylindernumber)

# Outliers
boxplot(cars$price)
cars<-subset(cars,price!=45400)

# Analysis
headTail(cars)
str(cars)
summary(cars)
dim(cars)
skim(cars)
describe(cars)
glimpse(cars)
describeBy(cars,group=cars$fueltypes,quant = c(0.25, 0.75), IQR = T)

# Correlation
# Correlation of Price with Car Dimensions
corr1<-cars %>% select(wheelbase,carlength,carwidth,carheight,curbweight,price)
corrgram(corr1, order=TRUE, upper.panel=panel.cor)
corrgram(corr1, lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)

# Correlation of Price with Engine Specification
corr2<-cars %>% select(enginesize,boreratio,stroke,compressionratio,price)
corrgram(corr2, order=TRUE, lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt)
corrgram(corr2, lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)

corr3<-cars %>% select(horsepower,peakrpm,citympg,highwaympg,price)
corrgram(corr3, order=TRUE, upper.panel=panel.conf,
         lower.panel=panel.ellipse, text.panel=panel.txt)
corrgram(corr3, lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)

# Models
model1<-lm(formula = price ~ enginesize, data = cars)
summary(model1)
ggplot(data = cars, aes(x = model1$residuals)) +geom_histogram(fill = 'steelblue', color = 'black') +  labs(title = 'Histogram of Residuals (Model1)', x = 'Residuals', y = 'Frequency')
ggPredict(model1,se=TRUE,interactive=TRUE)
plot(model1)

cars$fuelgas<-ifelse(cars$fueltypes=="gas",1,0)
cars$fueldiesel<-ifelse(cars$fueltypes=="diesel",1,0)
model2=lm(price~enginesize+fuelgas+fueldiesel,data=cars)
summary(model2)
ggplot(data = cars, aes(x = model2$residuals)) +geom_histogram(fill = 'steelblue', color = 'black') +  labs(title = 'Histogram of Residuals (Model2)', x = 'Residuals', y = 'Frequency')
ggplot(cars,aes(y=price,x=enginesize,color=factor(fueltypes)))+geom_point(size=0.75)+stat_smooth(method="lm",se=FALSE)+scale_colour_brewer(palette = "Accent")+labs(color="Fuel Types")+ stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))
plot(model2)
avPlots(model2)

cars$wheelsrwd<-ifelse(cars$drivewheels=="rwd",1,0)
cars$wheelsfwd<-ifelse(cars$drivewheels=="fwd",1,0)
cars$wheels4wd<-ifelse(cars$drivewheels=="4wd",1,0)
model3=lm(price~enginesize+wheels4wd+wheelsfwd+wheelsrwd,data=cars)
summary(model3)
ggplot(data = cars, aes(x = model3$residuals)) +geom_histogram(fill = 'steelblue', color = 'black') +  labs(title = 'Histogram of Residuals (Model3)', x = 'Residuals', y = 'Frequency')
ggplot(cars,aes(y=price,x=enginesize,color=factor(drivewheels)))+geom_point()+stat_smooth(method="lm",se=FALSE)+scale_colour_brewer(palette = "Set1")+labs(color="Drive Wheels")+ stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))
plot(model3)
avPlots(model3)

# Creating subsets
gas=subset(cars, fueltypes == "gas")
diesel=subset(cars, fueltypes == "diesel")
model4=lm(price~enginesize,data=gas)
summary(model4)
ggplot(data = gas, aes(x = model4$residuals)) +geom_histogram(fill = 'steelblue', color = 'black') +  labs(title = 'Histogram of Residuals (Model4)', x = 'Residuals', y = 'Frequency')
ggplot(gas,aes(y=price,x=enginesize))+geom_point(size=0.75)+geom_smooth(method="lm")+stat_regline_equation(label.x = 50, label.y = 45000)
plot(model4)

model5=lm(price~enginesize,data=diesel)
summary(model5)
ggplot(data = diesel, aes(x = model5$residuals)) +geom_histogram(fill = 'steelblue', color = 'black') +  labs(title = 'Histogram of Residuals (Model5)', x = 'Residuals', y = 'Frequency')
ggplot(diesel,aes(y=price,x=enginesize))+geom_point(size=0.75)+geom_smooth(method="lm")+stat_regline_equation(label.x = 103, label.y = 27000)
plot(model5)

fourwheel=subset(cars, drivewheels == "4wd")
rearwheel=subset(cars, drivewheels == "rwd")
frontwheel=subset(cars, drivewheels == "fwd")
model6=lm(price~enginesize,data=fourwheel)
summary(model6)
ggplot(data = fourwheel, aes(x = model6$residuals)) +geom_histogram(fill = 'steelblue', color = 'black') +  labs(title = 'Histogram of Residuals (Model6)', x = 'Residuals', y = 'Frequency')
ggplot(fourwheel,aes(y=price,x=enginesize))+geom_point(size=0.75)+geom_smooth(method="lm")+stat_regline_equation(label.x = 92, label.y = 20000)
plot(model6)

model7=lm(price~enginesize,data=rearwheel)
summary(model7)
ggplot(data = rearwheel, aes(x = model7$residuals)) +geom_histogram(fill = 'steelblue', color = 'black') +  labs(title = 'Histogram of Residuals (Model7)', x = 'Residuals', y = 'Frequency')
ggplot(rearwheel,aes(y=price,x=enginesize))+geom_point(size=0.75)+geom_smooth(method="lm")+stat_regline_equation(label.x = 80, label.y = 47000)
plot(model7)

model8=lm(price~enginesize,data=frontwheel)
summary(model8)
ggplot(data = frontwheel, aes(x = model8$residuals)) +geom_histogram(fill = 'steelblue', color = 'black') +  labs(title = 'Histogram of Residuals (Model8)', x = 'Residuals', y = 'Frequency')
ggplot(frontwheel,aes(y=price,x=enginesize))+geom_point(size=0.75)+geom_smooth(method="lm")+stat_regline_equation(label.x = 50, label.y = 25000)
plot(model8)
