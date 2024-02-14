#---------------------- Week_5_Module_5 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Week 4 Assignment - Module 4 R Practice")
print("Course Name -  ALY6010: Probability Theory and Introductory Statistics")

# Importing the dataset
getwd()
cars<-read.csv("audi.csv")

# Loading the libraries
library(psych)
library(naniar)
library(ggpubr)
library(dplyr)
library(skimr)
library(corrplot)
library(corrgram)
library(ggplot2)
library(ochRe)
library(PerformanceAnalytics)
library(wesanderson)
library(gridExtra)
library(Hmisc)

#------------------- Data Cleaning -------------------#
# Visualization and Checking on N/A values
gg_miss_which(cars)
gg_miss_var(cars)
miss_var_summary(cars)
sum(is.na(cars))
sum(is.null(cars))

# Checking duplicate values and removing it
duplicated(cars)
anyDuplicated(cars)
cars<-cars[!duplicated(cars), ]
anyDuplicated(cars)

# Changing the datatypes
cars$transmission<-as.factor(cars$transmission)
cars$fuelType<-as.factor(cars$fuelType)
cars$model<-as.factor(cars$model)

# checking outliers
boxplot(cars$price)
cars<-subset(cars,price!=145000)

#------------------- Exploratory Data Analysis -------------------#
# Understanding cars dataset
str(cars)
summary(cars)
headTail(cars)
describe(cars,quant = c(0.25, 0.75),IQR = T)
describeBy(cars,group=cars$transmission,quant = c(0.25, 0.75),IQR = T)

# Kolmogorov-Smirnov test
ks.test(cars$price,"pnorm")
ks.test(cars$mileage,"pnorm")
ks.test(cars$tax,"pnorm")
ks.test(cars$mpg,"pnorm")
ks.test(cars$engineSize,"pnorm")

# Density Plots
ggplot(cars, aes(x = `price`, fill = `transmission`)) +geom_density(alpha = 0.6)+scale_fill_ochre(palette="williams_pilbara")+xlab("Price")+ggtitle("Density plot A")
ggplot(cars, aes(x = `mileage`, fill = `transmission`)) +geom_density(alpha = 0.6)+scale_fill_ochre(palette="williams_pilbara")+xlab("Mileage")+ggtitle("Density plot B")
ggplot(cars, aes(x = `mpg`, fill = `transmission`)) +geom_density(alpha = 0.6)+scale_fill_ochre(palette="williams_pilbara")+xlab("Miles per gallon")+ggtitle("Density plot C")
ggplot(cars, aes(x = `tax`, fill = `transmission`)) +geom_density(alpha = 0.6)+scale_fill_ochre(palette="williams_pilbara")+xlab("Tax")+ggtitle("Density plot D")
ggplot(cars, aes(x = `engineSize`, fill = `transmission`)) +geom_density(alpha = 0.6)+scale_fill_ochre(palette="williams_pilbara")+xlab("Engine Size")+ggtitle("Density plot E")

# Correlation
cor<-cars %>% select(price,mileage,tax,mpg,engineSize)
table = rcorr(as.matrix(cor))
table

corrgram(cor, order=TRUE, lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlogram")

chart.Correlation(cor, histogram=TRUE, pch="+")

# Models
cars %>% ggplot(aes(y=tax,x=price,color=transmission))+geom_point(size = 0.7)+geom_smooth(method="lm",se=FALSE,fullrange=TRUE,size = 0.5,color="black")+scale_color_brewer(palette = "Dark2")+ylim(0, 1000)+stat_regline_equation(label.y = 1000, aes(label = ..eq.label..))+ stat_regline_equation(label.y = 950, aes(label = ..rr.label..))+facet_wrap(~transmission)+theme(legend.position = "none")
model1<-lm(price~tax,data=cars)
summary(model1)
plot(model1)

cars %>% ggplot(aes(y=mileage,x=price,color=transmission))+geom_point(size = 0.7)+geom_smooth(method="lm",se=FALSE,fullrange=TRUE,size = 0.5,color="black")+ylim(0, 175000)+scale_color_brewer(palette = "Set1")+stat_regline_equation(label.y = 175000, aes(label = ..eq.label..))+ stat_regline_equation(label.y = 165000, aes(label = ..rr.label..))+facet_wrap(~transmission)+theme(legend.position = "none")
model2<-lm(price~mileage,data=cars)
summary(model2)
plot(model2)

cars %>% ggplot(aes(y=engineSize,x=price,color=transmission))+geom_point(size = 0.7)+geom_smooth(method="lm",se=FALSE,fullrange=TRUE,size = 0.5,color="black")+ylim(0, 8)+scale_color_manual(values= wes_palette("Darjeeling1", n = 3))+stat_regline_equation(label.y = 8, aes(label = ..eq.label..))+ stat_regline_equation(label.y = 7.5, aes(label = ..rr.label..))+facet_wrap(~transmission)+theme(legend.position = "none")
model3<-lm(price~engineSize,data=cars)
summary(model3)
plot(model3)

cars %>% ggplot(aes(y=mpg,x=price,color=transmission))+geom_point(size = 0.7)+ylim(0, 300)+geom_smooth(method="lm",se=FALSE,fullrange=TRUE,size = 0.5,color="black")+scale_color_manual(values= wes_palette("GrandBudapest1", n = 3))+stat_regline_equation(label.y = 300, aes(label = ..eq.label..))+ stat_regline_equation(label.y = 280, aes(label = ..rr.label..))+facet_wrap(~transmission)+theme(legend.position = "none")
model4<-lm(price~mpg,data=cars)
summary(model4)
plot(model4)

model5<-lm(price~mpg+engineSize+mileage+tax,data=cars)
summary(model5)
plot(model5)
