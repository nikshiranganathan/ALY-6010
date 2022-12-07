#---------------------- Week_3_Module_3 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Week 3 Assignment - Module 3 R Practice")
print("Course Name -  ALY6010: Probability Theory and Introductory Statistics")

# Loading the packages
library(dplyr)
library(visdat)
library(psych)
library(ggplot2)
library(naniar)
library(skimr)
library(moments)
library(magrittr)

# Importing the dataset of student performance
# Check the current working directory
getwd()
books<-read.csv("Amazon.csv")


#------------------- Data Cleaning -------------------#
# Checking NA values
vis_dat(books)
gg_miss_which(books)
vis_miss(books)
sum(is.na(books))
sum(is.null(books))

# Checking for duplication
duplicated(books)
anyDuplicated(books)
sum(duplicated(books))

# Changing column names
names(books)<-c('Name','Author','Rating','Reviews','Price','Year','Genre')
View(books)

# Changing datatypes
books$Genre=as.factor(books$Genre)

# Boxplots 
boxplot(books$Reviews)
outlier_1<-boxplot(books$Reviews,plot = FALSE)$out
outlier_1
books<-books[-which(books$Reviews %in% outlier_1),]
boxplot(books$Reviews~books$Genre,main = "Boxplot A",xlab = "Genre",ylab = "Reviews",col = c("#FFE0B2", "#F57C00"))
means <- tapply(books$Reviews,books$Genre, mean)
points(means, pch=20,col="royalblue1")

boxplot(books$Rating)
outlier_2<-boxplot(books$Rating,plot = FALSE)$out
outlier_2
books<-books[-which(books$Rating %in% outlier_2),]
boxplot(books$Rating~books$Genre,main = "Boxplot B",xlab = "Genre",ylab = "Rating",col = c("#FFE0B2", "#F57C00"))
means <- tapply(books$Rating, books$Genre, mean)
points(means, pch=20,col="royalblue1")

boxplot(books$Price)
outlier_3<-boxplot(books$Price,plot = FALSE)$out
outlier_3
books<-books[-which(books$Price %in% outlier_3),]
boxplot(books$Price~books$Genre,main = "Boxplot C",xlab = "Genre",ylab = "Price",col = c("#FFE0B2", "#F57C00"))
means <- tapply(books$Price, books$Genre, mean)
points(means, pch=20,col="royalblue1")

#------------------- Exploratory Data Analysis -------------------#
# Analysis
str(books)
summary(books)
glimpse(books)
dim(books)
headTail(books)
describe(books,quant = c(0.25, 0.75),IQR = T)
describeBy(books,group="Genre",quant = c(0.25, 0.75), IQR = T)
describeBy(books,group="Year",quant = c(0.25, 0.75), IQR = T)

# Creating groups
fictionbooks<-filter(books,books$Genre=="Fiction")
nonfictionbooks<-filter(books,books$Genre=="Non Fiction")
Before2010<-filter(books,books$Year=="2009")
year2010to2015<-books[books$Year %in% c('2010', '2011', '2012','2013','2014'), ]
year2015to2019<-books[books$Year %in% c('2015', '2016', '2017','2018','2019'), ]

# Histograms of Features
library(ggplot2)
library(viridis)
ggplot(books,aes(`Price`))+geom_histogram(bins=20,aes(fill=..count..))+labs(title="Histogram for Price")+xlim(0,120)
ggplot(books,aes(`Rating`))+geom_histogram(bins=20,aes(fill=..count..))+labs(title="Histogram for Ratings")+xlim(0,6)+scale_fill_distiller(type = "div")
ggplot(books,aes(`Reviews`))+geom_histogram(aes(fill=..count..))+labs(title="Histogram for Reviews")+xlim(0,20000)+scale_fill_viridis()

# Normal QQ plots
qqnorm(books$Price, pch = 1, frame = FALSE,main="Q-Q Plot (Price)",ylim = c(0, 120),xlim = c(-4, 4))
qqline(books$Price, col = "tomato", lwd = 2)

qqnorm(books$Rating,pch = 1, frame = FALSE,main="Q-Q Plot (Rating)",ylim = c(0, 6),xlim = c(-4, 4))
qqline(books$Rating, col = "tomato", lwd = 2)

qqnorm(books$Reviews,pch = 1, frame = FALSE,main="Q-Q Plot (Reviews)",xlim = c(-4, 4),ylim = c(0, 90000))
qqline(books$Reviews, col = "tomato", lwd = 2)
library("ggpubr")

ggqqplot(nonfictionbooks$Rating,main="Q-Q Plot (Non-Fiction Rating)")
ggqqplot(nonfictionbooks$Reviews,main="Q-Q Plot (Non-Fiction Reviews)")
ggqqplot(nonfictionbooks$Price,main="Q-Q Plot (Non-Fiction Price)")

ggqqplot(fictionbooks$Rating,main="Q-Q Plot (Fiction Rating)")
ggqqplot(fictionbooks$Reviews,main="Q-Q Plot (Fiction Reviews)")
ggqqplot(fictionbooks$Price,main="Q-Q Plot (Fiction Price)")

# Density Plots
library(gridExtra)
library(grid)
library(ggplot2)
library(wesanderson)
density1<-ggplot(books, aes(x = `Price`, colour = `Genre`)) +geom_density()+ scale_color_manual(values = c('#EEB422', '#238E68'))
density2<-ggplot(books, aes(x = `Rating`, colour = `Genre`)) +geom_density()+ scale_color_manual(values = c('#EEB422', '#238E68'))
density3<-ggplot(books, aes(x = `Reviews`, colour = `Genre`)) +geom_density()+ scale_color_manual(values = c('#EEB422', '#238E68'))
grid.arrange(density1,density2,density3)

# Shapiro test
shapiro.test(books$Reviews)
shapiro.test(books$Rating)
shapiro.test(books$Price)

# One Sample T tests

# One-sample t test for mean of Price-Fiction Books
# H0: mu <= 10
# H1: mu > 10
# 95% confidence level for mu
ttest1<-t.test(fictionbooks$Price,mu=10,alt="greater")
ttest1
ttest1$p.value

# One-sample t test for mean of Reviews
# H0: mu >= 14000
# H1: mu < 14000
# 95% confidence level for mu
ttest2<-t.test(fictionbooks$Reviews,mu=14000,alt="less")
ttest2
ttest2$p.value

# One-sample t test for mean of Price
# H0: mu >= 15
# H1: mu < 15
# 95% confidence level for mu
ttest3<-t.test(nonfictionbooks$Price,mu=15,alt="less")
ttest3
ttest3$p.value

# One-sample t test for mean of Reviews
# H0: mu <= 10000
# H1: mu > 10000
# 95% confidence level for mu
ttest4<-t.test(nonfictionbooks$Reviews,mu=10000,alt="greater")
ttest4
ttest4$p.value

# Welch two sample t-test for mean of User Rating
# H0: Difference in means between group Fiction and Non-fiction is equal to 0
# H1: Difference in means between group Fiction and Non-fiction is not equal to 0
ttest5<-t.test(`Rating`~ `Genre`,data=books,conf=0.90)
ttest5
ttest5$p.value

# P value calculation
# Sample 1 - Before 2010 - Price
# H0: The sample mean >= to population mean
# H1: The sample mean < population mean
# Sample Mean
Samplemean<-mean(Before2010$Price)
Samplemean
# Population mean
Populationmean<-mean(books$Price)
Populationmean
# Sample size(n)=50
n<-nrow(Before2010)
degreeoffreedom<-n-1
# Standard Deviation of the sample (SD)
sd<-sd(Before2010$Price)
# T score
tscore<-(Samplemean-Populationmean)/(sd/sqrt(n))
tscore
# P value
pt(tscore,degreeoffreedom,lower.tail = TRUE )

# Sample 2 - Before 2010 - Reviews
# H0: The sample mean >= to population mean
# H1: The sample mean < population mean
# Sample Mean
Samplemean2<-mean(Before2010$Reviews)
Samplemean2
# Population mean
Populationmean2<-mean(books$Reviews)
Populationmean2
# Sample size(n)=50
n2<-nrow(Before2010)
degreeoffreedom2<-n2-1
# Standard Deviation of the sample (SD)
sd2<-sd(Before2010$Reviews)
# T score
tscore2<-(Samplemean2-Populationmean2)/(sd2/sqrt(n2))
tscore2
# P value
pt(tscore2,degreeoffreedom2,lower.tail = TRUE)
