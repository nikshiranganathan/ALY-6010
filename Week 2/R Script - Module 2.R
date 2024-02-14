#---------------------- Week_2_Module_2 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Week 2 Assignment - Module 2 R Practice")
print("Course Name -  ALY6010: Probability Theory and Introductory Statistics")

# Loading the packages
library(dplyr)
library(psych)
library(ggplot2)
library(naniar)
library(skimr)
library(moments)
library(magrittr)

# Importing the dataset of student performance
# Check the current working directory
getwd()
studentdata<-read.csv("Students.csv")

#------------------- Data Cleaning -------------------#

# Changing the column names
names(studentdata)
names(studentdata)<-c('Gender','Ethnicity','Parental Level of Education','Lunch','Test Prep Score','Math score','Reading score','Writing score')

# Checking NA values
gg_miss_which(studentdata)
miss_var_summary(studentdata)
sum(is.na(studentdata))

# Checking for duplication
duplicated(studentdata)
anyDuplicated(studentdata)

# Changing datatypes to factors
studentdata$Gender=as.factor(studentdata$Gender)
studentdata$Lunch=as.factor(studentdata$Lunch)
studentdata$Ethnicity=as.factor(studentdata$Ethnicity)
studentdata$`Parental Level of Education`=as.factor(studentdata$`Parental Level of Education`)
studentdata$`Test Prep Score`=as.factor(studentdata$`Test Prep Score`)

#------------------- Exploratory Data Analysis -------------------#

# Analyzing studentdata dataset
str(studentdata)
dim(studentdata)
summary(studentdata)
glimpse(studentdata)
skim(studentdata)
headTail(studentdata)
psych::describe(studentdata)

# Descriptive Statistics (describe and other functions)
psych::describe(studentdata$`Math score`)
IQR(studentdata$`Math score`)
t.test(studentdata$`Math score`)
var(studentdata$`Math score`)
quantile(studentdata$`Math score`)

psych::describe(studentdata$`Reading score`)
IQR(studentdata$`Reading score`)
t.test(studentdata$`Reading score`)
var(studentdata$`Reading score`)
quantile(studentdata$`Reading score`)

psych::describe(studentdata$`Writing score`)
IQR(studentdata$`Writing score`)
t.test(studentdata$`Writing score`)
var(studentdata$`Writing score`)
quantile(studentdata$`Writing score`)

install.packages("bruceR")
library(bruceR)
print_table(studentdata)

# Descriptive Statistics by Group (Using describeby)
# BY GENDER
describeBy(studentdata,group = studentdata$Gender)
female<-filter(studentdata,studentdata$Gender=='female')
f<-describe(female)
print_table(f,digits=1)
male<-filter(studentdata,studentdata$Gender=='male')
m<-describe(male)
print_table(m,digits=1)

# BY ETHNICITY
describeBy(studentdata,group = studentdata$Ethnicity)

# BY TEST PREP
describeBy(studentdata,group = studentdata$`Test Prep Score`)
none<-filter(studentdata,studentdata$`Test Prep Score`=='none')
n<-describe(none)
print_table(n,digits=1)
completed<-filter(studentdata,studentdata$`Test Prep Score`=='completed')
c<-describe(completed)
print_table(c,digits=1)

# BY LUNCH
describeBy(studentdata,group = studentdata$`Lunch`)

# BY LEVEL OF EDUCATION
describeBy(studentdata,group = studentdata$`Parental Level of Education`)

#------------------- Data Visualizations -------------------#

# Plot 1
par( mfrow= c(1,3) )
hist(studentdata$`Math score`,ylim = c(0,300),xlim=c(0,100),main="Histogram A",xlab="Math Scores",col="#E1DEFC")
hist(studentdata$`Reading score`,ylim = c(0,300),xlim=c(0,100),main="Histogram B",xlab="Reading Scores",col="lightsalmon")
hist(studentdata$`Writing score`,ylim = c(0,300),xlim=c(0,100),main="Histogram C",xlab="Writing Scores",col="yellowgreen")

# Plot 2 -  Correlation Matrix
scores<-studentdata %>% select(`Math score`,`Reading score`,`Writing score`)
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot::ggcorrplot(cor(scores),colors = c("#6D9EC1", "white", "#E46726"),lab = TRUE,title = "Correlation Matrix")

# Plot 3
library(wesanderson)
ggplot(studentdata,aes(x=`Test Prep Score`,y=`Math score`,fill=`Gender`))+geom_boxplot()+facet_wrap(~`Test Prep Score`,scale="free")+ scale_fill_manual(values=wes_palette(n=2, name="BottleRocket2"))

ggplot(studentdata,aes(x=`Test Prep Score`,y=`Reading score`,fill=`Gender`))+geom_boxplot()+facet_wrap(~`Test Prep Score`,scale="free")+ scale_fill_manual(values=wes_palette(n=2, name="Darjeeling2"))

ggplot(studentdata,aes(x=`Test Prep Score`,y=`Writing score`,fill=`Gender`))+geom_boxplot()+facet_wrap(~`Test Prep Score`,scale="free")+ scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2"))


# Plot 4 - 
ggplot(studentdata,aes(x=`Parental Level of Education`,y=`Math score`,fill=`Parental Level of Education`))+geom_boxplot(alpha=0.4,notch=TRUE,outlier.colour = "red",outlier.size = 2)+geom_jitter(color="black", size=0.4, alpha=0.9)+scale_color_brewer(palette="Dark2")+ggtitle("Boxplot (A) with jitter")

ggplot(studentdata,aes(x=`Parental Level of Education`,y=`Reading score`,fill=`Parental Level of Education`))+geom_boxplot(alpha=0.4,notch=TRUE,outlier.colour = "red",outlier.size = 2)+geom_jitter(color="black", size=0.4, alpha=0.9)+scale_color_brewer(palette="Dark2")+ggtitle("Boxplot(B) with jitter")

ggplot(studentdata,aes(x=`Parental Level of Education`,y=`Writing score`,fill=`Parental Level of Education`))+geom_boxplot(alpha=0.4,notch=TRUE,outlier.colour = "red",outlier.size = 2)+geom_jitter(color="black", size=0.4, alpha=0.9)+scale_color_brewer(palette="Dark2")+ggtitle("Boxplot(C) with jitter")

# Plot 5 - Scatterplots
Reading=studentdata$`Reading score`
Math=studentdata$`Math score`
Writing=studentdata$`Writing score`

par( mfrow= c(1,3) )
plot(Math, Reading, main = "Scatterplot A",xlab = "Math scores", ylab = "Reading scores",pch = 16, frame = FALSE,col="sandybrown")  
abline(lm(Reading ~ Math, data = studentdata), col = "black",lty=2,lwd=1.5)

plot(Reading, Writing, main = "Scatterplot B",xlab = "Reading scores", ylab = "Writing scores",pch = 16, frame = FALSE,col="#56B4E9")  
abline(lm(Writing ~ Reading, data = studentdata), col = "black",lty=2,lwd=1.5)

plot(Writing, Math, main = "Scatterplot C",xlab = "Writing scores", ylab = "Math scores",pch = 16, frame = FALSE,col="#00A087FF")  
abline(lm(Math ~ Writing, data = studentdata), col = "black",lty=2,lwd=1.5)

# Plot 6
library(ggplot2)
install.packages("ggExtra")
library(ggExtra)

fig<-ggplot(studentdata, aes(x=Math, y=Reading,color=`Gender`)) +geom_point() +scale_color_manual(values=wes_palette(n=2, name="Chevalier1"))+theme(legend.position="top")
ggMarginal(fig, type="boxplot")

fig2<-ggplot(studentdata, aes(x=Reading, y=Writing,color=`Gender`)) +geom_point() +scale_color_manual(values=wes_palette(n=2, name="Royal1"))+theme(legend.position="top")
ggMarginal(fig2, type="boxplot")

fig3<-ggplot(studentdata, aes(x=Writing, y=Math,color=`Gender`)) +geom_point() +scale_color_manual(values=wes_palette(n=2, name="Moonrise3"))+theme(legend.position="top")
ggMarginal(fig3, type="boxplot")

# Plot 7
install.packages("gridExtra")
library(gridExtra)
fig4<-ggplot(studentdata,aes(x=`Lunch`,y=`Math score`,col=`Lunch`))+geom_jitter()+coord_flip()+scale_color_brewer(palette="Accent")
fig5<-ggplot(studentdata,aes(x=`Lunch`,y=`Reading score`,col=`Lunch`))+geom_jitter()+coord_flip()+scale_color_brewer(palette="Accent")
fig6<-ggplot(studentdata,aes(x=`Lunch`,y=`Writing score`,col=`Lunch`))+geom_jitter()+coord_flip()+scale_color_brewer(palette="Accent")
grid.arrange(fig4, fig5, fig6)
             
# Plot 8
library(dplyr)
library(hrbrthemes)
library(tidyr)
library(viridis)
ggplot(studentdata,aes(x=`Math score`,group=`Ethnicity`,fill=`Ethnicity`))+geom_density(alpha=0.3)+theme_ipsum()+ggtitle("Multiple Density Chart A")
ggplot(studentdata,aes(x=`Reading score`,group=`Ethnicity`,fill=`Ethnicity`))+geom_density(alpha=0.3)+theme_ipsum()+ggtitle("Multiple Density Chart B")
ggplot(studentdata,aes(x=`Writing score`,group=`Ethnicity`,fill=`Ethnicity`))+geom_density(alpha=0.3)+theme_ipsum()+ggtitle("Multiple Density Chart C")
