#---------------------- Week_4_Module_4 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Week 4 Assignment - Module 4 R Practice")
print("Course Name -  ALY6010: Probability Theory and Introductory Statistics")

# Loading the packages
library(ggthemes)
library(plotly)
library(visdat)
library(ggpubr)
library(dlookr)
library(psych)
library(skimr)
library(MASS)

# Checking NA values 
vis_dat(cats)
vis_miss(cats)
sum(is.na(cats))
sum(is.null(cats))

#------------------- Exploratory Data Analysis -------------------#
# Analysis
data(cats)
headTail(cats)
str(cats)
summary(cats)
dim(cats)
skim(cats)
describe(cats)
describeBy(cats,group=cats$Sex)

# Creating Subgroups
Femalecats<-subset(cats,cats$Sex=="F")
Malecats<-subset(cats,cats$Sex=="M")
Femalecatsbwt<-subset(cats,cats$Sex=="F",select=c("Bwt"))
Malecatsbwt<-subset(cats,cats$Sex=="M",select=c("Bwt"))

# Shapiro tests
shapiro.test(Malecats$Bwt)
shapiro.test(Femalecats$Bwt)

# Normal QQplot
ggqqplot(Femalecats$Bwt,main="Q-Q Plot (Body weight of Female Cats)")
ggqqplot(Malecats$Bwt,main="Q-Q Plot (Body weight of Male Cats)")

# Density Plots
library(gridExtra)
library(grid)
library(ggplot2)
library(wesanderson)
devtools::install_github("ropenscilabs/ochRe")
require(ochRe)
library(ochRe)
library(naniar)
ggplot(cats, aes(x = `Bwt`, fill = `Sex`)) +geom_density(alpha = 0.6)+xlim(0,5)+scale_fill_ochre(palette="williams_pilbara")+theme(text = element_text(size = 20))+xlab("Body Weight")+ggtitle("Density plots")

# Histograms
ggplot( cats,aes(x=Bwt, fill=Sex)) + geom_histogram( alpha=0.7, position = 'identity',bins = 30) + scale_fill_manual(values= wes_palette("GrandBudapest2", n = 2)) +ggtitle("Histograms for Body weight")+xlab("Body Weight")+labs(fill="")+theme(text = element_text(size = 20))

# Boxplots
ggplot(cats, aes(x = Sex, y = Bwt,fill=Sex)) +geom_boxplot(notch=TRUE,outlier.fill = "red",outlier.shape = 22,outlier.colour = "black")+ggtitle("Boxplot")+scale_fill_ochre(palette="mccrea")+theme_minimal()+geom_jitter(cex = 0.9)+theme(text = element_text(size = 20))+ylab("Body Weight")

#------------------- Part 1 -------------------#
# Two sample t tests
ttest1<-t.test(Malecatsbwt,Femalecatsbwt,alternative = "two.sided")
ttest1
ttest1$p.value

#------------------- Part 2 -------------------#
score_beforeworkshop<-c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
score_afterworkshop<-c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)
difference<-score_afterworkshop-score_beforeworkshop
sleepquality<- data.frame(category = rep(c("Before", "After"), each = 10),qualityscore=c(score_beforeworkshop, score_afterworkshop))

# Analysis
headTail(sleepquality)
summary(sleepquality)
describe(sleepquality)
describeBy(sleepquality,group=sleepquality$category)

# Shapiro tests
shapiro.test(difference)
install.packages("RVAideMemoire")
library(RVAideMemoire)
byf.shapiro(qualityscore~category,data=sleepquality)

# Normal QQplots
library(ggpubr)
ggqqplot(difference,main="Q-Q Plot Difference in Quality scores (After and Before Workshop)")
ggqqplot(sleepquality,main="Q-Q Plot",x = "qualityscore",facet.by = "category",color="category",fill="category")+scale_color_ochre(palette="healthy_reef")+scale_fill_ochre(palette="healthy_reef")

# Density Plots
cols <- c("#79AF97FF","#FFDC91FF")
ggplot(sleepquality, aes(x = `qualityscore`,fill=`category`)) +theme(text = element_text(size = 20))+geom_density(alpha=0.6)+xlab("Quality Score")+ scale_fill_manual(values=cols)+xlab("Quality scores")+ggtitle("Density plot A")
difference=as.data.frame(difference)
ggplot(difference,aes(x=`difference`))+geom_density(fill = "#925E9F99")+theme(text = element_text(size = 20))+xlab("Difference between Quality scores")+ggtitle("Density plot B")

# Boxplot
ggplot(sleepquality, aes(x=category,y=qualityscore, fill=category)) + geom_boxplot()+scale_fill_brewer(palette = "Spectral")+theme(text = element_text(size = 20))+ylab("Quality scores")+ggtitle("Boxplot")

# Paired Boxplot
ggpaired(sleepquality, x = "category", y = "qualityscore",
color = "category", line.color = "seashell4", line.size = 0.5,
palette = "aaas")+ stat_compare_means(paired = TRUE)+theme(text = element_text(size = 20))

# Paired T test
ttest2<-t.test(data=sleepquality,qualityscore~category,paired=TRUE)
ttest2
ttest2$p.value

