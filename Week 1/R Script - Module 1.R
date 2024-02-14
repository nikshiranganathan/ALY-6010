#---------------------- Week_1_Module_1 R Script ----------------------#

print("Author : Nikshita Ranganathan")
print("Week 1 Assignment - Module 1 R Pratice")

# Importing the Hotel booking dataset
# Check the current working directory
getwd()
booking=read.csv(file='Hotel.csv')

# Analyzing Booking dataset
# Structure of the dataset
str(booking)

# Dimensions - rows and columns
dim(booking) 

# Column names of the dataset
colnames(booking) 

#------------------- Data Cleaning -------------------#

# Removing and moving columns
library(dplyr)
drop<-c("agent","arrival_date_week_number","market_segment")
cleanbooking<-booking[,!(names(booking) %in% drop)]
cleanbooking$arrival_date<-paste(cleanbooking$arrival_date_day_of_month,cleanbooking$arrival_date_month,cleanbooking$arrival_date_year)
drop1<-c("arrival_date_day_of_month","arrival_date_month","reservation_status_date")
cleanbooking<-cleanbooking[,!(names(cleanbooking) %in% drop1)]
arrival_date<-cleanbooking$arrival_date
cleanbooking<-cleanbooking %>% relocate(arrival_date,.before=stays_in_weekend_nights)
cleanbooking$kids<-cleanbooking$children + cleanbooking$babies
cleanbooking<-cleanbooking %>% relocate(kids,.after = adults)
drop2<-c("children","babies")
cleanbooking<-cleanbooking[,!(names(cleanbooking) %in% drop2)]
cleanbooking$reservedVSassigned<-ifelse(cleanbooking$reserved_room_type == cleanbooking$assigned_room_type, "Same", "Different")
cleanbooking<-cleanbooking %>% relocate(reservedVSassigned,.after = distribution_channel)
drop3<-c("assigned_room_type","reserved_room_type")
cleanbooking<-cleanbooking[,!(names(cleanbooking) %in% drop3)]

# Replacing column values using gsub function
cleanbooking$is_repeated_guest<-gsub("1","Y",as.character(cleanbooking$is_repeated_guest))
cleanbooking$is_repeated_guest<-gsub("0","N",as.character(cleanbooking$is_repeated_guest))
cleanbooking
cleanbooking$is_canceled<-gsub("1","Y",as.character(cleanbooking$is_canceled))
cleanbooking$is_canceled<-gsub("0","N",as.character(cleanbooking$is_canceled))
cleanbooking

# Replacing column values using ifelse() function with conditions
cleanbooking$company<-ifelse(cleanbooking$company == "NULL", "Individual", "Company")

# Removing rows with no adults and no kids
adults<-cleanbooking$adults
kids<-cleanbooking$kids
cleanbooking<-cleanbooking[!(adults==0 & kids==0),]
cleanbooking<-subset(cleanbooking,adults!=0)

# Removing rows with NULL and Undefined values
cleanbooking<-cleanbooking[!grepl("Undefined",cleanbooking$meal),]
cleanbooking<-cleanbooking[!grepl("NULL",cleanbooking$country),]

# Changing column names
names(cleanbooking)<-c('Hotel Type','Is Canceled?(Y/N)','Lead Time','Arrival Year','Arrival date','Stays in weekend','Stays in weekdays','Adults','Kids','Meal','Country','Distribution Channel','Room(Reserved VS Assigned)','Repeated Guest(Y/N)','Previous Cancellations','Previous Not Canceled','Booking Changes','Deposit Type','Company','Waiting days','Customer type','Average Daily Rate','Required Car parks','Special requests','Reservation status')
View(cleanbooking)

# Checking for duplicate rows and removing them
duplicated(cleanbooking)
cleanbooking<-distinct(cleanbooking)
anyDuplicated(cleanbooking)

# Visualization of NA values and removing them
library(visdat)
library(ggplot2)
library(naniar)
library(dplyr)
library(plotly)

gg_miss_which(cleanbooking)
miss_var_summary(cleanbooking)
gg_miss_var(cleanbooking)
complete.cases(cleanbooking)
which(!complete.cases(cleanbooking))
na<-which(!complete.cases(cleanbooking)) 
cleanbooking<- cleanbooking[-na,]

library(psych)
describe(cleanbooking)

# Removing the Outliers
boxplot(cleanbooking$`Waiting days`)
outlier_wd<-boxplot(cleanbooking$`Waiting days`,plot = FALSE)$out
outlier_wd
cleanbooking<-cleanbooking[-which(cleanbooking$`Waiting days` %in% outlier_wd),]
boxplot(cleanbooking$`Waiting days`)

boxplot(cleanbooking$`Stays in weekend`)
outlier_weekend<-boxplot(cleanbooking$`Stays in weekend`,plot = FALSE)$out
outlier_weekend
cleanbooking<-cleanbooking[-which(cleanbooking$`Stays in weekend` %in% outlier_weekend),]
boxplot(cleanbooking$`Stays in weekend`)

boxplot(cleanbooking$Adults)
outlier_Adult<-boxplot(cleanbooking$Adults,plot = FALSE)$out
outlier_Adult
cleanbooking<-cleanbooking[-which(cleanbooking$Adults %in% outlier_Adult),]
boxplot(cleanbooking$Adults)

boxplot(cleanbooking$Kids)
outlier_kids<-boxplot(cleanbooking$Kids,plot = FALSE)$out
outlier_kids
cleanbooking<-cleanbooking[-which(cleanbooking$Kids %in% outlier_kids),]
boxplot(cleanbooking$Kids)

boxplot(cleanbooking$`Lead Time`)
outlier_ld<-boxplot(cleanbooking$`Lead Time`,plot = FALSE)$out
outlier_ld
cleanbooking<-cleanbooking[-which(cleanbooking$`Lead Time` %in% outlier_ld),]
boxplot(cleanbooking$`Lead Time`)

boxplot(cleanbooking$`Stays in weekdays`)
outlier_week<-boxplot(cleanbooking$`Stays in weekdays`,plot = FALSE)$out
outlier_week
cleanbooking<-cleanbooking[-which(cleanbooking$`Stays in weekdays` %in% outlier_week),]
boxplot(cleanbooking$`Stays in weekdays`)

boxplot(cleanbooking$`Average Daily Rate`)
outlier_adr<-boxplot(cleanbooking$`Average Daily Rate`,plot = FALSE)$out
outlier_adr
cleanbooking<-cleanbooking[-which(cleanbooking$`Average Daily Rate` %in% outlier_adr),]
boxplot(cleanbooking$`Average Daily Rate`)

boxplot(cleanbooking$`Booking Changes`)
outlier_book<-boxplot(cleanbooking$`Booking Changes`,plot = FALSE)$out
outlier_book
cleanbooking<-cleanbooking[-which(cleanbooking$`Booking Changes` %in% outlier_book),]
boxplot(cleanbooking$`Booking Changes`)

boxplot(cleanbooking$`Required Car parks`)
outlier_car<-boxplot(cleanbooking$`Required Car parks`,plot = FALSE)$out
outlier_car
cleanbooking<-cleanbooking[-which(cleanbooking$`Required Car parks` %in% outlier_car),]
boxplot(cleanbooking$`Required Car parks`)

boxplot(cleanbooking$`Special requests`)
outlier_spec<-boxplot(cleanbooking$`Special requests`,plot = FALSE)$out
outlier_spec
cleanbooking<-cleanbooking[-which(cleanbooking$`Special requests` %in% outlier_spec),]
boxplot(cleanbooking$`Special requests`)

boxplot(cleanbooking$`Previous Cancellations`)
outlier_pc<-boxplot(cleanbooking$`Previous Cancellations`,plot = FALSE)$out
outlier_pc
cleanbooking<-cleanbooking[-which(cleanbooking$`Previous Cancellations` %in% outlier_pc),]
boxplot(cleanbooking$`Previous Cancellations`)

boxplot(cleanbooking$`Previous Not Canceled`)
outlier_pnc<-boxplot(cleanbooking$`Previous Not Canceled`,plot = FALSE)$out
outlier_pnc
cleanbooking<-cleanbooking[-which(cleanbooking$`Previous Not Canceled` %in% outlier_pnc),]
boxplot(cleanbooking$`Previous Not Canceled`)

boxplot(cleanbooking$`Lead Time`)
outlier_ld2<-boxplot(cleanbooking$`Lead Time`,plot = FALSE)$out
outlier_ld2
cleanbooking<-cleanbooking[-which(cleanbooking$`Lead Time` %in% outlier_ld2),]
boxplot(cleanbooking$`Lead Time`)

boxplot(cleanbooking$`Lead Time`)
outlier_ld3<-boxplot(cleanbooking$`Lead Time`,plot = FALSE)$out
outlier_ld3
cleanbooking<-cleanbooking[-which(cleanbooking$`Lead Time` %in% outlier_ld3),]
boxplot(cleanbooking$`Lead Time`)

boxplot(cleanbooking$`Lead Time`)
outlier_ld4<-boxplot(cleanbooking$`Lead Time`,plot = FALSE)$out
outlier_ld4
cleanbooking<-cleanbooking[-which(cleanbooking$`Lead Time` %in% outlier_ld4),]
boxplot(cleanbooking$`Lead Time`)

boxplot(cleanbooking$`Average Daily Rate`)
outlier_adr2<-boxplot(cleanbooking$`Average Daily Rate`,plot = FALSE)$out
outlier_adr2
cleanbooking<-cleanbooking[-which(cleanbooking$`Average Daily Rate` %in% outlier_adr2),]
boxplot(cleanbooking$`Average Daily Rate`)

boxplot(cleanbooking$`Average Daily Rate`)
outlier_adr3<-boxplot(cleanbooking$`Average Daily Rate`,plot = FALSE)$out
outlier_adr3
cleanbooking<-cleanbooking[-which(cleanbooking$`Average Daily Rate` %in% outlier_adr3),]
boxplot(cleanbooking$`Average Daily Rate`)

boxplot(cleanbooking$`Average Daily Rate`)
outlier_adr4<-boxplot(cleanbooking$`Average Daily Rate`,plot = FALSE)$out
outlier_adr4
cleanbooking<-cleanbooking[-which(cleanbooking$`Average Daily Rate` %in% outlier_adr4),]
boxplot(cleanbooking$`Average Daily Rate`)

boxplot(cleanbooking$`Average Daily Rate`)
outlier_adr5<-boxplot(cleanbooking$`Average Daily Rate`,plot = FALSE)$out
outlier_adr5
cleanbooking<-cleanbooking[-which(cleanbooking$`Average Daily Rate` %in% outlier_adr5),]
boxplot(cleanbooking$`Average Daily Rate`)

# Change datatypes to factor
cleanbooking$`Hotel Type`<-as.factor(cleanbooking$`Hotel Type`)
cleanbooking$`Is Canceled?(Y/N)`<-as.factor(cleanbooking$`Is Canceled?(Y/N)`)
cleanbooking$`Repeated Guest(Y/N)`<-as.factor(cleanbooking$`Repeated Guest(Y/N)`)
cleanbooking$`Distribution Channel`<-as.factor(cleanbooking$`Distribution Channel`)
cleanbooking$Company<-as.factor(cleanbooking$Company)
cleanbooking$Country<-as.factor(cleanbooking$Country)
cleanbooking$Meal<-as.factor(cleanbooking$Meal)
cleanbooking$`Deposit Type`<-as.factor(cleanbooking$`Deposit Type`)
cleanbooking$`Reservation status`<-as.factor(cleanbooking$`Reservation status`)
cleanbooking$`Room(Reserved VS Assigned)`<-as.factor(cleanbooking$`Room(Reserved VS Assigned)`)
cleanbooking$`Arrival Year`<-as.factor(cleanbooking$`Arrival Year`)

#------------------- Exploratory Data Analysis -------------------#

#Checking skew and kurtosis of the variables
library(moments)
skewness(cleanbooking$`Lead Time`)
skewness(cleanbooking$`Average Daily Rate`)
skewness(cleanbooking$`Stays in weekend`)
skewness(cleanbooking$`Stays in weekdays`)
kurtosis(cleanbooking$`Lead Time`)
kurtosis(cleanbooking$`Average Daily Rate`)
kurtosis(cleanbooking$`Stays in weekend`)
kurtosis(cleanbooking$`Stays in weekdays`)

# Exploring data using Frequency tables
install.packages("freqtables")
library(freqtables)

T1<-table(cleanbooking$`Hotel Type`)
T2<-table(cleanbooking$`Meal`)
T3<-cleanbooking %>% freq_table(`Country`)
T4<-cleanbooking %>% freq_table(`Distribution Channel`)
T5<-cleanbooking %>% freq_table(`Deposit Type`)

# Exploring data using Crosstables
install.packages("gmodels")
library(gmodels)
C1<-table(cleanbooking$`Arrival Year`,cleanbooking$`Is Canceled?(Y/N)`)
C2<-CrossTable(cleanbooking$`Hotel Type`,cleanbooking$Company)
C3<-CrossTable(cleanbooking$`Hotel Type`,cleanbooking$`Reservation status`)

# Creating histograms
hist(cleanbooking$`Lead Time`,ylim=c(0,12000),xlim=c(0,300),main = "Histogram - Lead time",xlab="Lead time",ylab="Frequency",col="lightpink")

hist(cleanbooking$`Stays in weekend`,col=rgb(1,0,0,0.5),ylim=c(0,14000),xlim=c(0,4),ylab="Frequency", xlab="Stays",main="Distribution of stays in weekend vs weekdays")
hist(cleanbooking$`Stays in weekdays`,breaks = 30,add=TRUE,col=rgb(0,0,1,0.5),position="identity")
legend("topright",cex=0.7,text.font=3,bty="n", legend=c("Weekend","Weekday"), col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pt.cex=1, pch=15 )

Specialreq<-cleanbooking$`Special requests`
fig<-plot_ly(type="histogram",x=~Specialreq,nbinsx = 10,marker = list(color = "teal"))%>%layout(title = "Frequency distribution - Special Requests",xaxis = list(title = "Number of Special requests",yaxis = list(title = "Frequency"),zeroline = FALSE)) %>% layout(yaxis = list(range = c(0,18000)))
fig

# Creating vectors
Hoteltype<-cleanbooking$`Hotel Type`
Iscancelled<-cleanbooking$`Is Canceled?(Y/N)`
Ld<-cleanbooking$`Lead Time`
ArrivalYear<-cleanbooking$`Arrival Year`
week<-cleanbooking$`Stays in weekdays`
weekend<-cleanbooking$`Stays in weekend`
Customertype<-cleanbooking$`Customer type`
Hoteltype<-cleanbooking$`Hotel Type`
Kids<-cleanbooking$Kids
Adults<-cleanbooking$Adults
Previouscan<-cleanbooking$`Previous Cancellations`
Previousnocan<-cleanbooking$`Previous Not Canceled`
Dischannel<-cleanbooking$`Distribution Channel`
Country<-cleanbooking$Country
Deposittype<-cleanbooking$`Deposit Type`

# Analyzing the dataset
str(cleanbooking)
dim(cleanbooking)
glimpse(cleanbooking)
install.packages("skimr")
library(skimr)
skim(cleanbooking)
headTail(cleanbooking)
summary(cleanbooking)

# ----------- Plot 1: Stacked Bargraph(No of Bookings as per Customer type) ----------- #

library(plotly)
library(dplyr)
ggplot(cleanbooking,aes(`Customer type`,group=`Hotel Type`,fill=`Hotel Type`),stat="identity")+geom_bar()+scale_fill_brewer(palette="Spectral")+labs(fill="Types of Hotel")+ggtitle("Bookings by Customer type")+xlab("Customer category")+ylab("No of Bookings")+theme(axis.text.x = element_text(angle = 90,size=7))

# ----------- Plot 2: Stacked Bargraph(No of Bookings by Hotel type adding meal) ----------- #

library(plotly)
library(dplyr)
ggplot(cleanbooking,aes(`Hotel Type`,group=`Meal`,fill=`Meal`),stat="identity")+geom_bar()+scale_fill_brewer(palette="Set3")+labs(fill="Meal Preferences")+ggtitle("Bookings by Hotel type as per meal packages")+xlab("Hotel category")+ylab("No of Bookings")+theme(axis.text.x = element_text(angle = 90,size=7))

# ----------- Plot 3: Histogram with density curve ----------- #

library(hrbrthemes)
install.packages("devtools")
library(devtools)
devtools::install_github('cttobin/ggthemr')
install.packages("ggthemr")
library(ggthemr)
ggthemr("dust")
ADRate<-cleanbooking$`Average Daily Rate`
ggplot(cleanbooking,aes(x=ADRate))+geom_histogram(aes(y=..density..,fill=`Company`),bins=30,color="grey")+geom_density(fill="orange",alpha=0.2)+scale_fill_brewer(palette="Pastel1")+xlab("Average Day rate")+ylab("Density")+ggtitle("Histogram with density curve")+labs(fill = "Company/Individual")

# ----------- Plot 4: Bubble Chart(Lead time vs Count as per year) ----------- #

cleanbooking$`Lead Time`<-as.factor(cleanbooking$`Lead Time`)
df2<-cleanbooking %>% group_by(`Arrival Year`,`Lead Time`) %>% summarise(count = n())%>% top_n(10)
plot_ly(df2,x= ~`Lead Time`,y= ~count,type = 'scatter',mode='markers',color= ~ `Arrival Year`,colors = "Accent",marker = list(size = ~`Lead Time`,sizeref = 0.3,opacity = 20,line = list(width = 1, color = 'black'))) %>% layout(title = 'Bubble Chart',yaxis = list(title = 'Count'),xaxis=list(title='Lead time'))

# ----------- Plot 5: World Map(Number of Bookings in Top 25 countries) ----------- #
df3<-cleanbooking %>% group_by(`Country`) %>% summarise(count = n())%>% top_n(25) %>%  arrange(desc(count))
bound <- list(color = toRGB("grey"),width=1)
library("RColorBrewer")
proj<- list(showframe = F,projection = list(type = 'Miller'))
fig3 <- plot_ly(df3,type = 'choropleth',locations=df3$Country,z=df3$count,text=df3$Country,colors="Spectral" ,marker = list(line = bound))
fig3 <- fig3 %>% layout(title = 'Choropleth Map- Number of Bookings in Top 25 countries', geo = proj)
fig3
# ----------- Plot 6: Dotplot(Arrival Date vs Count of Repeated Guest(Y/N) ----------- #

ggthemr_reset()
cleanbooking$`Stays in weekend`<-as.factor(cleanbooking$`Stays in weekend`)
df2015<-cleanbooking %>% group_by(`Repeated Guest(Y/N)`,`Arrival date`) %>% summarize(count=n()) %>% arrange(desc(count))
fig4<-plot_ly(df2015,x=~`Arrival date`,y=~count,type="scatter",mode="markers",color =~`Repeated Guest(Y/N)`,colors = "Set1")
fig4<-fig4 %>% layout(title = 'Dot Plot', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Arrival Date'), yaxis = list(title = 'Count'), legend = list(title=list(text='Repeated Guest (Y/N)')))
fig4

# ----------- Plot 7: Animated Graphs(Number of Stays in Weekdays/Weekend as per Hotel Type ----------- #

ggthemr_reset()
library(ggplot2)
library(gganimate)
cleanbooking$`Stays in weekend`=as.factor(cleanbooking$`Stays in weekend`)
animate<-cleanbooking %>% group_by(`Hotel Type`,`Stays in weekend`) %>% summarise(count=n())
ggplot(animate,aes(x=`Stays in weekend`,y=count,fill=`Hotel Type`))+geom_col(position = "dodge", colour = "black")+ scale_fill_brewer(palette = "Dark2") +transition_states(`Stays in weekend`,transition_length = 2,state_length = 1)+ ease_aes('sine-in-out')+ enter_fade() +  exit_shrink()
rstudioapi::viewer("http://localhost:17916/session/file59c0176c7600.gif")
anim_save(filename = "Animated.gif")

cleanbooking$`Stays in weekdays`=as.factor(cleanbooking$`Stays in weekdays`)
animate2<-cleanbooking %>% group_by(`Hotel Type`,`Stays in weekdays`) %>% summarise(count=n())
ggplot(animate2,aes(x=`Stays in weekdays`,y=count,fill=`Hotel Type`))+geom_col(position = "dodge", colour = "black")+ scale_fill_brewer(palette = "Pastel2") +transition_states(`Stays in weekdays`,transition_length = 2,state_length = 1)+ ease_aes('sine-in-out')+ enter_fade() +  exit_shrink()
rstudioapi::viewer("http://localhost:17916/session/file59c010e555f2.gif")
anim_save(filename = "Animated2.gif")

# ----------- Plot 8: Donut Chart(Distribution Channel) ----------- #

library(dplyr)
dataframe2<-cleanbooking %>% group_by(`Distribution Channel`) %>% summarise(count=n())
colors<-c("deeppink","darkseagreen","darkorange","cornflowerblue")
donut<-plot_ly(dataframe2,labels = ~`Distribution Channel`,values=~count,type="pie",hole=0.6,pull = c(0.1, 0.1, 0.1, 0.1),marker = list(colors = colors)) 
donut<-donut %>% layout(title="Distribution Channels - Donut chart",showlegend = T,xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
donut

# ----------- Plot 9: Correlation Matrix ----------- #
library(GGally)
cormat<-booking %>% select(`is_canceled`,`lead_time`,`stays_in_weekend_nights`,`stays_in_week_nights`,`adults`,`children`,`babies`,`is_repeated_guest`,`previous_cancellations`,`previous_bookings_not_canceled`,`booking_changes`,`days_in_waiting_list`,`required_car_parking_spaces`,`total_of_special_requests`)
colnames(cormat)
names(cormat)<-"Ca"
ggcorr(cormat,hjust = 1,layout.exp=2, size = 3)

