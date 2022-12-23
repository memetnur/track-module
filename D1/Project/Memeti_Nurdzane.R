'''
project D1
memetnur
date:06.11.2020
covid-19 outbreak
'''

install.packages(" ggplot2 ")
install.packages(" mice ")
install.packages(" missForest ")
install.packages ("tidyverse")
install.package("reshape2")
install.package(" ggthemeis ")
install.package(" devtools ")
install.package(" ggExtra ")
install.package(" psych ")
install.packages("hrbrthemes")
install.packages("ggrepel")

library("tidyverse")
library("mice")
library("ggplot2")
library("reshape2")
library("ggthemes")
library("devtools")
library("ggExtra")
library("psych")
library("missForest")
library("dplyr")
library("hrbrthemes")
library("scales")
library("ggrepel")
options(scipen = 999)
theme_set(theme_bw())
##2. Inspect the Data Set
data = read.delim("Memeti_Nurdzane_1.csv", sep=";", dec=".",
                header=TRUE, na.strings=c("","NA"))

#check the data for an overview:
str(data)

#T2.1
tib=as_tibble(data)
print(tib)
view(tib)
data2=as.data.frame(tib)
print(data2)

#T2.2 (2p) What are the variables? And T2.3 (2p) What is the type of each variable? 
#select the specific data variables, which I like.
mydata = subset(data, select=c(1:30))

#list the variables in data
names(mydata)
sapply(mydata, class)

#T2.4 (2p) How many observations are in the dataset? 
str(mydata)

#T2.5 Missing Data
a= md.pattern(mydata, plot = TRUE, rotate.names = TRUE)


##4. Clean the Data Set

#4.1data frames of type tibble
tib=as_tibble(mydata)
print(tib)
#view(mydata)
#view(tib)

#4.2 tidy data: rename first column of data, because it was like this "i..Data"
ncol(mydata)
# Check class
class(mydata)
class(mydata[,1])
colnames(mydata)[1]="Date"
#check again the variables
names(mydata)

#4.3. date structure convert class of "character" to "Date" of 1. column
mydata$Date=as.Date(mydata$Date)
#check class of first column
class(mydata$Date)

# Dummy data
# mydata <- data.frame(
#   day = as.Date("2017-06-14") - 0:364,
#   value = runif(365) - seq(-140, 224)^2 / 10000
# )
mydata
#4.3 missing data
#change values "NA" to zero
mydata[is.na(mydata)]=0
#check
print(mydata)
#delete missing data, if existing

#mydata2=na.omit(mydata)
#is.na(mydata2)
#sum(is.na(mydata2))
#md.pattern(mydata2,plot=TRUE)


##5. Apply Descriptive Measures

#T5.1 (2p) Compute the mean, median, variance, minimum, maximum, and quartiles. 
print(mydata$ZH)
mean(mydata$Hospitalized_ZH)
median((mydata$Hospitalized_ZH))
var(mydata$Hospitalized_ZH)
min(mydata$Hospitalized_ZH)
max(mydata$Hospitalized_ZH)
quantile(mydata$Hospitalized_ZH)
mydata2=mydata
box= ggplot(mydata, aes(x = factor (0),Hospitalized_ZH))
box + geom_boxplot(outlier.size = 1.5) + ylab ("COVID-19 hospitalized cases in ZH") 
#delete last row of mydata
mydata2=mydata
mydata2=mydata[-c(271,272,273),]
view(mydata2)
##6. Apply Data Visualisation Towards your Goal
#T6.1 (2p) Construct a basic plot, i.e. a plot with one layer only, and without faceting.


basic= ggplot(mydata2, aes(x=mydata2$Date, y=mydata2$Hospitalized_ZH))+geom_line()
basic

#6.2 Extend the basic plot

#show relation between hospitalized in canton zh and in switzerland
g=ggplot(mydata2, aes(x=mydata2$Date))+
  geom_line(aes(y=mydata2$Hospitalized_ZH, color="Hospitalized in ZH"))+
  geom_line(aes(y=mydata2$Hospitalized_CH, color="Hospitalized in CH"))
plot(g)
#6.3 Aestehtics 
#6.4 Titles
#6.5Themes
g2=g+labs(x="Date", y="Hospitalized",title="Hospitalized in Switzerland during Covid-19")+
  theme(axis.title.y = element_text(size = rel(0.9), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.9)))+
  legend(legend="topleft")+
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "10 day")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) 
g2


b=ggplot(mydata2, aes(x=mydata2$Date))+
  geom_line(aes(y=mydata2$ZH, colour="Anzahl bestätigter Erkrankungsfälle in ZH"))+
  geom_line(aes(y=mydata2$CH, colour="Anzahl bestätigter Erkrankungsfälle in CH"))

b2=b+labs(x="Datum", y="Anzahl bestätigter Erkrankungsfälle",title="Entwicklung der Coronavirus-Epidemie (COVID-19) in der Schweiz")+
  theme(axis.title.y = element_text(size = rel(0.9), angle = 90))+
  theme(axis.title.x = element_text(size = rel(0.9)))+
  theme(
    legend.position = 'top',
  )+
  scale_x_date(labels = date_format("%Y-%m-%d"), date_breaks = "10 day")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) 
b2

b2 + geom_text_repel(aes(label=mydata2$CH), size=2, data=mydata2) + labs(subtitle="With ggrepel::geom_text_repel") + theme(legend.position = "None")


#6.6 Faceting, multiple plots, saving plots

library("gridExtra")
d=grid.arrange(g2,b2, nrow=2)
ggsave("Memeti_Nurdzane_plot.pdf", d)




