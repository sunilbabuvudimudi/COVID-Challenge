###################################PROJECT4 - COVID and Worldwide Measures#############################################
#Ref: Dataset: https://www.kaggle.com/roche-data-science-coalition/uncover

#Task: Analyzing worldwide measures implementations in tackling COVID
#Import data

measures <- read.csv("covid_measures.csv", stringsAsFactors=FALSE)
cases <- read.csv("covid_cases.csv", stringsAsFactors=FALSE)
countries<-read.csv("covid_countries.csv",stringsAsFactors = FALSE)
#Change countries to vector
country_vector<-as.vector(unlist(countries$country))
rm(countries)

nrow(measures)
nrow(cases)

str(measures)
str(cases)

#Change Korea DPR to North Korea in measures
#change Korea Republic of to South Korea in measures
#Change United states of America to US in measures
#Change Viet nam to Vietnam in measures

measures[measures$country=="Korea DPR","country"]<-rep("North Korea",4)
measures[measures$country=="Korea Republic of","country"]<-rep("South Korea",18)
measures[measures$country=="United States of America","country"]<-rep("US",49)


#Filter out the countries not in countries dataframe
cases<-cases[cases$Country.Region %in% country_vector,]
measures<-measures[measures$country %in% country_vector,]


#Delete rows where implemented date (in covid_measures) is blank)
measures<-measures[measures$date_implemented!="",]

#Change format of dates to PosixCt
measures$measure_posixtime<-as.POSIXct(measures$date_implemented,format="%Y-%m-%d") #2020-03-01
cases$confirmed_posixtime<-as.POSIXct(cases$Date,format="%m/%d/%Y") #1/22/2020

#Deleting data after April 4th in covid_measures
measures<-measures[measures$measure_posixtime<"2020-04-05",]

#Let us assume US and China as a region in itself
measures[measures$country=="US","region"]<-rep("US",48)
measures[measures$country=="China","region"]<-rep("China",25)

library(dplyr)
#Checking unique countries
measures %>% distinct(measures$country, measures$region)
cases %>% distinct(cases$Country.Region, cases$`measures$region`)

#join with cases and measures to get region in cases
cases<-merge(cases,measures %>% distinct(measures$country, measures$region),by.x=c("Country.Region"),by.y=c("measures$country"),all.x=TRUE)

cases<-cases[!is.na(cases$`measures$region`),] #Deleting from cases where region is NA because of no implementation date

#Renaming measures$region to region in cases
cases$region<-"a"
cases$region<-cases$`measures$region`
cases$`measures$region`<-NULL

#Region wise confirmed/deaths
library(scales)
ggplot(data=cases)+
  geom_line(aes(x=confirmed_posixtime,y=Deaths,color=region))+
  facet_grid(region~.,scales="free")

#Join cases and measures
cases_measures<-merge(cases,measures,by.x=c("Country.Region","confirmed_posixtime"),by.y=c("country","measure_posixtime"),all.x=TRUE)

#measures vs confirmed cases worldwide
p<-ggplot(data=cases_measures[!is.na(cases_measures$category),])
p+geom_bar(aes(x=confirmed_posixtime,fill=category))+
  scale_x_datetime(labels = date_format("%b"))+
  facet_grid(region.x~.,scales="free")
head(cases_measures[cases_measures$Country.Region=="China",],200)

#doubleYScale(measures_across_countries, cases_across_countries, add.ylab2 = TRUE)

#plot(1:10)
#par(new=TRUE)
#plot(1:10, rnorm(10), xaxt="n", yaxt="n", xlab="", ylab="", type="l")
#axis(side=4)