

#Notes 
#distinct(as.data.frame(measures$iso))

library(tidyverse)
library(dplyr)


#Import data
cases<-read.csv("johns-hopkins-covid-19-daily-dashboard-cases-over-time.csv",na.strings = FALSE)
cases$iso<-cases$iso3
cases$iso3<-NULL
measures<-read.csv("acaps-covid-19-government-measures-dataset.csv",na.strings=FALSE)
countries<-read.csv("countries.csv",na.strings=FALSE)

#Filter countries in cases and measures
cases<-cases[cases$iso3 %in% as.vector(countries$iso),]
measures<-measures[measures$iso %in% as.vector(countries$iso),]

#Change date to posix format
cases$confirmed_posix<-as.POSIXct(cases$last_update,format="%Y-%m-%d")
measures$measures_posix<-as.POSIXct(measures$date_implemented,format="%Y-%m-%d")

#Checking distinct countries
measures %>% distinct(measures$country, measures$region)
cases%>%distinct(cases$country_region)

#Let us consider Italy, China, USA, United Kingdom,Russia as separate regions(due to size and importance and news)
measures$region<-as.character(measures$region)

measures[measures$iso=="ITA","region"]<-rep("Italy",43)
measures[measures$iso=="USA","region"]<-rep("USA",49)
measures[measures$iso=="CHN","region"]<-rep("China",35)
measures[measures$iso=="GBR","region"]<-rep("United Kingdom",24)
measures[measures$iso=="RUS","region"]<-rep("Russian Federation",18)

countries$region<-as.character(countries$region)
countries[countries$iso=="ITA","region"]<-"Italy"
countries[countries$iso=="USA","region"]<-"USA"
countries[countries$iso=="CHN","region"]<-"China"
countries[countries$iso=="GBR","region"]<-"United Kingdom"
countries[countries$iso=="RUS","region"]<-"Russian Federation"

#1  Simple line graph of confirmed cases in USA
ggplot(data=cases[cases$iso=="USA" & cases$province_state!="",])+
 geom_line(aes(confirmed_posix,y=deaths,color=province_state))

#2  What was the confirmed cases numbers worldwide?
library(dplyr)
library(plyr)

cases_posix_sum<-ddply(cases, .(confirmed_posix), summarise, posix_sum = sum(confirmed))

# Method 1
ggplot(data=ddply(cases, .(confirmed_posix), summarise, posix_sum = sum(confirmed)))+
  geom_line(aes(x=confirmed_posix,y=posix_sum))

# Method2a
#cases_posix_sum<-cases %>% 
#  group_by(confirmed_posix) %>% 
#  mutate(confirmed_sum = sum(confirmed))
#cases_posix_sum<-cases_posix_sum %>% distinct(confirmed_posix,confirmed_sum)
#ggplot(data=cases_posix_sum)+geom_line(aes(x=confirmed_posix,y=confirmed_sum))

# Method 2b
#ggplot(data=(cases %>% 
#  group_by(confirmed_posix) %>% 
#  mutate(confirmed_sum = sum(confirmed)))%>% distinct(confirmed_posix,confirmed_sum))+
#  geom_line(aes(x=confirmed_posix,y=confirmed_sum))


#select columns from measures and cases dataframes
measures<-as.data.frame(measures)
measures<-measures %>% select(country,iso,region,measures_posix,category,measure)
cases<-cases %>% select(country_region,confirmed,deaths,delta_confirmed,incident_rate,iso,confirmed_posix)

#to get region in cases
library(plyr)
cases<-join(cases, countries, by = "iso")

cases$region<-as.character(cases$region)

cases[cases$iso=="ITA","region"]<-rep("Italy",98)
cases[cases$iso=="USA","region"]<-rep("USA",5292)
cases[cases$iso=="CHN","region"]<-rep("China",98)
cases[cases$iso=="GBR","region"]<-rep("United Kingdom",98)
cases[cases$iso=="RUS","region"]<-rep("Russian Federation",98)


#aggregating cases by region and visualizing results
cases_region<-ddply(cases, .(region,confirmed_posix), summarise, posix_sum = sum(confirmed))
ggplot(data=cases_region)+
  geom_line(aes(x=confirmed_posix,y=posix_sum,color=region))+
  facet_grid(region~.,scales="free")+
  ggtitle("regions over time")
#+geom_smooth(aes(x=confirmed_posix,y=posix_sum))

#countries started applying stringent measures 
ggplot(data=measures)+
  geom_boxplot(aes(x=measures_posix,y=region,fill=category),outlier.color=NA)












#join measures and cases
join_measures<-right_join(measures,cases, by = c("iso"="iso","measures_posix"="confirmed_posix"))
join_measures$country<-NULL
join_measures$region<-NULL



ggplot(data=join[!is.na(join$category),])+
  geom_bar(aes(x=measures_posix,fill=category))+
  geom_line(aes(x=measures_posix,y=confirmed),stat="identity")+
  facet_grid(region~.,scales="free")



#as.vector(unlist(as.data.frame(join[,"iso"])))
#as.vector(unlist(as.data.frame(join[,"region"])))
#p<-cbind(as.vector(unlist(as.data.frame(join[,"iso"]))),as.vector(unlist(as.data.frame(join[,"region"]))))
#
#p[,2]
#
#for(i in 1:length(as.vector(unlist(as.data.frame(join[!is.na(join$region),"iso"]) %>% distinct())))){
#  join[,2]
#}








#join measures and x(which has world wide cases)

#3  When did the countries worldwide start to take measures?
#First let us aggregate the measures taken per measures_posix date column

#Pivoting to get measures and category applied per confirmed_posix column
measures<-measures %>% 
  group_by(iso,measures_posix) %>% 
  mutate(measures_derived = paste0(measure, collapse = "-"))

measures<-measures %>% 
  group_by(iso,measures_posix) %>% 
  mutate(category_derived = paste0(category, collapse = "-"))

#3  When did the countries worldwide start to take measures?
ggplot(data=measures)+
  geom_bar(aes(x=measures_posix,fill=region))


#select columns from measures dataframe
measures<-measures %>% select(country,iso,region,measures_posix,category_derived,measures_derived)

measures<-measures %>% distinct() # will have pivoted measures

#select columns of cases dataframe
cases<-cases %>% select(country_region,confirmed,deaths,delta_confirmed,incident_rate,iso3,confirmed_posix)
class(measures)



#Join cases and measures to get all the case and all the 
# merge(x=cases,y=measures,by.x=c("iso","confirmed_posix"),by.y=c("iso3","measure_posix"),all.x=TRUE)
# ERROR: Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
# Checked the class of both the tables cases was data frame and measures was- "grouped_df" "tbl_df"     "tbl"        "data.frame"
# changed measures to data frame
measures<-as.data.frame(measures)
#merge(x=cases,y=measures,by.x=c("iso3","confirmed_posix"),by.y=c("iso","measure_posix"),all.x=TRUE) #Investigate error but continue with left_join function in tidyverse
join<-left_join(cases,measures, by = c("iso3"="iso","confirmed_posix"="measures_posix"))



#Divide a column based on delimiter
join$country<-NULL
p<-within(join, category_derived<-data.frame(do.call('rbind', strsplit(as.character(category_derived), '-', fixed=TRUE))))
