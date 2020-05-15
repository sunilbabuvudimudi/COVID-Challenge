#import data
country_cum<-read.csv("nation_level_daily.csv",na.strings = FALSE)
state_cum<-read.csv("complete.csv",na.strings = FALSE)
dist_total<-read.csv("district_level_latest.csv",na.strings=FALSE)
state_total<-read.csv("state_level_latest.csv",na.strings=FALSE)
statecodes<-state_total %>% select(state,statecode) #package dplyr

#change date to posix

str(country_cum)
library(lubridate)
country_cum$date<-as.character(country_cum$date)
country_cum$posix<-dmy(paste(country_cum$date," ","2020"))
country_cum<-country_cum[country_cum$posix>="2020-02-01" & country_cum$posix<="2020-05-14",]          


str(state_cum)
state_cum$Date<-as.character(state_cum$Date)
state_cum$posix<-as.POSIXct(state_cum$Date ,format="%Y-%m-%d")
state_cum<-state_cum[state_cum$posix>="2020-02-01" & state_cum$posix<="2020-05-14",]  

state_total<-state_total[state_total$statecode!="TT",]
statecodes<-statecodes[statecodes$statecode!="TT",]

#cases in India
ggplot(data=country_cum)+
  geom_point(aes(x=posix,y=totalconfirmed))

#State wise
str(state_total)
#Method 1
library(forcats)
state_total %>%
  mutate(name = fct_reorder(state,confirmed)) %>%
  ggplot(aes(x = reorder(state, -confirmed), y=confirmed))+
  coord_flip()+
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4,position="stack")+
  theme_bw()

#Method 2
library(reshape2)
ggplot(data=state_total, aes(y = reorder(state, desc(-confirmed)), x=confirmed,fill=deaths/confirmed)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("confirmed cases")+
  ylab("states")
#In WestBengal followed by Gujarat and Madhya Pradesh, the percentage of deaths is higher (7% and 5%)


#Visualizing top 10 states with confirmed cases
state_top<-head(state_total[order(-state_total$confirmed),],10)
ggplot(data=state_top, aes(y = reorder(state, desc(-confirmed)), x=confirmed,fill=deaths/confirmed)) + 
  geom_bar(position="stack", stat="identity")+
  xlab("confirmed cases")+
  ylab("states")


str(state_cum_top)
#Visualizing cumulative increase in top 10 states
state_cum_top<-state_cum[state_cum$Name.of.State...UT %in% as.vector(state_top$state),]
ggplot(data=state_cum_top)+
  geom_point(aes(x=posix,y=Total.Confirmed.cases,color=Name.of.State...UT ))
#First national lockdown on April 
ggplot(data=state_cum_top[state_cum_top$posix>"2020-03-24",])+
  geom_point(aes(x=posix,y=Total.Confirmed.cases,color=Name.of.State...UT ))
rm(state_cum_top_lockdown)

#Let check if the number of new cases everyday has stabilized:lag
library(dplyr)
#To get a lag
#state_cum_top<-state_cum_top %>%
#  group_by(Name.of.State...UT ) %>%
#  mutate(lag_confirmed = dplyr::lag(Total.Confirmed.cases, n = 1, default = NA))

#state_cum_top<-state_cum_top %>%
#  group_by(Name.of.State...UT ,posix) %>%
#  arrange(posix) %>%
#  mutate(lag_confirmed = Total.Confirmed.cases - lag(Total.Confirmed.cases, default = 0))

#Taking date for top 10 states from March 24th
state_cum_top_lockdown<-state_cum_top[state_cum_top$posix>"2020-03-24",]
state_cum_top_lockdown$lag_confirmed <- ave(state_cum_top_lockdown$Total.Confirmed.cases..Indian.National., c(state_cum_top_lockdown$Name.of.State...UT,state_cum_top_lockdown$posix), FUN=function(x) c(0, diff(x)))
state_cum_top_lockdown[state_cum_top_lockdown$lag_confirmed<0,"lag_confirmed"]<-0


state_cum_top_lockdown[state_cum_top_lockdown$lag_confirmed<0,]
ggplot(data=state_cum_top_lockdown)+
  geom_line(aes(x=posix,y=lag_confirmed,color=Name.of.State...UT ))+
#  scale_y_continuous(limits = c(0,75))
  facet_grid(Name.of.State...UT ~.,scales="free")

#Numbers say that lockdown helped to contain confirmed cases - Maximum of 50 cases
#on day to day basis in Maharashtra
#Is that a conclusive statement? Are the people catching Covid but not getting tested because 
#of lockdown?



