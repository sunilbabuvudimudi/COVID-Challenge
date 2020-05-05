#################PROJECT1 COVID ANALYSIS##############################################
#Ref:Dataset: https://www.kaggle.com/roche-data-science-coalition/uncover
# Time series visualizations for covid: cases and deaths
# dataset taken from Kaggle: Covid 2019\UNCOVER\USAFacts

#import covid data

covidcases<- read.csv("confirmed_covid_cases_apr05.csv",na.strings=c(""))
coviddeaths<- read.csv("confirmed_covid_deaths_apr05.csv",na.strings=c(""))
str(covidcases)
str(coviddeaths)
head(covidcases,5)
head(coviddeaths,5)

#Data preparation
covidcases$PosixTime<-as.POSIXct(covidcases$date,format="%Y-%m-%d")
coviddeaths$PosixTime<-as.POSIXct(coviddeaths$date,format="%Y-%m-%d")

# covid analysis in 4 states NY, AZ, CA, IL
covidcases_NY_AZ_IL_CA<-covidcases[covidcases$state_name==c("CA","NY","IL","AZ"),]
coviddeaths_NY_AZ_IL_CA<-coviddeaths[coviddeaths$state_name==c("CA","NY","IL","AZ"),]


#Time series of covid cases in 4 states
p<-ggplot(data=covidcases_NY_AZ_IL_CA)
q<-ggplot(data=coviddeaths_NY_AZ_IL_CA)

p+geom_line(aes(x=PosixTime,y=confirmed,color=state_name),size=1.2)+
  facet_grid(state_name~.)
#  geom_hline(yintercept=0.90, color="Gray", size=1.2,linetype=3)
#par(new=T)


q+geom_line(aes(x=PosixTime,y=deaths,color=state_name),size=1.2)+
  facet_grid(state_name~.)

#merging cases and deaths by date
covid_cases_deaths_merged<-merge(covidcases_NY_AZ_IL_CA,coviddeaths_NY_AZ_IL_CA
                                 ,by.x=c("state_name","county_fips", "PosixTime"),by.y=c("state_name","county_fips", "PosixTime"))


head(covidcases_NY_AZ_IL_CA)
head(coviddeaths_NY_AZ_IL_CA)
head(covid_cases_deaths_merged)
#Remove unwanted columns
covid_cases_deaths_merged$county_name.y<-NULL
covid_cases_deaths_merged$state_fips.y<-NULL
covid_cases_deaths_merged$date.y<-NULL
covid_cases_deaths_merged$lat.y<-NULL
covid_cases_deaths_merged$long.y<-NULL
covid_cases_deaths_merged$geometry.y<-NULL
#Rename columns
names(covid_cases_deaths_merged)<-c("state_name","county_fips","PosixTime","county_name","state_fips","date","confirmed","lat","long","geometry","deaths")
head(covid_cases_deaths_merged)

x<-ggplot(data=covid_cases_deaths_merged[covid_cases_deaths_merged$PosixTime>"2020-03-15",])
x+geom_point(aes(x=PosixTime,y=confirmed,color=state_name),size=1.2)+
  geom_smooth(aes(x=PosixTime,y=confirmed),fill=NA)+
  geom_hline(yintercept=10000, color="Gray", size=1.2,linetype=3)
#+coord_cartesian(xlim = c("2020-03-15", "2020-04-05"))
#facet_grid(state_name~.)

#combining confirmed cases as points and deaths as line
x+geom_point(aes(x=PosixTime,y=confirmed,color=state_name),size=2.5) +
  geom_line(aes(PosixTime,deaths),size=1)


#Plotting Percentage of deaths
x+geom_line(aes(x=PosixTime,y=deaths/confirmed,color=state_name))

x+geom_point(aes(x=PosixTime,y=deaths/confirmed,color=state_name),size=1.2)+
  facet_grid(state_name~.)+
  # geom_smooth(fill=NA)+
  ylim(0,0.5)  # But this plot is not best version because the density of population in NY and California is high



