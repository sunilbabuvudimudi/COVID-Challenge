

#Notes 
#distinct(as.data.frame(measures$iso))

library(dplyr)

#Import data
cases<-read.csv("johns-hopkins-covid-19-daily-dashboard-cases-over-time.csv",na.strings = FALSE)
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

#Simple line graph of caonfirmed cases in USA
ggplot(data=cases[cases$iso3=="USA" & cases$province_state!="",])+
 geom_line(aes(confirmed_posix,y=deaths,color=province_state))

#Pivoting to get measures and category applied per confirmed_posix column
measures<-measures %>% 
  group_by(iso,measures_posix) %>% 
  mutate(measures_derived = paste0(measure, collapse = "-"))

measures<-measures %>% 
  group_by(iso,measures_posix) %>% 
  mutate(category_derived = paste0(category, collapse = "-"))

#select columns from measures dataframe
measures<-measures %>% select(country,iso,region,measures_posix,category_derived,measures_derived)

measures<-measures %>% distinct() # will have pivoted measures

#select columns of cases dataframe
cases<-cases %>% select(country_region,confirmed,deaths,delta_confirmed,incident_rate,iso3,confirmed_posix)
class(measures)

#Join cases and measures
# merge(x=cases,y=measures,by.x=c("iso","confirmed_posix"),by.y=c("iso3","measure_posix"),all.x=TRUE)
# ERROR: Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
# Checked the class of both the tables cases was data frame and measures was- "grouped_df" "tbl_df"     "tbl"        "data.frame"
# changed measures to data frame
measures<-as.data.frame(measures)
merge(x=cases,y=measures,by.x=c("iso","confirmed_posix"),by.y=c("iso3","measure_posix"),all.x=TRUE)



