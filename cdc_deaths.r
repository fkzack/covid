# Plot  CDC death certificate data for deaths from all causes
library (jsonlite)
library(datasets)
library(tidyverse)
library(dplyr)
library(lattice)
library(latticeExtra)
library(plotly)
library(Hmisc)
library(RColorBrewer)
library(scales)
library(devtools)
library(lubridate)

library(devtools)
install_github("fkzack/FredsRUtils", type="source")
# install.packages('D:/Data/R/FredsRUtils_0.1.0.tar.gz', repos=NULL, type="source" )
library(FredsRUtils)

library(cdcfluview)

rm(list=ls())

source("covidPlot.r")



#cdc fluView has weekly all deaths, but in a web based interface, package cdcflueview queries this interface
#there is a fairly long lag (several weeks to more than one month) between actual deaths and data entry/availablility
mort <-  pi_mortality("state")
str(mort)
p_all_deaths <- xyplot(all_deaths ~week_start | region_name, data=mort)
# print(p_all_deaths)
mort$year <- ifelse(mort$weeknumber > 1, year(mort$week_start), year(mort$week_end))
mort$weeknumber <- as.numeric(mort$weeknumber)

all_deaths <- mort[c('year', 'weeknumber', 'week_start', 'region_name', 'all_deaths')]
names(all_deaths)[2] = "week_number"
names(all_deaths)[4] = "state"

all_deaths <- pivot_wider(all_deaths,id_cols=c('week_number', 'state', 'all_deaths'), names_from='year', values_from = c('all_deaths', 'week_start'))

all_deaths$year_on_year <- ifelse(is.na(all_deaths$all_deaths_2020), all_deaths$all_deaths_2019/all_deaths$all_deaths_2018, all_deaths$all_deaths_2020/all_deaths$all_deaths_2019)
all_deaths$all_deaths <- ifelse(is.na(all_deaths$all_deaths_2020), all_deaths$all_deaths_2019, all_deaths$all_deaths_2020)
all_deaths$week_starts <- ifelse(is.na(all_deaths$all_deaths_2020), all_deaths$week_start_2019, all_deaths$week_start_2020)
all_deaths$week_starts <-  as.Date(all_deaths$week_starts, origin = lubridate::origin)

str(all_deaths)

subtext <- "Data from CDC FluView https://gis.cdc.gov/grasp/fluview/mortality.html"
#combine 2019 and 2020 data into a rolling year
all_deaths_recentkey <- ifelse(is.na(all_deaths$all_deaths_2020), "2019 Deaths", "2020 Deaths")
p_all_deaths_recent <- covidPlot(all_deaths ~ week_starts | state, data=all_deaths, group=all_deaths_recentkey, 
                                 numTickIntervalsX = 5,
                                 auto.key=TRUE, 
                                 xlab="Date", 
                                 ylab="All Deaths", 
                                 subtitle = subtext,
                                 main="Weekly Deaths From All Causes")
  

#compare death rate this rolling year to same week previous year
all_deaths_year_on_year_key <- ifelse(is.na(all_deaths$all_deaths_2020), "2019/2018 Ratio", "2020/2019 Ratio")
p_year_on_year <- covidPlot(year_on_year ~ week_starts | state, data=all_deaths, group=all_deaths_year_on_year_key, 
                            numTickIntervalsX = 5,
                            logX = FALSE, logY = FALSE,
                            auto.key=TRUE, 
                            xlab="Date", 
                            ylab="All Deaths Year on Year Ratio" , 
                            subtitle = subtext,
                            main="Weekly Deaths From All Causes")




#cdc is quick-releasing provisional covid data, but it is still lagging by many weeks
#have not yeat merged this in with longer term data because interpretation is unclear
corona <- fromJSON("https://data.cdc.gov/resource/hc4f-j6nb.json")
str(corona)
week <- subset(corona, corona$group =="By week")
week$date <- as.POSIXct(week$indicator, format= "%m/%d/%Y", tz="")
week$covid_deaths <- as.numeric(week$covid_deaths)
week$total_deaths <- as.numeric(week$total_deaths)
week$percent_expected_deaths <- as.numeric(week$percent_expected_deaths)
week$pneumonia_deaths <- as.numeric(week$pneumonia_and_covid_deaths)
week$pneumonia_and_covid_deaths <- as.numeric(week$pneumonia_and_covid_deaths)
week$all_influenza_deaths_j09_j11 <- as.numeric(week$all_influenza_deaths_j09_j11)
week <- gather(week, key, value, covid_deaths:all_influenza_deaths_j09_j11)
week <- subset(week, !is.na(date))

#p_covid_provisional <- xyplot (value~date | key, data=week, scales=list(y=list(relation="free")))


testDeaths <- function(){
  print (p_all_deaths_recent)
  print (p_year_on_year)
}
# testDeaths()

