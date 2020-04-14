#plot cdc all-death data by week
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
library(cdcfluview)
library(reshape2)

#install_github("fkzack/FredsRUtils")
#library(FredsRUtils)

rm(list=ls())





#overall death rates in us
#https://data.cdc.gov/api/views/bi63-dtpu/rows.json?accessType=DOWNLOAD

#corona provisional death counts
#https://data.cdc.gov/resource/hc4f-j6nb.json


#cdc fluView has weekly all deaths, but in a web based interface, package cdcflueview queries this interface
#there is a fairly long log (1 month or more) between actual deaths and data entry/availablility
mort <-  pi_mortality("state")
str(mort)
p_all_deaths <- xyplot(all_deaths ~week_start | region_name, data=mort)
# print(p_all_deaths)
mort$year <- ifelse(mort$weeknumber > 1, year(mort$week_start), year(mort$week_end))
mort$weeknumber <- as.numeric(mort$weeknumber)
all_deaths <- mort[c('year', 'weeknumber', 'region_name', 'all_deaths')]
all_deaths <- dcast(all_deaths, weeknumber+region_name ~ year, value.var = 'all_deaths')
names(all_deaths) <- paste("all_deaths_", names(all_deaths), sep="")
names(all_deaths)[1] <- 'week_number'
names(all_deaths)[2] <- 'state'
all_deaths$key <- ifelse(is.na(all_deaths$all_deaths_2020), "2019/2018 Ratio", "2020/2019 Ratio")
all_deaths$year_on_year <- ifelse(is.na(all_deaths$all_deaths_2020), all_deaths$all_deaths_2019/all_deaths$all_deaths_2018, all_deaths$all_deaths_2020/all_deaths$all_deaths_2019)
str(all_deaths)
p_year_on_year <- xyplot(all_deaths$year_on_year ~ week_number | state, data=all_deaths, group=key, auto.key=TRUE, as.table=TRUE, ylab = " All Deaths Year on Year Ratio")
# print(p_year_on_year)



#cdc is quick-releasing provisional covid data, but it is still lagging by many weeks
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

p_covid_provisional <- xyplot (value~date | key, data=week, scales=list(y=list(relation="free")))





