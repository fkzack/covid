# plot daily county covid data provided by NY Times
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
install_github("fkzack/FredsRUtils")
library(FredsRUtils)

rm(list=ls())



#get county population data from Census
source ("census.r")
countyPopulations <- GetCountyPopulationsNYT()

# an xyplot formatted for this
source("covidPlot.r")


#covid by county from ny times
#Seems to be limited to around 1000 records, so get by state
counties_url <- "https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+order+by+date+desc"


# Build up the encoded url to retireve data for seleced counties from covid-19-datasettes.com
getSelectedCountiesUrl <- function(){
  sql_url <- paste(
    "https://covid-19.datasettes.com/covid.json?sql=select",
    "rowid,date,county,state,fips,cases,deaths from ny_times_us_counties where county in (",
    "'Alameda', 'Contra Costa', 'San Francisco', 'San Mateo', 'Santa Clara', 'Los Angeles', 'New York City', ",
    " 'Harris', 'King'",
    ") and state in ('California', 'New York', 'Texas', 'Washington') order by county, date"
  )
  #urlencode does not work correctly for some reason, so do manually
  sql_url <- gsub(" ", "+", sql_url)
  sql_url <- gsub(",", "%2c", sql_url)
  print (sql_url)
  return(sql_url)
}


# Build url to get county data for a single state
getStateUrl <- function(state){
  state_url <- "https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+where+%22state%22+%3D+%3Ap0+order+by+county%2C++date+desc&p0=STATE"
  state_url <- sub("STATE", state, state_url)
  return (state_url)
} 


# central diference approx to dx/dy, 
# y is a date, we assume we are sorted in date order (ascending)
# group_id identifies series that are in order, do not calculate across changes in group id
centralDifference <- function (x,y, group_id){
  dx <- lead(x,1) - lag(x,1)
  dy <- lead(y,1) - lag(y,1)
  dy <- as.numeric(dy, units='days' )
  slope <- dx/dy
  breaks <- (group_id != lag(group_id,1)) & (lead(group_id,1) == group_id)
  slope <- ifelse(breaks, NA, slope)
  return (slope)
}

#get county data from NYT
getCounties <- function(countyUrl, population_data){

  rawJSON <- fromJSON(countyUrl)
  
  counties <- data.frame(rawJSON$rows, stringsAsFactors = F)
  names(counties) <- rawJSON$columns
  counties$date <- as.Date(counties$date)
  counties$deaths <- as.numeric(counties$deaths)
  counties$cases <- as.numeric(counties$cases)
  counties$county <- paste(counties$county, state.abb[match(counties$state, state.name)])
  counties$fips <- as.numeric(counties$fips)
  print(str(counties))
  
  #special case NYC data since ny times aggregates all five boroughs
  counties$fips <- if_else(counties$county == "New York City NY", 36000, counties$fips)
  
  # add in census population data
  counties <- merge(counties, population_data)
  
  counties <- counties[order( counties$fips, counties$date),]
  counties$death.slope = centralDifference(counties$deaths, counties$date, counties$fips)
  counties$case.slope = centralDifference(counties$cases, counties$date, counties$fips)
  
  
  return(subset(counties, counties$date > as.POSIXct("2020-2-29")))
}

plotCounties <- function (countyPopulations){
    subtitle <- "Data from NY Times via covid-19.datasettes.com"
    
    ca <- getCounties(getStateUrl('California'), countyPopulations)
    ny <- getCounties(getStateUrl('New+York'), countyPopulations)
    selected <- getCounties(getSelectedCountiesUrl(), countyPopulations)
    
    print(covidPlot(cases~date | county, data=ca, group=county, subtitle = subtitle, main="California Counties"))
    
    
    print(covidPlot(100000* cases/county.population ~ date | county, data=ca, group=county, subtitle = subtitle, main="California Counties"))
    print(covidPlot(deaths~date | county, data=ca, group=county, subtitle = subtitle, main="California Counties"))
    
    print(covidPlot(cases~date | county, data=ny, group=county, subtitle = subtitle, main="New York Counties"))
    print(covidPlot(deaths~date | county, data=ny, group=county, subtitle = subtitle, main="New York Counties"))
    
    
    print(covidPlot(cases~date | county, data=selected, group=county, subtitle = subtitle, main="Selected Counties"))
    print(covidPlot(deaths~date | county, data=selected, group=county, subtitle = subtitle, main="Selected Counties"))
    print(covidPlot(case.slope~date | county, data=selected, group=county, subtitle = subtitle, main="Selected Counties", ylab="Slope (Cases/Day)"))
    print(covidPlot(death.slope~date | county, data=selected, group=county, subtitle = subtitle, main="Selected Counties", ylab="Slope (Deaths/Day)"))
    
    print(covidPlot(cases~date, data=selected, group=county, subtitle = subtitle, main="Selected Counties",
                            auto.key= list(cex=0.6, columns=3),
                            par.settings= list(superpose.symbol=list(pch=1:25)) ))
    
    print(covidPlot(deaths~date, data=selected, group=county, subtitle = subtitle, main="Selected Counties",
                            auto.key= list(cex=0.6, columns=3),
                            par.settings= list(superpose.symbol=list(pch=1:25)) ))
}



# s <- getCounties(getSelectedCountiesUrl(), countyPopulations)
# print(covidPlot(case.slope~date | county, data=s, group=county, subtitle = "fdfdfdfd", main="Selected Counties", ylab="Slope (Cases/Day)"))

# plotCounties(countyPopulations)
