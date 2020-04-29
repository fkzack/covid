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
#install_github("fkzack/FredsRUtils")
#library(FredsRUtils)

rm(list=ls())



#get county population data from Census
source ("census.r")
countyPopulations <- GetCountyPopulationsNYT()

# an xyplot formatted for this data
source("covidPlot.r")


#covid by county from ny times
#Seems to be limited to around 1000 records, so get by state insted
#counties_url <- "https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+order+by+date+desc"


# Build up the encoded url to retireve data for seleced counties in a state from covid-19-datasettes.com
# State is the state name as a string 
# counties is a collection of county names in that state
getSelectedCountiesUrl <- function(state, counties){
  
  options(useFancyQuotes = FALSE)
  counties <- paste(sQuote(counties), collapse = ",")
  
  sql_url <- paste(
    "https://covid-19.datasettes.com/covid.json?sql=select",
    "rowid,date,county,state,fips,cases,deaths from ny_times_us_counties where county in (",
    counties,
    ") and state = ",
    sQuote(state),
    "order by county, date"
  )
  #urlencode does not work correctly for some reason, so do manually
  sql_url <- gsub(" ", "+", sql_url)
  sql_url <- gsub(",", "%2c", sql_url)
  print (sql_url)
  return(sql_url)
}


# central diference approx to dx/dy, 
# y is a date, we assume we are sorted in date order (ascending)
# group_id identifies series that are in order, do not calculate across changes in group id
centralDifference <- function (x,y, group_id){
  dx <- lead(x,1) - lag(x,1)
  dy <- lead(y,1) - lag(y,1)
  dy <- as.numeric(dy, units='days' )
  slope <- dx/dy
  breaks <- (group_id != lag(group_id,1)) | (lead(group_id,1) != group_id)
  slope <- ifelse(breaks, NA, slope)
  return (slope)
}


#combine several selected counties data into a single df
addSelectedCounties <- function(selectedCounties, state, counties, population_data){
  selectedUrl <- getSelectedCountiesUrl(state, counties)
  selected <- getCounties(selectedUrl)
  if (is.null(selectedCounties)){
    selectedCounties <- selected
  }
  else {
    selectedCounties <- rbind(selectedCounties, selected)
  }
  return (selectedCounties)
}

# get data from a group of counties I find interesting
# * population_data is the census population df retrieved above
# * first_day is the first day to include in the returned df
getSelectedCounties <- function(population_data, first_day) {
  #print(paste('getSelectedCounties populationData',population_data ))
  selected <- NULL
  selected <- addSelectedCounties(
    selected, 
    "California",
    c('Alameda','Contra Costa','San Francisco','San Mateo','Santa Clara','Los Angeles', 'Orange'), 
    population_data
    )
  
  selected <- addSelectedCounties(
    selected, 
    "New York",
    c('New York City'),
    population_data
  )
  
  selected <- addSelectedCounties(
    selected, 
    "Texas",
    c('Harris'),
    population_data
  )

  selected <- addSelectedCounties(
    selected,       
    "Washington",
    c('King'),
    population_data
  )
  
  selected <- addSelectedCounties(
    selected,   
    "Michigan",
    c('Wayne', 'Oakland'),
    population_data
  )
  
  selected <- addSelectedCounties(
    selected,   
    "Hawaii",
    c('Kauai', 'Maui'),
    population_data
  )
  
  # add in census population data
  selected <- merge(selected, population_data)
  
  selected <- subset(selected, selected$date >= first_day)
  
  #sort and calculate slopes
  selected <- selected[order( selected$fips, selected$date),]
  selected$death.slope = centralDifference(selected$deaths, selected$date, selected$fips)
  selected$case.slope = centralDifference(selected$cases, selected$date, selected$fips)
  selected <-  selected[order(selected$county.name, selected$date),]

  return(selected)
}
  

# break county data retrieval into smappler chunks to avoid download limits
# * state is the state we are pulling data for 
# * get data for first_day and all later days
# * get data in chuncks of chunk_days
# * population_data is the previously loaded population by county df downloaded abovel
getCountiesByChunk <- function (state, first_day, chunk_days, population_data){
  # 
  # state<-'California'
  # first_day <- ISOdate(2020,4,1)
  # chunk_days <- 5
  # population_data <- GetCountyPopulationsNYT()
  
  start_days <- seq(as.Date(first_day),Sys.Date() ,by= as.difftime(days(chunk_days)))
  start_days <- c(start_days, start_days[length(start_days)] + days(chunk_days))
  counties <- NULL
  base_url <- 'https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+where+%22date%22+%3E%3D+%3Ap0+and+%22date%22+%3C+%3Ap1+and+%22state%22+%3D+%3Ap2+order+by+county%2C+date+desc'
  for (i in seq(1, length(start_days)-1)){
    chunk_url <- paste(base_url,"&p0=",start_days[i], "&p1=", start_days[i+1], "&p2=", state, sep="")
    print(paste("chunk url:", chunk_url))
    c <- getCounties(chunk_url )
    if (is.null(counties)){
      counties <- c
    } else {
      counties <- rbind(counties,c)
    }
  }
  
  # add in census population data
  counties <- merge(counties, population_data)
  
  #sort and calculate slopes
  counties <- counties[order( counties$fips, counties$date),]
  counties$death.slope = centralDifference(counties$deaths, counties$date, counties$fips)
  counties$case.slope = centralDifference(counties$cases, counties$date, counties$fips)
  counties <-  counties[order(counties$county.name, counties$date),]
  return (counties)
}
  

#get county data from NYT
#countyUrl is the url to retrieve json data for the desired counties and days
getCounties <- function(countyUrl){

  rawJSON <- fromJSON(countyUrl)
  
  counties <- data.frame(rawJSON$rows, stringsAsFactors = F)
  if (length(counties) < 1){
    return (NULL)
  }
  names(counties) <- rawJSON$columns
  counties$date <- as.Date(counties$date)
  counties$deaths <- as.numeric(counties$deaths)
  counties$cases <- as.numeric(counties$cases)
  counties$county <- paste(counties$county, state.abb[match(counties$state, state.name)])
  counties$fips <- as.numeric(counties$fips)
  
  #special case NYC data since ny times aggregates all five boroughs
  #36000 is just a made up fips that seems not to be used anywhere else
  counties$fips <- if_else(counties$county == "New York City NY", 36000, counties$fips)
  
  #print(str(counties))
  return(counties)
}

#print the plots for a single county
#inputs: countyData, a df containing the data to plot
#        title, the main title for the plots
plotCounty <- function(countyData, title, subtitle){
  
  
  print(covidPlot(cases~date | county, data=countyData, group=county,  subtitle=subtitle, main=title))
  print(covidPlot(100000*cases/county.population~date | county, data=countyData, group=county, subtitle=subtitle, 
                  ylab="Cases per 100,000", main=title))
  
  print(covidPlot(deaths~date | county, data=countyData, group=county, subtitle=subtitle, main=title))
  
  print(covidPlot(deaths~date | county, data=countyData, group=county, subtitle=subtitle, main=title, logY = 16))
  
  print(covidPlot(100000*deaths/county.population~date | county, data=countyData, group=county, subtitle=subtitle, 
                  ylab="Deaths per 100,000", main=title))
  
  
  print(symmetricPlot(death.slope~date | county, data=countyData, group=county, 
                      subtitle = subtitle, main=title, 
                      ylab="Slope (Deaths/Day)",
                      xlab="Date"))
  
  print(symmetricPlot(100000*death.slope/county.population~date | county, data=countyData, group=county, 
                      subtitle = subtitle, main=title, 
                      ylab="Slope (Deaths/Day/100,000)",
                      xlab="Date"))
}


# download and plot all the county data
# *  countyPopulations is the previously downloaded census population data
plotCounties <- function (countyPopulations){
    subtitle <- "Data from NY Times via covid-19.datasettes.com"
    
    first_day <- ISOdate(2020,3,1, tz="")
    
    selected <- getSelectedCounties(countyPopulations, first_day)
    plotCounty(selected, "Selected Counties", subtitle)
    
    ca <- getCountiesByChunk ('California', first_day, 5,  countyPopulations)
    plotCounty(ca, "California Counties", subtitle)
    
    ny <- getCountiesByChunk ('New+York', first_day, 5,  countyPopulations)
    plotCounty(ny, "New Yourk Counties", subtitle)
    
    hi <- getCountiesByChunk ('Hawaii', first_day, 5,  countyPopulations)
    plotCounty(hi, "Hawaii Counties", subtitle)
    
    tx <- getCountiesByChunk ('Texas', first_day, 5,  countyPopulations)
    plotCounty(tx, "Texas Counties", subtitle)
    
    ga <- getCountiesByChunk ('Georgia', first_day, 5,  countyPopulations)
    plotCounty(ga, "Georgia Counties", subtitle)
    
    fl <- getCountiesByChunk ('Florida', first_day, 5,  countyPopulations)
    plotCounty(fl, "Florida Counties", subtitle)
    
}



 # s <- getSelectedCounties(countyPopulations, ISOdate(2020, 3,1, tz=""))
 # 
 # 
 # print(symmetricPlot(death.slope~date | county, data=s, group=s$county, 
 #                     subtitle = "sub title", main="California Counties", 
 #                     ylab="Slope (Deaths/Day)",
 #                     xlab="Date"))
 
 
 # p <-covidPlot(abs(death.slope)~date | county, data=s, group=sign(death.slope), auto.key=list(text=c("Decreasing", "Zero", "Increasing")),
 #               numTickIntervals = 3,
 #               subtitle = "fdfdfdfd", main="Selected Counties",
 #               ylab="Slope (Deaths/Day)")
 # 
 
 # p <-covidPlot(death.slope~date | county, data=s, group=county, 
 #                numTickIntervals = 3,
 #                subtitle = "fdfdfdfd", main="Selected Counties",
 #                ylab="Slope (Deaths/Day)")
 # print(p)
 # 
 # str(s)
 # s2 <- s
 # s2$death.slope <- ifelse(s2$fips==6013, -s2$death.slope, s2$death.slope)
 # p2 <-symmetricPlot(death.slope~date | county, groupVector=s$county,  data=s2, 
 #               numTickIntervals = 3,
 #               subtitle = "fdfdfdfd", main="Selected Counties",
 #               ylab="Slope (Deaths/Day)")
 # print(p2)
 
# plotCounties(countyPopulations)
