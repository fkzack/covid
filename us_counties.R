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
library(fredsUtils)


#covid by county from ny times
#Seems to be limited to 1000 records, so get by state
counties_url <- "https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+order+by+date+desc"
#https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties++where+county+in+(%27Santa+Clara%27%2C+%27Harris%27)+order+by+date+desc

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

getStateUrl <- function(state){
  state_url <- "https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+where+%22state%22+%3D+%3Ap0+order+by+county%2C++date+desc&p0=STATE"
  state_url <- sub("STATE", state, state_url)
  return (state_url)
} 



covidPlot <- function(formula1, data, ...){
  
  #this gets the incoming data frame
  #print(get_all_vars(formula, data=data))
  #print(formula(formula1))
  
  ticksAt <-as.Date(pretty_dates(get_all_vars(formula1, data=data)$date, 3))
  
  p <- xyplot(formula1,  data = data,
              scales=list(y=list(log=10),
                          x=list(rot=45, at=ticksAt)
              ),
            yscale.components = latticeExtra::yscale.components.log10ticks,
            sub=list(label="Data from NY Times via covid-19.datasettes.com   ", cex=0.5, x=1, just="right"),
            par.strip.text=list(cex=0.75),
            type=c('p', 'l'),
            as.table=TRUE,
            panel=function(x,y,...){
                panel.xyplot(x,y,...)
             },
            
              ...)
  return (p)
}

#print(covidPlot(deaths ~ date |county, data=counties, main="main main main", ylab="fdfdfdfdfd"))




plotCounties <- function (countyUrl, title){

  rawJSON <- fromJSON(countyUrl)
  
  counties <- data.frame(rawJSON$rows, stringsAsFactors = F)
  ifelse(counties$state=='New York', "")
  names(counties) <- rawJSON$columns
  counties$date <- as.Date(counties$date)
  counties$deaths <- as.numeric(counties$deaths)
  counties$cases <- as.numeric(counties$cases)
  counties$county <- paste(counties$county, state.abb[match(counties$state, state.name)])
  
  print(str(counties))
  print(addGrid(covidPlot(cases~date | county, data=counties, group=county, main=title )))
  print(addGrid(covidPlot(cases~date, data=counties, group=county, main=title, 
                          auto.key= list(cex=0.6, columns=3), ,
                          par.settings= list(superpose.symbol=list(pch=1:25)) )
                ))
  print(addGrid(covidPlot(deaths~date | county, data=counties, group=county, main=title )))
        
        
 

}

plotCounties(getStateUrl('California'), "California Counties")
plotCounties(getStateUrl('New+York'), "New York Counties")
plotCounties(getSelectedCountiesUrl(), "Selected Counties")

