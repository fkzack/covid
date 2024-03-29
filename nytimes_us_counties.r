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

source("centraldifference.r")



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
  #print (sql_url)
  return(sql_url)
}


# getYesterdaysValues <- function(){
#   #  https://covid-19.datasettes.com/covid.json?sql=select+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+where+%22date%22+%3D+%3Ap0+order+by+fips&p0=2020-06-29
#   
#   #https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+where+%22date%22+%3D+%3Ap0+and+%22fips%22+%3E+%3Ap1+and+%22fips%22+%3C%3D+%3Ap2+order+by+date+desc+limit+101&p0=2020-06-29&p1=0&p2=2000
#   
#   # https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+where+%22date%22+%3D+%3Ap0+and+%22fips%22+%3E+%3Ap1+and+%22fips%22+%3C%3D+%3Ap2+order+by+fips+desc+&p0=2020-06-29&p1=50000&p2=60000
#   
#   #select rowid, date, county, state, fips, cases, deaths from ny_times_us_counties where "date" = :p0 and "fips" > :p1 and "fips" <= :p2 order by fips desc 
#   base_url <- "https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+where+%22date%22+%3D+%3Ap0+and+%22fips%22+%3E+%3Ap1+and+%22fips%22+%3C+%3Ap2+order+by+fips+asc"
#                    
#   
#   fipsStep <- 10000
#   votes <- NULL
#   for (startFips in seq(0, 50000, fipsStep)){
#     sql_url <- paste(base_url,
#                      "&p0=", Sys.Date()-1,
#                      "&p1=", startFips,
#                      "&p2=", startFips + fipsStep,
#                      sep="")
#     v <- getCounties(sql_url)
#     if(is.null(votes)){
#       votes <- v
#     } else {
#       votes <- rbind(votes,v)
#     }
#   }
#   
#   return(votes)
#   }
    
  # counties <- NULL
  # base_url <- 'https://covid-19.datasettes.com/covid.json?sql=select+rowid%2C+date%2C+county%2C+state%2C+fips%2C+cases%2C+deaths+from+ny_times_us_counties+where+%22date%22+%3E%3D+%3Ap0+and+%22date%22+%3C+%3Ap1+and+%22state%22+%3D+%3Ap2+order+by+county%2C+date+desc'
  # for (i in seq(1, length(start_days)-1)){
  #   chunk_url <- paste(base_url,"&p0=",start_days[i], "&p1=", start_days[i+1], "&p2=", state, sep="")
  #   #print(paste("chunk url:", chunk_url))
  #   c <- getCounties(chunk_url )
  #   if (is.null(counties)){
  #     counties <- c
  #   } else {
  #     counties <- rbind(counties,c)
  #   }
  # }
    
                   
                   
  
  #chunk_url <- paste(base_url,"&p0=",start_days[i], "&p1=", start_days[i+1], "&p2=", state, sep="")
  #counties <- getCounties(sql_url)

  
#combine several selected counties data into a single df
addSelectedCounties <- function(selectedCounties, state, counties, population_data){
  
  
  for (county in counties){
    selectedUrl <- getSelectedCountiesUrl(state, c(county))
    selected <- getCounties(selectedUrl)
    if (is.null(selectedCounties)){
      selectedCounties <- selected
    }
    else {
      selectedCounties <- rbind(selectedCounties, selected)
    }  
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
  selected$death.slope = ifelse(selected$death.slope < 0, NA, selected$death.slope)
  selected$case.slope = centralDifference(selected$cases, selected$date, selected$fips)
  selected$case.slope = ifelse(selected$case.slope < 0, NA, selected$case.slope)
  selected <-  selected[order(selected$county.name, selected$date),]

  return(selected)
}
  

t# break county data retrieval into smappler chunks to avoid download limits
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
    #print(paste("chunk url:", chunk_url))
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
  counties$death.slope <- centralDifference(counties$deaths, counties$date, counties$fips)
  counties$death.slope <- ifelse(counties$death.slope < 0, NA, counties$death.slope)
  counties$case.slope <- centralDifference(counties$cases, counties$date, counties$fips)
  counties$case.slope <- ifelse(counties$case.slope < 0, NA, counties$case.slope)
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

#hide loess errors when not enough data is available to create line with given params
#unfortunately this also hides other warnings, so may need to disable if things are weird
ignore <- function(e){
  #do nothing
}
lattice.options(panel.error=ignore)

plotCountySummary <- function(countyData, title, subtitle){
  
 if (length(unique(countyData$state)) < 2) {
   countyData$plotGroup <- countyData$county
   } else {
    countyData$plotGroup <- countyData$state
   }
 
  
  print(covidPlot(100000*deaths/county.population~date | county, data=countyData, group=plotGroup, subtitle=subtitle, 
                  ylab="Deaths per 100,000", main=title, logY = 16))
  
  month_ago = Sys.Date() - months(1)
  
  print(covidPlot(100000*deaths/county.population~date | county, data=subset(countyData, countyData$date > month_ago) , group=plotGroup, subtitle=subtitle, 
                  ylab="Deaths per 100,000", main=title, logY = 16))
  
  
  
  
  
}

countySummaryTest <- function(){
  subtitle <- paste("Data from NY Times via covid-19.datasettes.com on", Sys.Date());
  first_day <- ISOdate(2020,3,1, tz="")
  
  countyData <- getSelectedCounties(countyPopulations, first_day)
  title = "Selected Counties"
  plotCountySummary(countyData,title, subtitle)
}
#countySummaryTest()



plotCountyDetails <- function(countyData, title, subtitle){
  
  if (length(unique(countyData$state)) < 2) {
    countyData$plotGroup <- countyData$county
  } else {
    countyData$plotGroup <- countyData$state
  }
  
  
  #  print(symmetricPlot(100000*case.slope/county.population~date | county, 
  #                      data=countyData, group=plotGroup, 
  #                      type = c("p", "smooth"),
  #                      span=0.2,
  #                      subtitle = subtitle, main=title, 
  #                      ylab="Slope (New Cases/Day/100,000)",
  #                      xlab="Date"))  
  
  # print(symmetricPlot(100000*death.slope/county.population~date | county, data=countyData, group=plotGroup, 
  #                      type = c("p", "smooth"),
  #                     span=0.2,
  #                      panel.error=ignore,
  #                     subtitle = subtitle, main=title, 
  #                      ylab="Slope (Deaths/Day/100,000)",
  #                      xlab="Date"))
  
  
  print(covidPlot(cases~date | county, data=countyData, group=plotGroup,  subtitle=subtitle, main=title, logY = 16))
  
  print(covidPlot(cases~date | county, data=countyData, group=plotGroup,  subtitle=subtitle, main=title), logY=10)
  
  print(covidPlot(100000*cases/county.population~date | county, data=countyData, group=plotGroup, subtitle=subtitle, 
                  ylab="Cases per 100,000", main=title))
  
    print(covidPlot(100000*cases/county.population~date | county, data=countyData, group=plotGroup, subtitle=subtitle, 
                    ylab="Cases per 100,000", main=title, logY=16))
  
  print(symmetricPlot(case.slope~date | county, data=countyData, group=plotGroup, 
                      type = c("p", "smooth"),
                      span=0.2,
                      subtitle = subtitle, main=title, 
                      ylab="Slope (New Cases/Day)",
                      xlab="Date"))
  
    
  print(covidPlot(deaths~date | county, data=countyData, group=plotGroup, subtitle=subtitle, main=title))
  
  print(covidPlot(100000*deaths/county.population~date | county, data=countyData, group=plotGroup, subtitle=subtitle, 
                  logY=FALSE, ylab="Deaths per 100,000", main=title))
  
  print(covidPlot(100000*deaths/county.population~date | county, data=countyData, group=plotGroup, subtitle=subtitle, 
                  ylab="Deaths per 100,000", main=title))
  
  print(symmetricPlot(death.slope~date | county, data=countyData, group=plotGroup, 
                      type = c("p", "smooth"),
                      span=0.2,
                      subtitle = subtitle, main=title, 
                      ylab="Slope (Deaths/Day)",
                      xlab="Date"))
  
  print(symmetricPlot(100000*death.slope/county.population~date | county, data=countyData, group=plotGroup, 
                      type = c("p", "smooth"),
                      span=0.2,
                      subtitle = subtitle, main=title, 
                      ylab="Slope (Deaths/Day/100,000)",
                      xlab="Date"))
}


# # download and plot all the county data
# # *  countyPopulations is the previously downloaded census population data
# plotCounties <- function (countyPopulations){
#     subtitle <- "Data from NY Times via covid-19.datasettes.com"
#     
#     first_day <- ISOdate(2020,3,1, tz="")
#     
#     selected <- getSelectedCounties(countyPopulations, first_day)
#     plotCounty(selected, "Selected Counties", subtitle)
#     
#     ca <- getCountiesByChunk ('California', first_day, 5,  countyPopulations)
#     plotCounty(ca, "California Counties", subtitle)
#     
#     ny <- getCountiesByChunk ('New+York', first_day, 5,  countyPopulations)
#     plotCounty(ny, "New Yourk Counties", subtitle)
#     
#     hi <- getCountiesByChunk ('Hawaii', first_day, 5,  countyPopulations)
#     plotCounty(hi, "Hawaii Counties", subtitle)
#     
#     tx <- getCountiesByChunk ('Texas', first_day, 5,  countyPopulations)
#     plotCounty(tx, "Texas Counties", subtitle)
#     
#     ga <- getCountiesByChunk ('Georgia', first_day, 5,  countyPopulations)
#     plotCounty(ga, "Georgia Counties", subtitle)
#     
#     fl <- getCountiesByChunk ('Florida', first_day, 5,  countyPopulations)
#     plotCounty(fl, "Florida Counties", subtitle)
#     
# }


# #plot summary data in the current rmd and detail data in a detail rmd
# #county data is the data to plot
# #state is the name of the state or other group we are plotting(e.g. 'California' or 'Selected' )
# #returns the name of the output file used
# plotDataForOneState<- function(countyData, title, subtitle){
#   plotCountySummary(countyData,title, subtitle)
#   
#   
#   out_file_name <- gsub(' ', '_', title)
#   cat(paste("[", state, " Details](", out_file_name, ")\n", sep=""))
#   rmarkdown::render("countyPlots.rmd", output_file=out_file_name)
#   return(paste(out_file_name, ".md", sep=""))
#   
# }


#read data  and generate plots
#countyPopulation is the census data loaded above
#first_day is the first day to include in the plots
#returns links to all of the detail plots since we need to print them from rmd
CreateCountyPlots <- function(countyPopulations, first_day){
  subtitle <- paste("Data from NY Times via covid-19.datasettes.com on", Sys.Date());
  
  #plot selected summary in main rmd
  #browser()
  t <- system.time( countyData <- getSelectedCounties(countyPopulations, first_day))
  print(paste("*** get selected times ", t, "\n"))
  
  
  title = "Selected Counties"
  t <- system.time(plotCountySummary(countyData, title, subtitle))
  print(paste("*** plot selected ", t, "\n"))
  
  
  #plot selected and details to sub rmd
  #plot
  out_file_name <- gsub(' ', '_', title)
  t <- system.time(rmarkdown::render("countyPlots.rmd", output_file=out_file_name))
  print(paste("*** render county plots  ", t, "\n"))
  
  #create link to detail
  links <- c(paste("[", title, " Details](", out_file_name, ".html)\n", sep=""))
  
  states_to_plot <- c("California", "New York", "Michigan", "Hawaii", "Texas", "Georgia", "Florida")
  for(state in states_to_plot){
    title = paste(state, "Counties")
    countyData <- getCountiesByChunk (gsub(' ', '+', state), first_day, 5,  countyPopulations)
    out_file_name <- gsub(' ', '_', title)
    rmarkdown::render("countyPlots.rmd", output_file=out_file_name)
    
    #create link to detail
    links <- c(links, paste("[", title, " Details](", out_file_name, ".html)\n", sep=""))
    
  }
  
  return (links)
}

testCountyPlots <- function(){
  first_day <- ISOdate(2020,3,1, tz="")
  CreateCountyPlots(countyPopulations, first_day)
  
}

plotTest <- function(){
  subtitle <- paste("Data from NY Times via covid-19.datasettes.com on", Sys.Date());
  first_day <- ISOdate(2020,3,1, tz="")
  
  countyData <- getSelectedCounties(countyPopulations, first_day)
  title = "Selected Counties"
  print(symmetricPlot(100000*death.slope/county.population~date | county, data=countyData, group=county, 
                      type = c("p", "smooth"),
                      span=0.2,
                      subtitle = subtitle, main=title, 
                      ylab="Slope (Deaths/Day/100,000)",
                      xlab="Date"))
  
}


#plotTest()


#links <- CreateCountyPlots(countyPopulations, ISOdate(2020,3,1, tz=""))

# #plot data for for all counties in a state to to a mark down file for display in github
# plotCountiesOfStateToMd <- function(stateName, countyPopulations){
#   subtitle <- "Data from NY Times via covid-19.datasettes.com"
#   first_day <- ISOdate(2020,3,1, tz="")
#   title <- paste(stateName, "Counties")
#  
#   county <- getCountiesByChunk (gsub(' ', '+', stateName), first_day, 5,  countyPopulations)
#   rmarkdown::render("countyPlots.rmd", output_file=paste(stateName, "Counties", sep="_"))
#   
# }
# 
# plotSelectedCountiesToMd <- function( countyPopulations){
#   subtitle <- "Data from NY Times via covid-19.datasettes.com"
#   first_day <- ISOdate(2020,3,1, tz="")
#   title <-  "Selected Counties"
#   
#   county <- selected #getSelectedCounties(countyPopulations, first_day )
#   
#   rmarkdown::render("countyPlots.rmd", output_file="Selected_Counties")
#   
# }


#plotCountiesOfStateToMd("California", countyPopulations)




# plotCountiesToMD <- function(states, countyPopulations){
# 
#   plotSelectedCountiesToMd
#   for(state in states){
#     print(paste("Generating plots for counties of", state))
#     plotCountiesOfStateToMd(state, countyPopulations)
#   }
#           
# }

#plotCountiesToMD(c("California", "New York", "Michigan", "Hawaii", "Texas", "Georgia", "Florida"),
#                 countyPopulations)




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
