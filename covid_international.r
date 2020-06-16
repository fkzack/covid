# Retrieve and plot international covid data from Johns Hopkins CSEE via covid-19.datasettes.com

library (jsonlite)
library(lattice)
library(latticeExtra)
library(lubridate)
library(plyr)

rm(list=ls())

source("covidPlot.r")

source("centraldifference.r")

#queries seem to be limited to returning a few thousand rows, so need to read in chunks of a few days

chunk_days <- 2
start_days <- seq(ymd('2020-01-01'),Sys.Date() ,by= as.difftime(days(chunk_days)))
start_days <- c(start_days, start_days[length(start_days)] + days(chunk_days))

# https://covid-19.datasettes.com/covid.json?sql=select+*+from+johns_hopkins_csse_daily_reports+where+\"day\"+>=+:p0+and+\"day\"+<+:p1&p0=2020-03-31&p1=2020-04-04
base_url <-"select * from johns_hopkins_csse_daily_reports where 'country_or_region' != 'US' and 'day' >= :p0 and 'day' < :p1"
#automatic encoding not working for some reason, so do manually
base_url <- gsub(" ", "+", base_url) 
base_url <- gsub(",", "%2c", base_url)
base_url <- gsub("'", "%22", base_url)
base_url <- gsub("<", "%3c", base_url)
base_url <- gsub(">", "%3e", base_url)
base_url <- gsub("=", "%3D", base_url)
base_url <- paste("https://covid-19.datasettes.com/covid.json?sql=", base_url, sep = "")

covid <- NULL
for (i in seq(1, length(start_days)-1)){
  #chunk_url <-  sub("PRIOR_DAY", start_days[i],date_range_url)
  #chunk_url <- sub("NEXT_DAY", next_days[i], chunk_url)
  chunk_url <- paste(base_url,"&p0=",start_days[i], "&p1=", start_days[i+1], sep="")
  print (paste(start_days[i],"<= day < ", start_days[i+1]))
  print(chunk_url)
  chunk <- fromJSON(chunk_url)
  print (length(chunk$rows))
  if (length(chunk$rows) < 1){
    next
  }
  chunk_names <- chunk$columns
  chunk <- data.frame(chunk$rows, stringsAsFactors=FALSE)
  names(chunk)     <- chunk_names
  chunk$date        <- as.POSIXct(chunk$day)
  chunk$confirmed  <- as.numeric(chunk$confirmed)
  chunk$deaths     <- as.numeric(chunk$deaths)
  chunk$recovered  <- as.numeric(chunk$recovered)
  chunk$latitude   <- as.numeric(chunk$latitude)
  chunk$longitude  <- as.numeric(chunk$longitude)
  
  #China is sometimes mainland china, sometimes china,  so make all China
  chunk$country_or_region <-  ifelse(startsWith(chunk$country_or_region, "Mainland"), "China", chunk$country_or_region)
  
  chunk$location   <- paste(chunk$country_or_region, ifelse(is.na(chunk$province_or_state), "", chunk$province_or_state))
  
  
  if (is.null(covid)){
    covid <- chunk
  } else {
    covid <- rbind(covid, chunk)
  }
}


#sweden is now reporting by individual state, so need to aggreate them
sweden <- subset(covid, country_or_region=="Sweden")
c2 <- subset(covid, country_or_region != "Sweden")
s2 <- ddply(sweden,"day",numcolwise(sum))
s2$date <-  as.POSIXct(s2$day)
s2$country_or_region <- "Sweden"
s2$location <- "Sweden"
covid <- rbind.fill(c2,s2)



# Denmark is sometimes Denmark, sometimes Denmark,Greenland, Denmark, Faroe Islands, .... 
# Consider only "Denmark, Denmark" and "Denmark,NA" to be actually denmark, as anything else appears to be an outlying teritory
covid$province_or_state = ifelse(startsWith(covid$country_or_region, "Denmark") & is.na(covid$province_or_state), "Denmark", covid$province_or_state)
covid$region <- ""
covid$region <- ifelse(startsWith(covid$province_or_state, "Denmark"), "Scandanavia", covid$region)
covid$region <- ifelse(startsWith(covid$location, "Sweden"), "Scandanavia", covid$region)
covid$region <- ifelse(startsWith(covid$location, "Norway"), "Scandanavia", covid$region)


#population data for scandanavia from quick google search
population = data.frame(country_or_region = c("Norway", "Sweden", "Denmark") , population = c(5.368, 10.23, 5.806))
population$population <- 1000000* population$population

covid <- merge(covid, population, all.x = TRUE)

covid <- covid[order(covid$country_or_region, covid$province_or_state, covid$day),]

covid$deaths.slope <-  centralDifference(covid$deaths, covid$date, covid$country_or_region)



label <-  "Data from Johns Hopkins CSSE via https://covid-19.datasettes.com"

# p_us <- covidPlot(confirmed~date | location, group=location,  
#                   data=subset(covid, startsWith(location, "US")),
#                   main="US", subtitle = label)

p_china <- covidPlot(confirmed~date | location, group=location,  
                     data=subset(covid, startsWith(location, "China")),
                     main = 'China',subtitle = label)

p_scandanavia <- covidPlot(confirmed~date | country_or_region, group=country_or_region,  
                     data=subset(covid, startsWith(region, "Scandanavia")),
                     main = 'Scandanavia',subtitle = label, numTickIntervalsX = 12)


p_scandanavia_per  <- covidPlot(100000 * confirmed/population~date | country_or_region, group=country_or_region,  
                           data=subset(covid, startsWith(region, "Scandanavia")),
                           ylab = "Cases per 100,000",
                           main = 'Scandanavia',subtitle = label, numTickIntervalsX = 12)

p_scandanavia_per_log2 <- covidPlot(100000 * confirmed/population~date | country_or_region, group=country_or_region,  
                                data=subset(covid, startsWith(region, "Scandanavia")),
                                logY=2,
                                ylab = "Cases per 100,000",
                                main = 'Scandanavia',subtitle = label, numTickIntervalsX = 12)

p_world <- covidPlot(confirmed~date | location, group=location,  
                     data=subset(covid, !startsWith(location, "China") & !startsWith(location, "US")),
                     main = 'Rest of World',subtitle = label)

# p_us_deaths <- covidPlot(deaths~date | location, group=location,  
#                   data=subset(covid, startsWith(location, "US")),
#                   main="US", subtitle = label)

p_china_deaths <- covidPlot(deaths~date | location, group=location,  
                     data=subset(covid, startsWith(location, "China")),
                     main = 'China',subtitle = label)



p_scandanavia_deaths <- covidPlot(deaths~date | country_or_region, group=country_or_region,  
                           data=subset(covid, startsWith(region, "Scandanavia")),
                           main = 'Scandanavia',subtitle = label, numTickIntervalsX = 12)


p_scandanavia_deaths_per  <- covidPlot(100000 * deaths/population~date | country_or_region, group=country_or_region,  
                                data=subset(covid, startsWith(region, "Scandanavia")),
                                ylab = "Deaths per 100,000",
                                main = 'Scandanavia',subtitle = label, numTickIntervalsX = 12)


p_scandanavia_deaths_per_log2  <- covidPlot(100000 * deaths/population~date | country_or_region, group=country_or_region,  
                                       data=subset(covid, startsWith(region, "Scandanavia")),
                                       logY=2,
                                       ylab = "Deaths per 100,000",
                                       main = 'Scandanavia',subtitle = label, numTickIntervalsX = 12)

p_scandanavia_deaths_per_linear  <- covidPlot(100000 * deaths/population~date | country_or_region, group=country_or_region,  
                                       data=subset(covid, startsWith(region, "Scandanavia")),
                                       logY=FALSE,
                                       ylab = "Deaths per 100,000",
                                       main = 'Scandanavia',subtitle = label, numTickIntervalsX = 12)


# exclude some really noisy sampling issues with sweden
covid$deaths_per_slope <- 100000 * covid$deaths.slope/covid$population
covid$deaths_per_slope <- ifelse(covid$deaths_per_slope > 5, NA, covid$deaths_per_slope)
covid$deaths_per_slope <- ifelse(covid$deaths_per_slope < 0, NA, covid$deaths_per_slope)
p_scandanavia_deaths_per_slope  <- covidPlot(deaths_per_slope~date | country_or_region, group=country_or_region,  
                                             data=subset(covid, startsWith(region, "Scandanavia")),
                                             logY=FALSE,
                                             ylab = "Slope(Deaths per 100,000 per day",
                                             main = 'Scandanavia',subtitle = label, numTickIntervalsX = 12)
p_scandanavia_deaths_per_slope 


p_world_deaths <- covidPlot(deaths~date | location, group=location,  
                     data=subset(covid, !startsWith(location, "China") & !startsWith(location, "US")),
                     main = 'Rest of World',subtitle = label)



printAll <- function(){
  #print(p_us)
  print(p_china)
  print(p_scandanavia)
  print(p_scandanavia_per)
  #print(p_world)
  #print(p_us_deaths)
  print(p_china_deaths)
  #print(p_world_deaths)
  print(p_scandanavia_deaths)
  print(p_scandanavia_deaths_per)
  print(p_scandanavia_deaths_per_log2)
  
}

#print(p_china)

#printAll()

test <- function(){
  s1 <- seq(ISOdate(2020, 1,1), by="day", length.out=160)
  df <- data.frame(date=s1)
  df$count <- 1:length(df$date)
  df$group <- df$count %% 14
  df$x <- 10^((df$count + df$group)/100)
  p <- covidPlot(x~date, data=df)
  
  print(p)
  
  
}

