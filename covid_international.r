library (jsonlite)
library(lattice)
library(latticeExtra)
library(lubridate)

rm(list=ls())

source("covidPlot.r")

#queries seem to be limited to returning a few thousand rows, so need to read in chunks of a few days

chunk_days <- 2
start_days <- seq(ymd('2020-01-01'),Sys.Date() ,by= as.difftime(days(chunk_days)))
start_days <- c(start_days, start_days[length(start_days)] + days(chunk_days))

# https://covid-19.datasettes.com/covid.json?sql=select+*+from+johns_hopkins_csse_daily_reports+where+\"day\"+>=+:p0+and+\"day\"+<+:p1&p0=2020-03-31&p1=2020-04-04
base_url <-"https://covid-19.datasettes.com/covid.json?sql=select+*+from+johns_hopkins_csse_daily_reports+where+%22day%22+%3E%3D+%3Ap0+and+%22day%22+%3C+%3Ap1"  
#"&p0=2020-03-04&p1=2020-03-06"

#base_url <-"https://covid-19.datasettes.com/covid.json?sql=select+*+from+johns_hopkins_csse_daily_reports+where+%22day%22+%3E%3D+%3Ap0+and+%22day%22+%3C+%3Ap1"  
base_url <-"select * from johns_hopkins_csse_daily_reports where 'country_or_region' != 'US' and 'day' >= :p0 and 'day' < :p1"
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
  
  #China is sometimes mainland china, sometimes china,  so make all china
  chunk$country_or_region <-  ifelse(startsWith(chunk$country_or_region, "Mainland"), "China", chunk$country_or_region)
  
  chunk$location   <- paste(chunk$country_or_region, ifelse(is.na(chunk$province_or_state), "", chunk$province_or_state))
  
  if (is.null(covid)){
    covid <- chunk
  } else {
    covid <- rbind(covid, chunk)
  }
}

covid <- covid[order(covid$country_or_region, covid$province_or_state, covid$day),]


label <-  "Data from Johns Hopkins CSSE via https://covid-19.datasettes.com"

# p_us <- covidPlot(confirmed~date | location, group=location,  
#                   data=subset(covid, startsWith(location, "US")),
#                   main="US", subtitle = label)

p_china <- covidPlot(confirmed~date | location, group=location,  
                     data=subset(covid, startsWith(location, "China")),
                     main = 'China',subtitle = label)

p_sweden <- covidPlot(confirmed~date | location, group=location,  
                     data=subset(covid, startsWith(country_or_region, "Sweden")),
                     main = 'Sweden',subtitle = label, numTickIntervalsX = 6)


p_world <- covidPlot(confirmed~date | location, group=location,  
                     data=subset(covid, !startsWith(location, "China") & !startsWith(location, "US")),
                     main = 'Rest of World',subtitle = label)

# p_us_deaths <- covidPlot(deaths~date | location, group=location,  
#                   data=subset(covid, startsWith(location, "US")),
#                   main="US", subtitle = label)

p_china_deaths <- covidPlot(deaths~date | location, group=location,  
                     data=subset(covid, startsWith(location, "China")),
                     main = 'China',subtitle = label)


p_sweden_deaths <- covidPlot(deaths~date | location, group=location,  
                            data=subset(covid, startsWith(country_or_region, "Sweden")),
                            main = 'Sweden',subtitle = label, numTickIntervalsX = 6)

p_world_deaths <- covidPlot(deaths~date | location, group=location,  
                     data=subset(covid, !startsWith(location, "China") & !startsWith(location, "US")),
                     main = 'Rest of World',subtitle = label)



printAll <- function(){
  #print(p_us)
  print(p_china)
  print(p_sweden)
  print(p_world)
  #print(p_us_deaths)
  print(p_china_deaths)
  print(p_world_deaths)
  print(p_sweden_deaths)
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

