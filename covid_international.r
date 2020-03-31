library (jsonlite)
library(lattice)
library(latticeExtra)
library(lubridate)

#queries seem to be limited to returning a few thousand rows, so need to chunk by day

chunk_days <- 5
prior_days <- seq(ymd('2020-01-01'),Sys.Date(),by= as.difftime(days(chunk_days)))
next_days <- (prior_days + days(1))[-1]
next_days <- append(next_days, Sys.Date() + days(1))
date_range_url <- "https://covid-19.datasettes.com/covid.json?sql=select+*+from+daily_reports+where+day+%3E++%22PRIOR_DAY%22+and+day+%3C+%22NEXT_DAY%22%0D%0A"

covid <- NULL
for (i in seq(1, length(prior_days))){
  chunk_url <-  sub("PRIOR_DAY", prior_days[i],date_range_url)
  chunk_url <- sub("NEXT_DAY", next_days[i], chunk_url)
  print(chunk_url)
  chunk <- fromJSON(chunk_url)
  print (length(chunk$rows))
  if (length(chunk$rows) < 1){
    next
  }
  chunk_names <- chunk$columns
  chunk <- data.frame(chunk$rows, stringsAsFactors=FALSE)
  names(chunk)     <- chunk_names
  chunk$day        <- as.POSIXct(chunk$day)
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

label <- paste( "COVID data from covidtracking.com/api/states/daily on ", Sys.Date())

plotConfirmed <- function (dataset){
  p <- xyplot(confirmed~day | location, group=location, data=dataset,
              scales=list(
                y=list(log=10), 
                #x=list(rot=45, at=as.Date(pretty_dates(as.Date(dataset$day),2)))
                x=list(rot=45)
                ),
              yscale.components = latticeExtra::yscale.components.log10ticks,
              #layout=c(10,10),
              #main=list(label=label, cex=0.7),
              as.table=T, par.strip.text=list(cex=0.75))
  return (p)
  
}



p_us <- plotConfirmed(subset(covid, startsWith(location, "US")))
print(p_us)

p_china <- plotConfirmed(subset(covid, startsWith(location, "China")))
print(p_china)

                   
p_world <- plotConfirmed(subset(covid, !startsWith(location, "US") & !startsWith(location, "China")))
print(p_world)


plotDeaths <- function (dataset){
  p <- xyplot(deaths~day | location, group=location, data=dataset,
              scales=list(
                y=list(log=10), 
                #x=list(rot=45, at=as.Date(pretty_dates(as.Date(dataset$day),2)))
                x=list(rot=45)
              ),
              yscale.components = latticeExtra::yscale.components.log10ticks,
              #layout=c(10,10),
              main=list(label=label, cex=0.7),
              as.table=T, par.strip.text=list(cex=0.75))
  return (p)
}




p_us_deaths <- plotDeaths(subset(covid, startsWith(location, "US")))
print(p_us_deaths)

p_china_deaths <- plotDeaths(subset(covid, startsWith(location, "China")))
print(p_china_deaths)


p_world_deaths <- plotDeaths(subset(covid, !startsWith(location, "US") & !startsWith(location, "China")))
print(p_world_deaths)
#pop<- fromJSON("https://data.opendatasoft.com/api/records/1.0/search/?dataset=world-population%40kapsarc&rows=1000&facet=year&facet=country_name&refine.year=2018")

  