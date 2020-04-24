########################################################################################
########################################################################################
# This seems to be rarely updated, not very useful.
# See NY Times data by county (us_counties.r in this project) for more useful data
########################################################################################
########################################################################################



library (jsonlite)
library(lattice)
library(latticeExtra)
library(RCurl)
library(dplyr)
library(lubridate)




#stanford open data portal is showing covid data in a google sheet
#csvLink <- "https://s3.us-east-2.amazonaws.com/open-data-portal/covid19_sccph.csv"
#scc  <- read.csv( textConnection(getURL(csvLink)), stringsAsFactors = F)


#bay area counties from stanford open data (as csv): (this appears to be last updated 3/24/2020, so too 
#old to be useful)
baLink <- "https://s3.us-east-2.amazonaws.com/open-data-portal/covid19_bayarea_counties.csv"
ba  <- read.csv( textConnection(getURL(baLink)), stringsAsFactors = F)
ba$Date <- as.Date(as.character(ba$Date), "%m/%d/%Y")
ba$Population..2017. <- as.numeric(gsub(",", "", ba$Population..2017.))
ba$Total.Confirmed.Cases <- as.numeric(ba$Total.Confirmed.Cases)
ba$Total.Confirmed.Cases.Per.100K <- as.numeric(ba$Total.Confirmed.Cases.Per.100K)
ba$Past.3.day.Growth <- as.numeric(gsub("%", "", ba$Past.3.day.Growth))/100
ba$County < as.factor(ba$County)
str(ba)

label = paste("SF Bay Area Covid Cases, ", (max(ba$Date)))
xyplot(Total.Confirmed.Cases.Per.100K ~ Date | County, data=ba, group=County, type=c('p', 'l'))
xyplot(na_if(Total.Confirmed.Cases.Per.100K, 0) ~ Date | County, data=ba, group=County, 
       type=c('p', 'l'),
       scales=list(
         x=list(at=as.Date(pretty_dates(ba$Date, 2)), rot=45),
         y=list(log=10)),
       yscale.components = latticeExtra::yscale.components.log10ticks,
       main=list(label=label, cex = 0.75),
       ylab="Total Confirmed Cases per 100,000 Population",
       sub=list(label=paste("  Data Source:", baLink), cex=0.5, x=0, just="left"),
       #par.sub.text=list(just="right"),
       as.table=T, 
       par.strip.text=list(cex=0.75)
)

xyplot(na_if(Total.Confirmed.Cases, 0) ~ Date | County, data=ba, group=County, 
       type=c('p', 'l'),
       scales=list(
         x=list(at=as.Date(pretty_dates(ba$Date, 2)), rot=45),
         y=list(log=10)),
       yscale.components = latticeExtra::yscale.components.log10ticks,
       main=list(label=label, cex = 0.75),
       ylab="Total Confirmed Cases",
       sub=list(label=paste("  Data Source:", baLink), cex=0.5, x=0, just="left"),
       #par.sub.text=list(just="right"),
       as.table=T, 
       par.strip.text=list(cex=0.75)
)
       
       
       







# #this data seems to be two weeks old. is there a new data set >
# 
# scs_data <- fromJSON("https://covid-19.datasettes.com/covid.json?sql=select%0D%0A++*%0D%0Afrom%0D%0A++daily_reports%0D%0Awhere%0D%0A++province_or_state+like+%27Santa+Clar%25%27")
# scs <- data.frame(scs_data$rows, stringsAsFactors = FALSE)
# names(scs)     <- scs_data$columns
# scs$day        <- as.POSIXct(scs$day)
# scs$confirmed  <- as.numeric(scs$confirmed)
# scs$deaths     <- as.numeric(scs$deaths)
# scs$recovered  <- as.numeric(scs$recovered)
# scs$latitude   <- as.numeric(scs$latitude)
# scs$longitude  <- as.numeric(scs$longitude)
# str(scs)
# xyplot(confirmed~day, data=scs)
# 
# 
# #this has same problem, turns out they have switche tostate level
# 
# scs_data <- fromJSON("https://covid-19.datasettes.com/covid/daily_reports.json?_facet=province_or_state&_facet=country_or_region&province_or_state__startswith=santa+clara")
# scs <- data.frame(scs_data$rows, stringsAsFactors = FALSE)
# head(scs_data)
# names(scs)     <- scs_data$columns
# scs$day        <- as.POSIXct(scs$day)
# scs$confirmed  <- as.numeric(scs$confirmed)
# scs$deaths     <- as.numeric(scs$deaths)
# scs$recovered  <- as.numeric(scs$recovered)
# scs$latitude   <- as.numeric(scs$latitude)
# scs$longitude  <- as.numeric(scs$longitude)
# str(scs)
# xyplot(confirmed~day, data=scs)




# plot_covid <- function (covidUrl){
#   data <- fromJSON(covidUrl)
#   covid <- data.frame(data$rows, stringsAsFactors = FALSE)
#   head(covid)
#   names(covid)     <- data$columns
#   covid$day        <- as.POSIXct(covid$day)
#   covid$confirmed  <- as.numeric(covid$confirmed)
#   covid$deaths     <- as.numeric(covid$deaths)
#   covid$recovered  <- as.numeric(covid$recovered)
#   covid$latitude   <- as.numeric(covid$latitude)
#   covid$longitude  <- as.numeric(covid$longitude)
#   str(covid)
#   xyplot(confirmed~day, data=covid)
#   
# }

#plot_covid("https://covid-19.datasettes.com/covid.json?sql=select+*+from+daily_reports+where+province_or_state+%3D+%27California%27")
