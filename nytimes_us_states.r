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

states_url <- "https://covid-19.datasettes.com/covid/ny_times_us_states.json"


rawJSON <- fromJSON(states_url)

states <- data.frame(rawJSON$rows, stringsAsFactors = F)
names(states) <- rawJSON$columns
states$date <- as.Date(states$date)
states$deaths <- as.numeric(states$deaths)
states$cases <- as.numeric(states$cases)
states$county <- paste(states$county, state.abb[match(states$state, state.name)])
states$fips < as.numeric(states$fips)
print(str(states))
