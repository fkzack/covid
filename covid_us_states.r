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

rm(list=ls())


GetStatesAndPopulation <- function() {
  # get 2019 state population data from census bureau
  pop <- fromJSON("https://api.census.gov/data/2019/pep/population?get=NAME,POP&for=state:*")
  popNames <- pop[1,]
  pop <- data.frame(pop[-1,], stringsAsFactors = FALSE)
  names(pop) <- popNames
  pop <- rename(pop, c("state.name" = "NAME" , "state.FIPS" = "state", "state.population" = "POP"))


  #need to match state names with state abbreviations
  states <- data.frame(state.name, state.abb, stringsAsFactors = FALSE)
  states <- add_row(states, state.name="District of Columbia",  state.abb ="DC")
  states <- add_row(states, state.name="Puerto Rico",  state.abb ="PR")
  states <- merge(states, pop)
  print(str(states))
  return (states)
head(states)
}


states <- GetStatesAndPopulation()

#get latest corona data from covidtracking.com
corona <- fromJSON("https://covidtracking.com/api/states")
corona <- rename(corona, c("state.abb" = "state" ))
print(str(corona))
head(corona)


coronaData<- merge(states, corona)
coronaData$state.population <- as.numeric(coronaData$state.population)
coronaData$deaths.per.million <- 1000000 * coronaData$death / coronaData$state.population
coronaData$hover = paste(coronaData$state.name, 
                         '<br>', "  Deaths:", coronaData$death ,
                         '<br>', "  Population:", prettyNum(coronaData$state.population, big.mark=",") ,
                         '<br>', "  Deaths Per Million", prettyNum(coronaData$deaths.per.million, digits=3) ,
                         '<br>', "  Last Update(ET)", coronaData$lastUpdateEt
)
print (coronaData$hover)
print (coronaData$state.death)


str(coronaData)


#replace zero values with low number to use in log plot
min_non_zero <- min(coronaData[coronaData$deaths.per.million > 0,]$deaths.per.million, na.rm=TRUE)
zeros <- which(coronaData$deaths.per.million == 0)
coronaData$log.deaths.per.million <- coronaData$deaths.per.million
coronaData$log.deaths.per.million<-  replace(coronaData$log.deaths.per.million, zeros, min_non_zero/100)
coronaData$log.deaths.per.million <- log10(coronaData$log.deaths.per.million)

log.ticks <- seq(floor(min(coronaData$log.deaths.per.million, na.rm=TRUE)) , ceiling(max(coronaData$log.deaths.per.million, na.rm=TRUE)))
log.labels <- 10 ^ log.ticks


print(str(coronaData))
head(coronaData)


barchart(coronaData$deaths.per.million~coronaData$state.name,  scales=list(x=list(rot=45)))



redPallete <- colorRamp(c("#FFFFFF", "#FF0000"), interpolate="spline", space="Lab", bias = 10)


f <- plot_ly(type='choropleth', locations = coronaData$state.abb, locationmode="USA-states",  z=coronaData$deaths.per.million,
             text=coronaData$hover, colors = redPallete) 
f <- layout(f, geo=list(scope="usa", bgcolor="EEE"), title = 'Deaths from COVID 19<br>(Hover for Details)')
f <- colorbar(f, title="Deaths per Million")
print(f)
 


f <- plot_ly(type='choropleth', locations = coronaData$state.abb, locationmode="USA-states", z=coronaData$log.deaths.per.million,
             text=coronaData$hover, colors = redPallete) 
f <- layout(f, geo=list(scope="usa", bgcolor="EEE"), title = 'Deaths from COVID 19<br>(Hover for Details)')
f <- colorbar(f, title="Deaths per Million", tickvals=log.ticks, ticktext=log.labels)
print(f)




dailies <- fromJSON("https://covidtracking.com/api/states/daily")
dailies$date <- as.Date(as.character(dailies$date), "%Y%m%d")
dailies <- rename (dailies, "state.abb" = "state")
dailies <- merge(states, dailies)
dailies$state.population <- as.numeric(dailies$state.population)
str(dailies)


label <- paste( "COVID data from covidtracking.com/api/states/daily on ", Sys.Date())

p_positives <- xyplot(na_if(positive, 0)~date | state.abb,  group=state.abb, data=dailies, 
       ylab='positives',
       main=list(label=label, cex=0.75),
       scales=list(y=list(log=10), x=list(rot=45, at=as.Date(pretty_dates(dailies$date,2)))),
       yscale.components = latticeExtra::yscale.components.log10ticks,
       as.table=TRUE
       )
#print(addGrid(p_positives))
#print(addGrid(p_positives, MinorGrid=FALSE))

p_positivesPer <- xyplot(na_if(10000*positive/state.population, 0)~date | state.abb,  group=state.abb, data=dailies, 
            ylab='positive tests per 100,000 population',
            main=list(label=label, cex=0.75),
            scales=list(y=list(log=10), x=list(rot=45, at=as.Date(pretty_dates(dailies$date,2)))),
            yscale.components = latticeExtra::yscale.components.log10ticks,
            as.table=TRUE
)

#print(p_positivesPer)
#print(addGrid(p_positivesPer))

p_deaths <- xyplot(na_if(death, 0)~date | state.abb,  group=state.abb, data=dailies, 
            ylab='deaths',
            main=list(label=label, cex=0.75),
            scales=list(y=list(log=10), x=list(rot=45, at=as.Date(pretty_dates(dailies$date,2)))),
            yscale.components = latticeExtra::yscale.components.log10ticks,
            as.table=TRUE
)

#print(p_deaths)
#print(addGrid(p_deaths))

p_deathsPer <- xyplot(na_if(100000*death/state.population, 0)~date | state.abb,  group=state.abb, data=dailies, 
            ylab='deaths per 100,000 population',
            main=list(label=label, cex=0.75),
            scales=list(y=list(log=10), x=list(rot=45, at=as.Date(pretty_dates(dailies$date,2)))),
            yscale.components = latticeExtra::yscale.components.log10ticks,
            as.table=TRUE
)

#print(p_deathsPer)
#print(addGrid(p_deathsPer))

p_hosp <- xyplot(na_if(hospitalized, 0)~date | state.abb,  group=state.abb, data=dailies, 
                   ylab='hospitalized',
                   main=list(label=label, cex=0.75),
                   scales=list(y=list(log=10), x=list(rot=45, at=as.Date(pretty_dates(dailies$date,2)))),
                   yscale.components = latticeExtra::yscale.components.log10ticks,
                   as.table=TRUE
)
print(p_hosp)

p_hospPer <- xyplot(na_if(100000*hospitalized/state.population, 0)~date | state.abb,  group=state.abb, data=dailies, 
                 ylab='hospitalized per 100,000 population',
                 main=list(label=label, cex=0.75),
                 scales=list(y=list(log=10), x=list(rot=45, at=as.Date(pretty_dates(dailies$date,2)))),
                 yscale.components = latticeExtra::yscale.components.log10ticks,
                 as.table=TRUE
)
print(p_hospPer)



print(p_positivesPer)
print(p_deaths)
print (p_deathsPer)



#show a color palette
showColorPalette<- function (pal){
  df <- data.frame(x=seq(0,1,.05))
  df$col = pal(df$x)
  print (df)

  #show_col(rgb(df$col, maxColorValue = 255), cex=0.5)
  n <- length(df$x)
  image(df$x, 1, as.matrix(df$x), col=rgb(df$col, maxColorValue = 255))
  
  
}

# par(mfcol=c(3,1))
# pal1 <- colorRamp(c("#FFEEEE", "#FF0000"), interpolate="spline", space="Lab", bias = 10)
# showColorPalette((pal1))
# pal2 <- colorRamp(c("#FFEEEE", "#FF0000"), interpolate="spline", space="Lab", bias = 0.1)
# showColorPalette((pal2))
# pal3 <- colorRamp(c("#FFEEEE", "#FF0000"), interpolate="linear", space="Lab", bias = 1)
# showColorPalette((pal3))
