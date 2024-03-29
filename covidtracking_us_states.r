#covid data by state from https://covidtracking.com/


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
install_github("fkzack/FredsRUtils")
library(FredsRUtils)


rm(list=ls())

source("census.r")
source("covidPlot.r")



states <- GetStatesAndPopulation()

#get latest corona data from covidtracking.com
#corona <- fromJSON("https://covidtracking.com/api/states")
corona <- fromJSON("https://covidtracking.com/api//v1/states/daily.json")
#corona <- rename(corona, "state.abb" = state )
#print(str(corona))
head(corona)




# most recent corona data by state for interactive map

coronaData<- merge(states, corona)
coronaData$state.population <- as.numeric(coronaData$state.population)
coronaData$deaths.per.million <- 1000000 * coronaData$death / coronaData$state.population
coronaData$hover = paste(coronaData$state.name, 
                         '<br>', "  Deaths:", coronaData$death ,
                         '<br>', "  Population:", prettyNum(coronaData$state.population, big.mark=",") ,
                         '<br>', "  Deaths Per Million", prettyNum(coronaData$deaths.per.million, digits=3) ,
                         '<br>', "  Last Update(ET)", coronaData$lastUpdateEt
)


#replace zero values with low number to use in log plot
min_non_zero <- min(coronaData[coronaData$deaths.per.million > 0,]$deaths.per.million, na.rm=TRUE)
zeros <- which(coronaData$deaths.per.million == 0)
coronaData$log.deaths.per.million <- coronaData$deaths.per.million
coronaData$log.deaths.per.million<-  replace(coronaData$log.deaths.per.million, zeros, min_non_zero/100)
coronaData$log.deaths.per.million <- log10(coronaData$log.deaths.per.million)

log.ticks <- seq(floor(min(coronaData$log.deaths.per.million, na.rm=TRUE)) , ceiling(max(coronaData$log.deaths.per.million, na.rm=TRUE)))
log.labels <- 10 ^ log.ticks


str(coronaData)


#barchart(coronaData$deaths.per.million~coronaData$state.name,  scales=list(x=list(rot=45)))



redPallete <- colorRamp(c("#FFFFFF", "#FF0000"), interpolate="spline", space="Lab", bias = 10)

#interactive maps do not work nicely with display via gethub, so leave out for now

# f <- plot_ly(type='choropleth', locations = coronaData$state.abb, locationmode="USA-states",  z=coronaData$deaths.per.million,
#              text=coronaData$hover, colors = redPallete) 
# f <- layout(f, geo=list(scope="usa", bgcolor="EEE"), title = 'Deaths from COVID 19<br>(Hover for Details)')
# f <- colorbar(f, title="Deaths per Million")
# print(f)
 


# p_death_map <- plot_ly(type='choropleth', locations = coronaData$state.abb, locationmode="USA-states", z=coronaData$log.deaths.per.million,
#                        text=coronaData$hover, colors = redPallete) 
# p_death_map <- layout(p_death_map, geo=list(scope="usa", bgcolor="EEE"), title = 'Deaths from COVID 19<br>(Hover for Details)')
# p_death_map <- colorbar(p_death_map, title="Deaths per Million", tickvals=log.ticks, ticktext=log.labels)
# print(p_death_map)


#daily data by state


dailies <- fromJSON("https://covidtracking.com/api/states/daily")
dailies$date <- as.Date(as.character(dailies$date), "%Y%m%d")
dailies <- rename (dailies, "state.abb" = "state")
dailies <- merge(states, dailies)
dailies$state.population <- as.numeric(dailies$state.population)
dailies <- dailies[order(dailies$state.abb, dailies$date),]
dailies$positiveIncrease <- ifelse(dailies$positiveIncrease < 0, NA, dailies$positiveIncrease)

str(dailies)


label <- paste( "COVID data from covidtracking.com/api/states/daily on ", Sys.Date())

p_positives <- covidPlot(positive~date | state.abb, group=state.abb, data=dailies, 
                         subtitle=label,ylab="positves", main="US States")

p_positivesPer <- covidPlot(100000*positive/state.population ~ date | state.abb, group=state.abb, data=dailies, 
                            subtitle=label, ylab = "positves per 100,000", main="US States")

p_deltaPositivesPer <- symmetricPlot(100000*positiveIncrease/state.population ~ date | state.abb, data=dailies, 
                                     group=state.abb, 
                                     type = c("p"),
                                     subtitle=label, 
                                     ylab = "Increase (positives/day/100,000)", main="US States")


#p_positive_fraction <- covidPlot2(positive/(positive + negative)~date | state.abb, group=state.abb, data=dailies, subtitle=label,ylab="fraction positve", main="US States")
#p_positive_fraction <- xyplot(positive/(positive + negative)~date | state.abb, group=state.abb, data=dailies, subtitle=label,ylab="fraction positve", main="US States", as.table=TRUE)
p_positive_fraction <- covidPlot(positive/(positive + negative)~date | state.abb, group=state.abb, data=dailies,
                                 subtitle=label,ylab="fraction positve", main="US States")


p_total_tests <- covidPlot(totalTestResults ~ date | state.abb, group=state.abb, data=dailies, 
                               subtitle=label, ylab = "Total Tests", main="US States")


p_total_tests_per <- covidPlot(100000*totalTestResults/state.population ~ date | state.abb, group=state.abb, data=dailies, 
                            subtitle=label, ylab = "Total Tests per 100,000", main="US States")


p_deaths <- covidPlot(death ~ date | state.abb, group=state.abb, data=dailies, 
                      subtitle=label, ylab = "deaths", main="US States")

p_deaths_liner <- covidPlot(death ~ date | state.abb, group=state.abb, data=dailies, 
                      subtitle=label, logY = FALSE, ylab = "deaths", main="US States")


p_deltaDeaths <- symmetricPlot(pmax(-10, deathIncrease) ~ date | state.abb, group=dailies$state.abb, data=dailies, 
                                  subtitle=label, type=list('p'), ylab = "Increase (deaths/day) (negetive deaths clipped to -10) ", main="US States")


p_deathsPerLinear <- covidPlot(100000*death/state.population ~ date | state.abb, group=state.abb, data=dailies, 
                         subtitle=label, logY=FALSE,  ylab = "deaths per 100,000", main="US States")

month_ago = Sys.Date() - months(1)
p_deathsPerLinearLastMonth <- covidPlot(100000*death/state.population ~ date | state.abb, group=state.abb, data=subset(dailies, dailies$date > month_ago), 
                               subtitle=label, logY=FALSE,  ylab = "deaths per 100,000", main="US States")





p_deathsPer <- covidPlot(100000*death/state.population ~ date | state.abb, group=state.abb, data=dailies, 
                         subtitle=label, ylab = "deaths per 100,000", main="US States")


p_deltaDeathsPer <- symmetricPlot(pmax(-0.001, 100000*deathIncrease/state.population) ~ date | state.abb, group=dailies$state.abb, data=dailies, 
                                  subtitle=label, type=list('p'), ylab = "Increase (deaths/day/100,000) (negetive deaths clipped to -0.001) ", main="US States")


p_hosp <- covidPlot(hospitalized ~ date | state.abb, group=state.abb, data=dailies, 
                    subtitle=label, ylab = "hospitalizations", main="US States")

p_hospPer <- covidPlot(100000*hospitalized/state.population ~ date | state.abb, group=state.abb, data=dailies, 
                       subtitle=label, ylab = "hospitalizations per 100,000", main="US States")

p_deltaHospPer <-symmetricPlot(100000*hospitalizedIncrease/state.population ~ date | state.abb, group=dailies$state.abb, 
                               data=dailies, subtitle=label, ylab = "Increase (hospitalized/day/100,000)", main="US States")


#print(p_deaths)

printPlots <- function (){
  print (p_hosp)
  print(p_hospPer)
  print(p_positives)
  print(p_positivesPer)
  print(p_deaths)
  print (p_deathsPer)
  
}


#printPlots()

#show a color palette
showColorPalette<- function (pal){
  df <- data.frame(x=seq(0,1,.05))
  df$col = pal(df$x)
  print (df)

  #show_col(rgb(df$col, maxColorValue = 255), cex=0.5)
  n <- length(df$x)
  image(df$x, 1, as.matrix(df$x), col=rgb(df$col, maxColorValue = 255))
  
  
}

CreateStatePlots <- function(){
  out_file_name = "statePlots"
  title <- "State"
  t <- system.time(rmarkdown::render("statePlots.rmd", output_file = "statePlots"))
  print(paste("*** render state plots  ", t, "\n"))
  link <- paste("[", title, " Details](", out_file_name, ".html)\n", sep="")
  return(link)
}

# par(mfcol=c(3,1))
# pal1 <- colorRamp(c("#FFEEEE", "#FF0000"), interpolate="spline", space="Lab", bias = 10)
# showColorPalette((pal1))
# pal2 <- colorRamp(c("#FFEEEE", "#FF0000"), interpolate="spline", space="Lab", bias = 0.1)
# showColorPalette((pal2))
# pal3 <- colorRamp(c("#FFEEEE", "#FF0000"), interpolate="linear", space="Lab", bias = 1)
# showColorPalette((pal3))

