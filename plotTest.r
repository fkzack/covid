library(lattice)
library(latticeExtra)
library(lubridate)

rm(list=ls())

source("covidPlot.r")



  s1 <- seq(ISOdate(2020, 1,1), by="day", length.out=160)
  df <- data.frame(date=s1)
  df$count <- 1:length(df$date)
  df$group <- df$count %% 14
  df$x <- 10^((df$count + df$group)/100)
  p <- covidPlot(x~date, data=df)
  
  print(p)
  
  

  