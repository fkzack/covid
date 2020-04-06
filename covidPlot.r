install_github("fkzack/FredsRUtils", type="source")
# install.packages('D:/Data/R/FredsRUtils_0.1.0.tar.gz', repos=NULL, type="source" )
library(FredsRUtils)
library(lattice)
library(latticeExtra)
        

# Wrap xyplot to create the plot I want (log y axis, log 10 grids, ...)
# depends on x variable being $date, should change this once I figure out how to decode formula
covidPlot <- function(formula1, data, subtitle = "", numTickIntervals = 5, ...){
  
  
  #this gets the incoming data frame
  #print(get_all_vars(formula, data=data))
  #print(formula(formula1))
  
  
  #ticksAt <-as.Date(pretty_dates(get_all_vars(formula1, data=data)$date, 3))
  #ticksAt <-as.Date(weekly_ticks(get_all_vars(formula1, data=data)$date, 0, numTickIntervals)$majors)
  date_var <- get_all_vars(formula1, data=data)$date
  ticksAt <- date_ticks(date_var,numTickIntervals, 0)
  
  #print(paste("ticksAt:", class(ticksAt)))
  #print(paste("date:   ", class(date_var)))
  
  
  p <- xyplot(formula1,  data = data,
              scales=list(y=list(log=10),
                          x=list(rot=45, at=ticksAt, format="%Y-%m-%d")
              ),
              yscale.components = latticeExtra::yscale.components.log10ticks,
              sub=list(label=paste(subtitle, "   "), cex=0.5, x=1, just="right"),
              par.strip.text=list(cex=0.75),
              type=c('p', 'l'),
              as.table=TRUE,
              panel=function(x,y,...){
                panel.xyplot(x,y,...)
              },
              
              ...)
  return (addGrid(p))
}


# s1 <- seq(ISOdate(2020, 1,11), by="day", length.out=200)
# df <- data.frame(date=s1)
# df$count <- 1:length(df$date)
# df$group <- df$count %% 14
# df$x <- 10^((df$count + df$group)/100)
# p <- covidPlot(x~date | group, data=df)
# 
# print(p)
