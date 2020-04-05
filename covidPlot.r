install_github("fkzack/FredsRUtils")
library(FredsRUtils)
library(lattice)
library(latticeExtra)
        

# Wrap xyplot to create the plot I want (log y axis, log 10 grids, ...)
# depends on x variable being $date, should change this once I figure out how to decode formula
covidPlot <- function(formula1, data, subtitle = "", dateTickCount = 5, ...){
  
  #this gets the incoming data frame
  #print(get_all_vars(formula, data=data))
  #print(formula(formula1))
  

  #ticksAt <-as.Date(pretty_dates(get_all_vars(formula1, data=data)$date, 3))
  ticksAt <-as.Date(weekly_ticks(get_all_vars(formula1, data=data)$date, 0, dateTickCount)$majors)
  print (dateTickCount)
  print(ticksAt)
  
  p <- xyplot(formula1,  data = data,
              scales=list(y=list(log=10),
                          x=list(rot=45, at=ticksAt)
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
