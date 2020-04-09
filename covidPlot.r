library(devtools)
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


# create a "symmetric log plot", with postive values on a log plot going up from zero, zero values at 0, 
# and negitive values as a plot of log of absolute error going down from zero
symmetricPlot <- function(formula1, data,   groupVector, subtitle = "", numTickIntervals = 5, ...){
  #this gets the incoming data frame
  #print(head(group))
  #original_df <- get_all_vars(formula1, data=data)
  #print(formula(formula1))
  
  lpf <- latticeParseFormula(formula1, data=data)
  df <- data.frame(lpf$right, lpf$left, unlist(lpf$condition, use.names = FALSE), groupVector)
  
  names(df) <- c('x', 'y', 'condition', 'group')
  
  #for debug use
  #df$log <- log10(abs(df$y))
  
  #scale values
  min_absolute <- min(ifelse(df$y==0, NA,abs(df$y)), na.rm = TRUE)
  
  #the center line we display around 
  log_origin = floor(log10(min_absolute))
  min_y <- min(df$y, na.rm=TRUE)
  max_y <- max(df$y, na.rm=TRUE)
  
    
  #transform from y values to display values
  df$dv <- ifelse(df$y > 0, log10(df$y) - log_origin+1, NA)
  df$dv <- ifelse(df$y < 0 , -(log10(-df$y) - log_origin+1), df$dv)
  df$dv <- ifelse(df$y == 0 , 0, df$dv)
  #print(df)
  
  #ticks as log value in original (_y) coordinates and in display value (_dv) coordianates
  negTicks_y  <- c()
  negTicks_dv <- c()
  posTicks_y  <- c()
  posTicks_dv <- c()
  
  #tick locations at powers of ten in original coordinate system
  if (min_y < 0){
    negTicks_y <- seq(ceiling(log10(-min_y)), log_origin) #log of original coordinates
    negTicks_dv <- -negTicks_y +log_origin-1
  }
  
  if (max_y > 0){
    posTicks_y <- seq(log_origin, ceiling(log10(max_y)))
    posTicks_dv <- posTicks_y - log_origin +1
  }
  
  ticksAt <- c(negTicks_dv, 0, posTicks_dv)
  tickLabels <- c(-10^negTicks_y, 0,10^posTicks_y )
  
  #x axis
  xticksAt <- date_ticks(df$x,numTickIntervals, 0)
  
  #print(str(df))
  p <- xyplot(df$dv~df$x | df$condition, group=df$group, pch=c('p', 'l'),
              scales=list(y=list(at=ticksAt, labels=tickLabels),
                          x=list(at=xticksAt,rot=45, format="%Y-%m-%d" )),
              
              sub=list(label=paste(subtitle, "   "), cex=0.5, x=1, just="right"),
              par.strip.text=list(cex=0.75),
              type=c('p', 'l'),
              as.table=TRUE,
              panel=function(x,y,...){
                panel.xyplot(x,y,...)
                #This gets overwritten by add grid
                panel.abline(h = 0, col=rgb(0,0,0), alpha=0.1,  lwd=20)
              },
              
              ...)
  #This does not work here or in xyplot above as it gets overwritten by add grid
  # p <- update(
  #   p, panel = function(x,y,...){
  #     panel.xyplot(x,y,...)
  #     panel.abline(h = 0, col=rgb(1,0,0), alpha=0.5,  lwd=20)
  #   }
  # )  
  #p <- addGrid(p)
  
  
  #return (addGrid(p))
  return(p)
}
  
test <- function(){

  df <- data.frame("y" = c(-10^seq(2,-3), 0, 10^seq(-3,2)))
  df$date <- seq(ISOdate(2020, 1,2), by="day", length.out=length(df$y))
  df$groupVar <- sprintf("group %d", 1:length(df$y) %%5)
  df$caseVar <- sprintf("case %d", 1:length(df$y %%7))
  print (df)
  #p <- symmetricPlot(x~date | caseVar, groupVector = df$groupVar, data=df)
  p <- symmetricPlot(y~date | 1 , groupVector = {1}, data=df)
  
  print(p)
}


#test()





