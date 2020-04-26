# Wrappers around xyplot to print covid plots in my format

library(devtools)
install_github("fkzack/FredsRUtils", type="source")
library(FredsRUtils)
library(lattice)
library(latticeExtra)
library(lubridate)        






# decide what kind of axis to use and figure out the tick/grid intervals and locations
# x is the data being plotted
# log base is the log to use for lo3g plots, or FALSE for linear
# numTickIntervals is the approximate number if spaces to have between major ticks
# returns a list
#   ticksAt, the location of each tick in linear (original) coordinates
#   majors, locations of major grid lines in log coordinates
#   minors, locations of major grid lines in log coordinates
# minors will be null for non log10 axis, may later add minors to some date axis if I have time

axisTicks <- function(x,logBase, numTickIntervals){
  ticksAt <- NULL
  minors <- NULL
  majors <- NULL
  format <- NULL
  if (is.instant(x)){
    if (max(x, na.rm = TRUE) - min(x, na.rm = TRUE) > 240){
      t<- FredsRUtils::monthly_ticks(x, numIntervals = numTickIntervals)
    } else {
      t<- FredsRUtils::weekly_ticks(x, numIntervals = numTickIntervals)
    }
    majors <- t$majors
    minors <- t$minors
    ticksAt <- majors
    format <- "%Y-%m-%d"
  } 
  else if (logBase!=FALSE){
    t <- log_ticks(x, base=logBase)
    minors <- t$minors
    majors <- t$majors
    ticksAt <- logBase ^ t$majors
  }
  else if(is.numeric(x)){
    t  <- FredsRUtils::linear_ticks(x, numIntervals = numTickIntervals)
    majors <- t$majors
    minors <- t$minors
    ticksAt <- majors
  } 
  
  return(list(ticksAt=ticksAt, majors=majors, minors=minors, format=format))
}


#Plot covid data in my format
#Parameters are same as for xyplot, except that 
#   * I have not yet implemented subset as an argument
#   * logX, logY determine if the axis are log or not (FALSE = linear, True = Log10, N = LogN)
#   * formatX, formatY are the format to apply to axis ticks
#   * numTickIntervalsX, numTickIntervalsY are the approximate number of intervals between tick marks
#    
covidPlot <- function(formula1, data, groups=NULL,  
                      logX = FALSE, logY = TRUE, 
                      subtitle = "", 
                      xlab=NULL, numTickIntervalsX = 5, formatX=NULL,
                      ylab=NULL,  numTickIntervalsY=5, formatY=NULL,
                      allow.multiple = is.null(groups) || outer,
                      outer = !is.null(groups),
                      drop.unused.levels = lattice.getOption("drop.unused.levels"), 
                      ...){
  
  
  if (logX==TRUE){
    logX <- 10
  }
  if (logY==TRUE){3
    logY <- 10
  }
  
  
  
  #get_all_vars gives every column used by the formula in its original form
  gav <- get_all_vars(formula1, data)
  print(gav)
  
  #latticeParseFormula gives the results of applying the formula
  #lpf <- latticeParseFormula(formula1, data=data)
  formula <- formula1
  groups <- eval(substitute(groups), data, environment(formula1))
  lpf <- latticeParseFormula(formula, data=data, groups = groups, multiple = allow.multiple, 
                             outer = outer,  subscripts = TRUE,
                             drop = drop.unused.levels)
  #print (lpf$left)
  #print (lpf$right)
  #print(lpf$condition)
  if (is.null(lpf$condition)){
    lpf$condition <- ""
  }
  if(is.null(lpf$groups)){
    lpf$groups <- ""
  }
  
  #should probably do something similar to groups handling above for subset, but I don't use the subset parameter so
  #have not yet tried to implement this
  
  
  df <- data.frame(lpf$right, lpf$left, unlist(lpf$condition, use.names = FALSE), lpf$groups)
  names(df) <- c('x', 'y', 'condition', 'groups')
  if (is.null(xlab)) {
    xlab <- lpf$right.name
  }
  if (is.null(ylab)) {
    ylab <- lpf$left.name
  }
  
  
  xTicks <-axisTicks(df$x, logX, numTickIntervalsX)
  yTicks <- axisTicks(df$y, logY, numTickIntervalsY)
  
  if (is.null(formatX)){
    formatX <- xTicks$format
  }

  if (is.null(formatY)){
    formatY <- xTicks$formatY
  }

  
  p <- xyplot(y~x | condition,  data = df, groups = groups,
              xTicks=xTicks, yTicks=yTicks,
              scales=list(
                x=list(rot=45, at=xTicks$ticksAt, format=formatX, log=logX),
                y=list(at=yTicks$ticksAt, log=logY, format=formatY)),
              sub=list(label=paste(subtitle, "   "), cex=0.5, x=1, just="right"),
              par.strip.text=list(cex=0.75),
              type=c('p', 'l'),
              as.table=TRUE,
              xlab = xlab,
              ylab=ylab,
 
              panel=function(x,y, subscripts,xTicks,  groups, yTicks, ...){
                panel.abline(h=yTicks$majors, alpha=0.1)
                panel.abline(h=yTicks$minors, alpha=0.05)
                panel.abline(v=xTicks$majors, alpha=0.1)
                panel.abline(v=xTicks$minors, alpha=0.05)
                panel.xyplot(x,y,subscripts=subscripts,groups=groups, ...)
              },
              
              ...)
  return (p)
}

testCovidPlot <- function (){
  df <- data.frame("x" = -10:10)
  df$y <- 10^ df$x
  df$z <- 1:length(df$x) %% 5
  df$t <- seq(ISOdate(2020, 4,1,tz=""), by = "5 days", length.out = length(df$x))
  print(df)
  p <- covidPlot(df$y~t, data=df, group=z, logY = 10, numTickIntervalsY = 10, 
                 xlab = "x label", ylab = "y label")
  print(p)
  #xyplot(y~t, data=df, groups = z)
}
# testCovidPlot()


# s1 <- seq(ISOdate(2020, 1,11), by="day", length.out=200)
# df <- data.frame(date=s1)
# df$count <- 1:length(df$date)
# df$group <- df$count %% 14
# df$x <- 10^((df$count + df$group)/100)
# p <- covidPlot(x~date | group, data=df)
# 
# print(p)




# Create a "symmetric log plot", with postive values on a log plot going up from zero, zero values at 0, 
# and negitive values as a plot of log of absolute error going down from zero
# Parameters are same as for xyplot, except that 
#   * I have not yet implemented subset as an argument
#   * logX, logY determine if the axis are log or not (FALSE = linear, True = Log10, N = LogN)
#   * formatX, formatY are the format to apply to axis ticks
#   * numTickIntervalsX, numTickIntervalsY are the approximate number of intervals between tick marks


symmetricPlot <- function(formula1, data,   groups=NULL, 
                          subtitle = "", numTickIntervals = 5, 
                          xlab=NULL, numTickIntervalsX = 5, formatX=NULL,
                          ylab=NULL, formatY=NULL,
                          allow.multiple = is.null(groups) || outer,
                          outer = !is.null(groups),
                          drop.unused.levels = lattice.getOption("drop.unused.levels"), 
                          ...){
  
  formula <- formula1
  groups <- eval(substitute(groups), data, environment(formula1))
  lpf <- latticeParseFormula(formula, data=data, groups = groups, multiple = allow.multiple, 
                             outer = outer,  subscripts = TRUE,
                             drop = drop.unused.levels)
  print (lpf$left)
  print (lpf$right)
  print(lpf$condition)
  if (is.null(lpf$condition)){
    lpf$condition <- ""
  }
  
  df <- data.frame(lpf$right, lpf$left, unlist(lpf$condition, use.names = FALSE), lpf$groups)
  names(df) <- c('x', 'y', 'condition', 'groups')
  if (is.null(xlab)) {
    xlab <- lpf$right.name
  }
  if (is.null(ylab)) {
    ylab <- lpf$left.name
  }
  
  
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
  
  yTicks <- c(negTicks_dv, 0, posTicks_dv)
  #print("ticks:")
  #print (ticksAt)
  tickLabels <- c(-10^negTicks_y, 0,10^posTicks_y )
  #print(tickLabels)
  
  #x axis
  xTicks <- date_ticks(df$x,numTickIntervals, 0)
 
  
  #print(str(df))
  p <- xyplot(df$dv~df$x | df$condition, groups=groups, 
              scales=list(y=list(at=yTicks, labels=tickLabels),
                          x=list(at=xTicks$majors,rot=45, format="%Y-%m-%d" )),
              xTicks=xTicks, yTicks=yTicks,
              xlab=xlab, ylab=ylab,
              sub=list(label=paste(subtitle, "   "), cex=0.5, x=1, just="right"),
              par.strip.text=list(cex=0.75),
              type=c('p', 'l'),
              as.table=TRUE,
              panel=function(x,y, subscripts,xTicks, yTicks, groups, ...){
               panel.abline(h = 0, col=rgb(0,0,0), alpha=0.1,  lwd=20)
                panel.abline(h=yTicks, alpha=0.1)
                panel.abline(v=xTicks$majors, alpha=0.1)
                panel.abline(v=xTicks$minors, alpha=0.05)
                panel.xyplot(x,y,subscripts=subscripts, groups=groups, ...)
              },
              
              ...)
  return(p)
}

testSymmetric <- function(){
  
  df <- data.frame("y" = c(-10^seq(2,-3), 0, 10^seq(-3,2)))
  df$date <- seq(ISOdate(2020, 1,2), by="day", length.out=length(df$y))
  df$groupVar <- sprintf("group %d", 1:length(df$y) %%5)
  df$caseVar <- sprintf("case %d", 1:length(df$y %%7))
  print (df)
  #p <- symmetricPlot(x~date | caseVar, groupVector = df$groupVar, data=df)
  p <- symmetricPlot(y~date | 1 , groups=groupVar, data=df)
  
  print(p)
}
# testSymmetric()


#testLinear()
#test()





