# central diference approx to dx/dy, 
# y is a date, we assume we are sorted in date order (ascending)
# group_id identifies series that are in order, do not calculate across changes in group id

library(lubridate)
library(dplyr)


centralDifference <- function (x,y, group_id){
  dx <- lead(x,1) - lag(x,1)
  dy <- lead(y,1) - lag(y,1)
  dy <- as.numeric(dy, units='days' )
  slope <- dx/dy
  breaks <- (group_id != lag(group_id,1)) | (lead(group_id,1) != group_id)
  slope <- ifelse(breaks, NA, slope)
  return (slope)
}