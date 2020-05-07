#NOT WORKING
#will eventually use for a comparison to covid deaths


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
library(lubridate)


nhtsa_far <- "https://crashviewer.nhtsa.dot.gov/CrashAPI/FARSData/GetFARSData?dataset=Accident&caseYear=2014&format=json"

far <- fromJSON(nhtsa_far)
