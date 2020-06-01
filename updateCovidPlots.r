# generate all of the covid plots, setting the pandoc path so it can be seen when run from crontab
library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC = "/usr/local/bin/pandoc")
rmarkdown::render('CovidPlots.rmd')
