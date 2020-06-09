#!/bin/bash
# generate and upload all of the covid plots
cd /home/fred/R/covid/covid
#Explicitly set pandoc path so that this will work when called from cron job
R -e "Sys.setenv(RSTUDIO_PANDOC = '/usr/lib/rstudio/bin/pandoc'); rmarkdown::render('CovidPlots.rmd')"
git add .
git commit -m"update plots"
git push
