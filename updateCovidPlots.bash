#!/bin/bash
cd /home/fred/R/covid/covid
#do this via an r script to allow setting path to pandoc when called from crontab
#R -e "rmarkdown::render('CovidPlots.rmd')"
Rscript updateCovidPlots.r
git add .
git commit -m"update plots"
git push
