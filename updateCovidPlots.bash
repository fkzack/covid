#!/bin/bash
cd /home/fred/R/covid/covid
R -e "rmarkdown::render('CovidPlots.rmd')"
git add .
git commit -m"update plots"
git push
