CovidPlots
================
28 June 2020 02:26 PM PDT

  - [Covid by County](#covid-by-county)
  - [](#section)
  - [](#section-1)
  - [\#\# CDC All Deaths by Week](#cdc-all-deaths-by-week)
  - [The CDC Flu View website gives weekly deaths by state going back
    several years. Data entry and reporting lags by many weeks, so the
    last month or so of deaths will be under-reported or
    missing.](#the-cdc-flu-view-website-gives-weekly-deaths-by-state-going-back-several-years.-data-entry-and-reporting-lags-by-many-weeks-so-the-last-month-or-so-of-deaths-will-be-under-reported-or-missing.)
  - [](#section-2)
  - [<https://gis.cdc.gov/grasp/fluview/mortality.html>](#httpsgis.cdc.govgraspfluviewmortality.html)
  - [](#section-3)
  - [`{r, cdc_all_deaths, echo=FALSE, results='hide',
    message=FALSE,warning=FALSE} # source("cdc_deaths.r") #
    print(p_all_deaths_recent) # print(p_year_on_year_linear) #
    print(p_year_on_year) #
    #`](#r-cdc_all_deaths-echofalse-resultshide-messagefalsewarningfalse-sourcecdc_deaths.r-printp_all_deaths_recent-printp_year_on_year_linear-printp_year_on_year)
  - [](#section-4)
  - [](#section-5)
  - [](#section-6)
  - [](#section-7)
  - [](#section-8)
  - [\#\# Covid in US States](#covid-in-us-states)
  - [Covid positive tests, deaths, and hospitalizations by state from
    covidtracking.com
    <https://covidtracking.com>](#covid-positive-tests-deaths-and-hospitalizations-by-state-from-covidtracking.com-httpscovidtracking.com)
  - [](#section-9)
  - [\#\#\# Hospitalizations](#hospitalizations)
  - [](#section-10)
  - [`{r, state_hospital, echo=FALSE, results='hide',
    message=FALSE,warning=FALSE} # source("covidtracking_us_states.r") #
    print (p_hosp) # print(p_hospPer)
    #`](#r-state_hospital-echofalse-resultshide-messagefalsewarningfalse-sourcecovidtracking_us_states.r-print-p_hosp-printp_hospper)
  - [](#section-11)
  - [\#\#\# Tests](#tests)
  - [](#section-12)
  - [`{r, state_positves, echo=FALSE, results='hide',
    message=FALSE,warning=FALSE} # # print(p_positives) #
    print(p_positive_fraction) # print(p_positivesPer) # print
    (p_deltaPositivesPer) # print(p_total_tests) #
    print(p_total_tests_per)
    #`](#r-state_positves-echofalse-resultshide-messagefalsewarningfalse-printp_positives-printp_positive_fraction-printp_positivesper-print-p_deltapositivesper-printp_total_tests-printp_total_tests_per)
  - [](#section-13)
  - [\#\#\# Deaths](#deaths)
  - [](#section-14)
  - [`{r, state_deaths, echo=FALSE, results='hide',
    message=FALSE,warning=FALSE} # # print(p_deaths_liner) #
    print(p_deaths) # print(p_deltaDeaths) # print(p_deathsPerLinear) #
    print (p_deathsPer) # print(p_deltaDeathsPer) #
    #`](#r-state_deaths-echofalse-resultshide-messagefalsewarningfalse-printp_deaths_liner-printp_deaths-printp_deltadeaths-printp_deathsperlinear-print-p_deathsper-printp_deltadeathsper)
  - [](#section-15)
  - [](#section-16)
  - [\#\# Covid in China](#covid-in-china)
  - [](#section-17)
  - [Covid data from Johns Hopkins CSSE via
    <https://covid-19.datasettes.com>](#covid-data-from-johns-hopkins-csse-via-httpscovid-19.datasettes.com)
  - [`{r, china, echo=FALSE, results='hide',
    message=FALSE,warning=FALSE} # source("covid_international.r") #
    print(p_china) # print(p_china_deaths)
    #`](#r-china-echofalse-resultshide-messagefalsewarningfalse-sourcecovid_international.r-printp_china-printp_china_deaths)
  - [](#section-18)
  - [\#\# Covid in Scandanavia](#covid-in-scandanavia)
  - [`{r, scandanavia, echo=FALSE, results='hide',
    message=FALSE,warning=FALSE} # print(p_scandanavia) #
    print(p_scandanavia_per) # print(p_scandanavia_per_log2) #
    print(p_scandanavia_deaths) # print(p_scandanavia_deaths_per_linear)
    # print(p_scandanavia_deaths_per_slope) #
    print(p_scandanavia_deaths_per) #
    print(p_scandanavia_deaths_per_log2)`](#r-scandanavia-echofalse-resultshide-messagefalsewarningfalse-printp_scandanavia-printp_scandanavia_per-printp_scandanavia_per_log2-printp_scandanavia_deaths-printp_scandanavia_deaths_per_linear-printp_scandanavia_deaths_per_slope-printp_scandanavia_deaths_per-printp_scandanavia_deaths_per_log2)

## Covid by County

Covid case counts and death counts by county, based on daily data from
NY Times via <https://covid-19.datasettes.com>
<img src="CovidPlots_files/figure-gfm/plot_counties-1.svg" width="4800" /><img src="CovidPlots_files/figure-gfm/plot_counties-2.svg" width="4800" /><img src="CovidPlots_files/figure-gfm/plot_counties-3.svg" width="4800" /><img src="CovidPlots_files/figure-gfm/plot_counties-4.svg" width="4800" />

# 

# 

# \#\# CDC All Deaths by Week

# The CDC Flu View website gives weekly deaths by state going back several years. Data entry and reporting lags by many weeks, so the last month or so of deaths will be under-reported or missing.

# 

# <https://gis.cdc.gov/grasp/fluview/mortality.html>

# 

# `{r, cdc_all_deaths, echo=FALSE, results='hide', message=FALSE,warning=FALSE} #   source("cdc_deaths.r") #   print(p_all_deaths_recent) #   print(p_year_on_year_linear) #   print(p_year_on_year) #  #`

# 

# 

# 

# 

# 

# \#\# Covid in US States

# Covid positive tests, deaths, and hospitalizations by state from covidtracking.com <https://covidtracking.com>

# 

# \#\#\# Hospitalizations

# 

# `{r,  state_hospital, echo=FALSE, results='hide', message=FALSE,warning=FALSE} #     source("covidtracking_us_states.r") #   print (p_hosp) #   print(p_hospPer) #`

# 

# \#\#\# Tests

# 

# `{r,  state_positves, echo=FALSE, results='hide', message=FALSE,warning=FALSE} #      #   print(p_positives) #   print(p_positive_fraction) #   print(p_positivesPer) #   print (p_deltaPositivesPer) #   print(p_total_tests) #   print(p_total_tests_per) #`

# 

# \#\#\# Deaths

# 

# `{r,  state_deaths, echo=FALSE, results='hide', message=FALSE,warning=FALSE} #      #   print(p_deaths_liner) #   print(p_deaths) #   print(p_deltaDeaths) #   print(p_deathsPerLinear) #   print (p_deathsPer) #   print(p_deltaDeathsPer) #      #`

# 

# 

# \#\# Covid in China

# 

# Covid data from Johns Hopkins CSSE via <https://covid-19.datasettes.com>

# `{r,  china, echo=FALSE, results='hide', message=FALSE,warning=FALSE} #     source("covid_international.r") #     print(p_china) #     print(p_china_deaths) #`

# 

# \#\# Covid in Scandanavia

# `{r,  scandanavia, echo=FALSE, results='hide', message=FALSE,warning=FALSE} #   print(p_scandanavia) #   print(p_scandanavia_per) #   print(p_scandanavia_per_log2) #   print(p_scandanavia_deaths) #   print(p_scandanavia_deaths_per_linear) #   print(p_scandanavia_deaths_per_slope) #   print(p_scandanavia_deaths_per) #   print(p_scandanavia_deaths_per_log2)`
