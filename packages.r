
# install packages from CRAN
p.needed <- c("readr", # imports spreadsheet data (Wickham)
              "readxl", # imports .xls .xlsx(Wickham)
              "haven", # imports SPSS, Stata and SAS files (Wickham)
              "magrittr", #  for piping (Wickham)
              "plyr", # for consistent split-apply-combines
              "dplyr",  # provides data manipulating functions (Wickham)
              "stringr", # for string processing (Wickham)
              "ggplot2", # for graphics (Wickham)
              "gridExtra", # place plots next each other
              "tidyr", # for tidying data frames (Wickham)
              "forcats",  # conveniently handling factors with fct_recode etc.
              "purrr", # pure and completing function programming package
              "httr", # talking to web APIs
              "scales", # supports scaling for graphs
              "broom", # for tidying model output (Dave Robinson)
              "janitor", # for basic data tidying and examinations
              "reshape2", # reshape data(Wickham)
              "xtable", # generate table output
              "directlabels", # add direct labels to plots
              "stargazer", # generate nice model table
              "babynames", # dataset compiled by Hadley Wickham; 
              "nycflights13", # data on 336776 flights departing from NYC in 2013
              "lubridate", # handling dates of all kinds
              "maps", "maptools", "ggmap",
              "sjPlot", "sjmisc",
              "survey",
              "rvest", # whickham for web scraping
              "data.table", # package for very large data (faster than other packages)
              "Metrics")  # for useful functions and machine learning

# install packages which are not in installed.packages()
packages <- rownames(installed.packages())
p.to.install <- p.needed[!(p.needed %in% packages)]
if (length(p.to.install) > 0) {
  install.packages(p.to.install)
  }

# load all p_needed
lapply(p.needed, require, character.only = TRUE)

rm(packages,p.needed, p.to.install)

