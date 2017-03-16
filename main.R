###############################################################################
## Election Forecasting 
## by: Alexander Sacharow & Moritz Hemmerlein
###############################################################################

###############################################################################
# CONTENT
# 0. Preparation
###############################################################################

###############################################################################
# 0. Preparations
###############################################################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Election Forecasting/ElectionForecasting"), silent = TRUE)
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Master thesis/SecurityPolicyForecastingTournament"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "dplyr", "ggplot2", "reshape2", "scales")
# package and why it is needed
# readxl: import excel files
# dyplyr: data manipulation
# ggplot: plots (e.g. density)
# reshape2: melt function
# scales: label transformation in ggplot

# install packages if not installed before
for (p in packages) {
  if (p %in% installed.packages()[,1]) {
    require(p, character.only=T)
  }
  else {
    install.packages(p, repos="http://cran.rstudio.com", dependencies = TRUE)
    require(p, character.only=T)
  }
}
rm(p, packages)

###############################################################################
# 1. get data
###############################################################################


