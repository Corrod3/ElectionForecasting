# Get and clean election data


# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Election Forecasting/ElectionForecasting"), silent = TRUE)
try(setwd("C:\\Users\\Moritz\\Desktop\\ElectionForecasting"), silent = TRUE)

# Collect packages/libraries we need:
packages <- c("readxl", "dplyr", "ggplot2", "tidyr" ,"reshape2", "scales")
# package and why it is needed
# readxl: import excel files
# dyplyr: data manipulation
# ggplot: plots (e.g. density)
# tidyr: spread function
# reshape2: melt function
# scales: label transformation in ggplot
# survey: tools for survey weighting and post-stratification
# sjPlot: nice alternative plots

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

###################################

# Get data
colnames <- c("Parties",
              "age_1_13", "age_1_09",
              "age_2_13", "age_2_09",
              "age_3_13", "age_3_09",
              "age_4_13", "age_4_09",
              "age_5_13", 
              "age_6_13", "age_6_09",
              "age_7_13")


Wahlstatistik <- read.table("C:\\Users\\Moritz\\Desktop\\ElectionForecasting\\Weitere Daten\\Bundeswahlstatistik\\Wählerschaft_der_Parteien_nach_Geschlecht_und_Alter.csv", 
                            sep = ";",dec =".", skip = 6)
row
