# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Election Forecasting/ElectionForecasting"), silent = TRUE)
try(setwd("C:/Users/Moritz/Desktop/ElectionForecasting"), silent = TRUE)

source("packages.R")

###################################

# Election statistics

# Get data Bundeswahlstatistik
# WÃ¤hler der Parteien nach Alter und Geschlecht
colnames <- c("Parties",
              "age18_25_2013", "age18_25_2009",
              "age26_35_2013", "age26_35_2009",
              "age36_45_2013", "age36_45_2009",
              "age46_60_2013", "age46_60_2009",
              "age61_69_2013", 
              "age60+_2013", "age60+_2009",
              "age70+_2013")

types <- replicate(13, "numeric")
types[1] <- "factor"

Wahlstatistik <- read.csv2("./Weitere Daten/Bundeswahlstatistik/Wählerschaft_der_Parteien_nach_Geschlecht_und_Alter.csv", 
                            sep = ";",dec =",", skip = 6, stringsAsFactors = FALSE)
Wahlstatistik <- Wahlstatistik[c(4:11,13:20,22:29),]
colnames(Wahlstatistik) <- colnames
Wahlstatistik$gndr <- c("Total", "Total", "Total", "Total", "Total", "Total", "Total", "Total",
                        "Male", "Male", "Male", "Male", "Male", "Male", "Male", "Male",
                        "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female")
Wahlstatistik <- select(Wahlstatistik, gndr, everything())
Wahlstatistik[3] <- as.numeric(sub(",", ".", unlist(Wahlstatistik[3]), fixed = TRUE))
Wahlstatistik[1] <- as.factor(unlist(Wahlstatistik[1]))
Wahlstatistik[2] <- as.factor(unlist(Wahlstatistik[2]))

save(Wahlstatistik, file = "Wahlstatistik.RData")

# Census data

