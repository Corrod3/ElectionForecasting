###############################################################################
## Election Forecasting 
## by: Alexander Sacharow & Moritz Hemmerlein
###############################################################################

###############################################################################
# CONTENT
# 0. Preparation
# 1. get data
# 2. data mining
###############################################################################

###############################################################################
# ToDo / discussion
# 1. what to do with respondents who claim that they are not eligible to vote?
# 2. What to do with votes who claim that they definitely don't vote?
#
###############################################################################

###############################################################################
# 0. Preparations
###############################################################################

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

# load csv
Dalia <- read.csv("Raw\\coded_csv\\data_coded_e28.csv - data_coded_e28.csv")

# change data type to character and factor
Dalia[1]<-as.character(unlist(Dalia[1]))
for(i in 5:ncol(Dalia)) {
  Dalia[i]<-as.factor(unlist(Dalia[i]))
}

# rename columns to remove prefix
names(Dalia)<-sub(".*\\.\\.(.+)", "\\1", names(Dalia))

# types for excel import
types<-replicate(90, "text")
types[2]<-"numeric"
types[4]<-"numeric"

# excel file import
# note: it includes the level names, weil the csv is coded (and its a lot of 
# work to include the levels)
Dalia1<-read_excel("Raw/Dalia_research_challenge_europulse.xlsx",sheet=1, 
                   col_types=types, na="NA")

# change types
Dalia1[3]<-as.factor(unlist(Dalia1[3]))
for(i in 5:ncol(Dalia1)) {
  Dalia1[i]<-as.factor(unlist(Dalia1[i]))
}

# change column names
names(Dalia1)<-sub("\\[.+\\] (.+)", "\\1", names(Dalia1))

###############################################################################
# 2. data mining
###############################################################################

DaliaDE <- filter(Dalia1, country_code == "DE")

# vote participation intention (turnout prediction?)
ggplot(DaliaDE, aes(x=vote_next_national_election)) +
    geom_bar() # bar type

# filter people who are not eligble to vote (self reported)
DaliaDE <- filter(DaliaDE, vote_next_national_election != "I'm not eligible to vote")

# last election vote
ggplot(DaliaDE, aes(x=voted_party_last_election_de)) +
  geom_bar() + # bar type
  coord_flip() # flip sides

# vote intention next election (BT 2017)
ggplot(DaliaDE, aes(x=vote_nextelection_de)) +
  geom_bar() + # bar type
  coord_flip() # flip sides

# Party ranking ################################################################
# comment to Dalia: Include in ranking: I prefer not to vote 
#                   -> allows to capture non-voters

# split ranking
# test with one string
# strsplit("Die Linke | Bündnis 90 / Die Grünen | SPD – Sozialdemokratische Partei Deutschlands | FDP - Freie Demokratische Partei | CDU/CSU – Christlich Demokratische Union/Christlich Soziale Union | AfD – Alternative für Deutschland"
#          , split = " | ", fixed = TRUE)

# list of all splitted strings
DaliaDE$ranking1 <- sapply(strsplit(as.character(DaliaDE$ranking_party_de), 
                                    split = " | ", fixed = TRUE),
                           function(x) x[[1]])
DaliaDE$ranking2 <- sapply(strsplit(as.character(DaliaDE$ranking_party_de), 
                                    split = " | ", fixed = TRUE),
                           function(x) x[[2]])
DaliaDE$ranking3 <- sapply(strsplit(as.character(DaliaDE$ranking_party_de), 
                                    split = " | ", fixed = TRUE),
                           function(x) x[[3]])
DaliaDE$ranking4 <- sapply(strsplit(as.character(DaliaDE$ranking_party_de), 
                                    split = " | ", fixed = TRUE),
                           function(x) x[[4]])
DaliaDE$ranking5 <- sapply(strsplit(as.character(DaliaDE$ranking_party_de), 
                                    split = " | ", fixed = TRUE),
                           function(x) x[[5]])
DaliaDE$ranking6 <- sapply(strsplit(as.character(DaliaDE$ranking_party_de), 
                                    split = " | ", fixed = TRUE),
                           function(x) x[[6]])

# vote intention next election (BT 2017)
ggplot(DaliaDE, aes(x=ranking1)) +
  geom_bar() + # bar type
  coord_flip() # flip sides

# How voters 1 to 2nd preferences are related
# plot function for flow chart
source("transitionplot.R")

# generate data frame relating rank1 to rank2 for transtion pot
SecPref <- as.data.frame(table(rank1 = DaliaDE$ranking1, 
                               rank2 = DaliaDE$ranking2))
SecPref <- spread(SecPref, rank2, Freq)

# transition plot (see transistion plot scriot for example)
transitionPlot(as.matrix(SecPref[,-1]),  
               box_txt = as.character(SecPref[,1]))

# voter loyality ##############################################################
# idea: compare past self-reported voting with intended voting?!
# (how about non-voters / too young voters)

VoteLast <- as.data.frame(table(
                  last.vote = DaliaDE$voted_party_last_election_de,
                  next.vote = DaliaDE$vote_nextelection_de))
VoteLast <- spread(VoteLast, next.vote, Freq)

# remove non-voters from last election (not relevant for loyality consideration)
VoteLast <- VoteLast %>% 
              filter(last.vote != "I wanted to vote but I wasn't able to" &
                     last.vote != "Yes, but I voted for another party" & 
                     last.vote != "No, I did not vote")

# move first column to row names
rownames(VoteLast) <- sub(VoteLast[,1], pattern = "Yes, I voted for ", replacement = "")

# order columns
VoteLast <- VoteLast %>% select(contains("AfD"), contains("Grünen"), 
                                contains("CDU"), contains("Linke"),
                                contains("FDP"), contains("SPD"),
                                contains("not vote"), contains("Other"))
# loyality: last vote = next vote / total respondents per party (last election)
VoteLast$loyality <- diag(as.matrix(VoteLast))/rowSums(VoteLast)