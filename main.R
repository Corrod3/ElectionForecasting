###############################################################################
## Election Forecasting 
## by: Alexander Sacharow & Moritz Hemmerlein
###############################################################################

###############################################################################
# CONTENT
# 0. Preparation
# 1. Get data
# 2. Data mining
# 3. Estimates
###############################################################################

###############################################################################
# To-Do / Discussion
# 1. What to do with respondents who claim that they are not eligible to vote?
# 2. What to do with votes who claim that they definitely don't vote?
# 3. Ideas for estimation and actual forecast?
# 4. Account for potential anti-incumbent bias, late swing, 
#     differential turnout bias, "don't knowers" bias and vote intention misreporting
# 5. How close is this survey to Forsa etc...? 
# 6. Attach value labels to coded data
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
## Das erste Argument in der sub function leuchtet mir nicht ein. Ziel ist es 
## das "X.question.." vor jedem variablen namen zu cutten oder?

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
    geom_bar(aes(y = (..count..)/sum(..count..))) + # bar type
  scale_y_continuous(labels=percent) + 
  ylab("Percentage of total respondents") +
  xlab("Voting behavior")

# filter people who are not eligble to vote (self reported)
DaliaDE <- filter(DaliaDE, vote_next_national_election != "I'm not eligible to vote")

# last election vote
ggplot(DaliaDE, aes(x=voted_party_last_election_de)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + # bar type
  coord_flip() + # flip sides
  scale_y_continuous(labels=scales::percent) + # percentages on y axis
  ylab("Share of total voters") +
  xlab("Parties")


# vote intention next election (BT 2017)
ggplot(DaliaDE, aes(x=vote_nextelection_de)) +
  geom_bar() + # bar type
  coord_flip() # flip sides

# Party ranking ################################################################
# comment to Dalia: Include in ranking: I prefer not to vote 
#                   -> allows to capture non-voters

# split ranking
# test with one string
# strsplit("Die Linke | BÃ¼ndnis 90 / Die GrÃ¼nen | SPD â€“ Sozialdemokratische Partei Deutschlands | FDP - Freie Demokratische Partei | CDU/CSU â€“ Christlich Demokratische Union/Christlich Soziale Union | AfD â€“ Alternative fÃ¼r Deutschland"
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
# hier nochmal kurz gemeinsam ransetzen und den code verstehen; was genau macht "function(x)"?

# most preferred party (consistency check with vote for next election/identify divergencies?)
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

# transition plot (see transistion plot script for example)
transitionPlot(as.matrix(SecPref[,-1]),  
               box_txt = as.character(SecPref[,1]))

# plot shows not really transitions but second preferences conditional on what the first preference was
# Transition more interesting for the loyalty considerations

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
rownames(VoteLast) <- c("AfD", "Die Grünen", "Union", "Die Linke", "FDP", "SPD")
colnames(VoteLast) <- c("", "AfD", "Die Grünen", "Union", "Die Linke", "FDP", "Will not vote", "Other", "SPD")
# order rows
PartyOrder <- c("Union", "SPD", "Die Grünen", "Die Linke", "FDP", "AfD")
VoteLast <- VoteLast[PartyOrder,]

# order columns
VoteLast <- VoteLast[,c(2:ncol(VoteLast))]
VoteLast <- VoteLast[,c(PartyOrder, "Will not vote", "Other")]

# Dein code hat aus irgendeinem Grund die AfD in den Spalte rausgekegelt. 
# Habs nochmal neu gemacht, auch zur Übung.

# loyality: last vote = next vote / total respondents per party (last election)
VoteLast$loyality <- diag(as.matrix(VoteLast))/rowSums(VoteLast)

# Table for 2013 -> 2017 voter transitions 
# transition plot (see transistion plot script for example)
transitionPlot(as.matrix(VoteLast[,c(1:6)]),  
               box_txt = PartyOrder)

###############################################################################
# 3. Estimates
###############################################################################

# Potentially interesting
# test impact of demographics; opinion; social status, education etc...
# e.g
# Model: vote_next_national_election = b1*demographics + b2*education
# -> adjust weighting of vote intent/forecast according to demographic group 
# probability to go to the polls

