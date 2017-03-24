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
# 1. Cleaning sample
#      a. Non-voters
#      b. non-citizen
#      c. too young (17 or 18?)
#      d. not able
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
## Regex syntax
## .* steht fuer beliebiges Zeichen beliebig oft
## \\. für punkt 
## .+ ?
## () als platzhalter, der mit \\1 aufgerufen werden kann

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

# dataset with german users
DaliaDE <- Dalia1 %>% filter(country_code == "DE")

# change values
DaliaDE$vote_nextelection_de <- plyr::mapvalues(DaliaDE$vote_nextelection_de, 
          from = c("AfD – Alternative fur Deutschland", "Bündnis 90 / Die Grünen",
                   "CDU/CSU – Christlich Demokratische Union/Christlich Soziale Union",
                   "Die Linke", "FDP - Freie Demokratische Partei", 
                   "SPD – Sozialdemokratische Partei Deutschlands", "Other", 
                   "I would not vote"),
          to = c("AfD", "Gruenen", "Union", "Linke", "FDP", "SPD", 
                 "Other", "No vote"))
# for the ones where it dit not work
levels(DaliaDE$vote_nextelection_de) <- sub("AfD.*", "AfD", levels(DaliaDE$vote_nextelection_de))
levels(DaliaDE$vote_nextelection_de) <- sub("CDU.*", "CDU", levels(DaliaDE$vote_nextelection_de))
levels(DaliaDE$vote_nextelection_de) <- sub("SPD.*", "SPD", levels(DaliaDE$vote_nextelection_de))

DaliaDE$voted_party_last_election_de <- plyr::mapvalues(DaliaDE$voted_party_last_election_de, 
     from = c("I wanted to vote but I wasn't able to", 
              "No, I did not vote", 
              "Yes, but I voted for another party",
              "Yes, I voted for AfD – Alternative für Deutschland",
              "Yes, I voted for Bündnis 90 / Die Grünen",
              "Yes, I voted for CDU/CSU – Christlich Demokratische Union/Christlich Soziale Union",
              "Yes, I voted for Die Linke", "Yes, I voted for FDP - Freie Demokratische Partei", 
              "Yes, I voted for SPD – Sozialdemokratische Partei Deutschlands"),
     to = c("Not able", "No vote", "Other" ,"AfD", "Gruenen", "Union", 
            "Linke", "FDP", "SPD"))
# correct the errors
levels(DaliaDE$voted_party_last_election_de) <- sub(".*AfD.*", "AfD", levels(DaliaDE$voted_party_last_election_de))
levels(DaliaDE$voted_party_last_election_de) <- sub(".*CDU.*", "CDU", levels(DaliaDE$voted_party_last_election_de))
levels(DaliaDE$voted_party_last_election_de) <- sub(".*SPD.*", "SPD", levels(DaliaDE$voted_party_last_election_de))


###############################################################################
# 2. data mining
###############################################################################

# vote participation intention (turnout prediction?)
ggplot(DaliaDE, aes(x=vote_next_national_election)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) + # bar type
  scale_y_continuous(labels=percent) + 
  ylab("Percentage of total respondents") +
  xlab("Voting behavior")

plot(DaliaDE$residency)

# filter people who are not eligble to vote
# 17 year olds will likely turn 18 by the time of the election
DaliaDE <- filter(DaliaDE, 
                  vote_next_national_election != "I'm not eligible to vote" & 
                  residency == "Yes, as a citizen" & 
                  age > 16)
# last election vote
ggplot(filter(DaliaDE,  
              voted_party_last_election_de != "No vote" & 
              voted_party_last_election_de != "Not able"), 
       aes(x=voted_party_last_election_de)) +
  geom_bar(aes(y = (..count..)/sum(..count..), label = (..count..)/sum(..count..))) + # bar type
  coord_flip() + # flip sides
  scale_y_continuous(labels=scales::percent) + # percentages on y axis
  ylab("Share of total voters") +
  xlab("Parties") +
  theme_bw()


# vote intention next election (BT 2017)
ggplot(filter(DaliaDE,vote_nextelection_de != "I would not vote" ),
       aes(x=vote_nextelection_de)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + # bar type
  coord_flip() + # flip sides
  scale_y_continuous(labels=scales::percent) + # percentages on y axis
  ylab("Share of total voters") +
  xlab("Parties") 

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
              filter(last.vote != "Not able" &
                     last.vote != "Other" & 
                     last.vote != "No vote")

# move first column to row names
# Gibt hier ein problem mit der Schriftcodierung (Sonderzeichen). 
rownames(VoteLast) <- c("AfD", "Gruenen", "CDU", "Linke", "FDP", "SPD")
colnames(VoteLast) <- c("", "AfD", "Gruenen", "CDU", "Linke", "FDP",
                        "Will not vote", "Other", "SPD")
# order rows
PartyOrder <- c("CDU", "SPD", "Gruenen", "Linke", "FDP", "AfD")
VoteLast <- VoteLast[PartyOrder,]
VoteLast <- VoteLast[,PartyOrder]

# loyality: last vote = next vote / total respondents per party (last election)
VoteLast$loyality <- diag(as.matrix(VoteLast))/rowSums(VoteLast)

# Table for 2013 -> 2017 voter transitions 
# transition plot (see transistion plot script for example)
# Weil die Loyalitätsstroeme so fett sind lassen die anderen sich kaum unterscheiden :/
transitionPlot(as.matrix(VoteLast[,c(1:6)]),  
               box_txt = PartyOrder)

# alternative voter loyality: certainty_party_to_vote

ggplot(filter(DaliaDE, vote_nextelection_de != "I would not vote"),
       aes(x = vote_nextelection_de, fill = certainty_party_to_vote)) +
  geom_bar() +
  coord_flip() # flip sides

# likelihood to vote
ggplot(filter(DaliaDE, vote_nextelection_de != "I would not vote"),
       aes(x = vote_nextelection_de, fill = vote_next_national_election)) +
  geom_bar() +
  coord_flip() # flip sides

###############################################################################
# 3. Estimates
###############################################################################

# Potentially interesting
# test impact of demographics; opinion; social status, education etc...
# e.g
# Model: vote_next_national_election = b1*demographics + b2*education
# -> adjust weighting of vote intent/forecast according to demographic group 
# probability to go to the polls

# weights #####################################################################
# Careful: 4 years difference between Exit polls and Dalia!!!
DaliaDE$age.gr <- c("<18", "18-29", "30-44", 
                    "45-59", "60+")[findInterval(DaliaDE$age , 
                                                 c(-Inf, 17.5, 29.5, 44.5,59.5, Inf))]

# for clustering
plyr::count(DaliaDE, c('gender','age.gr','voted_party_last_election_de'))

DaliaDE %>% group_by(gender, age.gr, voted_party_last_election_de) %>% tally()
