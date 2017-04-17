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

# 6. Zensus/Bundeswahldatenstatistik/Forschungsgruppe Table zu kumulativen H?ufigkeiten
# -> In excel und dann in R
# 7. 
# 8. Exit polls sch?n und gut. Aber was ist mit Briefw?hlern? Deutlich andere demgraphische
# Struktur? Wie hoch ist ihr Anteil?
###############################################################################

###############################################################################
# 0. Preparations
###############################################################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Election Forecasting/ElectionForecasting"), silent = TRUE)
try(setwd("C:\\Users\\Moritz\\Desktop\\ElectionForecasting"), silent = TRUE)

source("packages.R")

###############################################################################
# 1. get data + cleaning
###############################################################################

# Import Dalia data coded
DaliaC <- read.csv("Raw\\coded_csv\\data_coded_e28.csv - data_coded_e28.csv")

# change data type to character and factor
DaliaC[1] <- as.character(unlist(DaliaC[1]))
for(i in 5:ncol(DaliaC)) {
  DaliaC[i] <- as.factor(unlist(DaliaC[i]))
}

# rename columns to remove prefix (using regex)
names(DaliaC) <- sub(".*\\.\\.(.+)", "\\1", names(DaliaC))

# types for excel import
types <- replicate(90, "text")
types[2] <- "numeric"
types[4] <- "numeric"

#Import dalia data as strings
DaliaS <- read_excel("Raw/Dalia_research_challenge_europulse.xlsx",sheet=1, 
                   col_types=types, na="NA")

# change types
DaliaS[3] <- as.factor(unlist(DaliaS[3]))
for(i in 5:ncol(DaliaS)) {
  DaliaS[i] <- as.factor(unlist(DaliaS[i]))
}

# change column names
names(DaliaS) <- sub("\\[.+\\] (.+)", "\\1", names(DaliaS))

# dataset with german users/sorting
DaliaDE <- DaliaS %>% filter(country_code == "DE") %>% arrange(age, gender, degree_of_urbanisation, education_level)

# Create easy identifier
DaliaDE$identifier <- c(1:nrow(DaliaDE))
DaliaDE$uuid <- NULL    # Drops uuid
DaliaDE <- select(DaliaDE, identifier, weight, country_code, age, gender, degree_of_urbanisation, education_level, everything())
# weights vary across four demographic variables  

# Rename label (order in parties vector must equal order in vote_nextelection_de)
parties <- c("AfD", "Gruenen", "Union", "Linke", "FDP", "No vote", "Other", "SPD")
levels(DaliaDE$vote_nextelection_de) <- parties

label_temp <- c("Not able", "No vote", "Other" ,"AfD", "Gruenen", "Union", "Linke", "FDP", "SPD")
levels(DaliaDE$voted_party_last_election_de) <- label_temp
rm(label_temp)

levels(DaliaDE$education_level) <- c("High school or equivalent", 
                                     "No formal education", 
                                     "University or equivalent",
                                     "Some high school or secondary school",
                                     "Other/Rather not answer")

DaliaDE$edu.cat <- ifelse(
        DaliaDE$education_level == "University or equivalent", 
        "Higher education", 
        DaliaDE$education_level) 
DaliaDE$edu.cat <- ifelse(
        DaliaDE$education_level == "No formal education",
        "Lower education",
        DaliaDE$edu.cat)
DaliaDE$edu.cat <- ifelse(
      DaliaDE$education_level == "Other/Rather not answer",
      "Lower education",
      DaliaDE$edu.cat)
DaliaDE$edu.cat <- ifelse(
      DaliaDE$education_level == "High school or equivalent",
      "Medium education",
      DaliaDE$edu.cat)
DaliaDE$edu.cat <- ifelse(
      DaliaDE$education_level == "Some high school or secondary school",
      "Medium education",
      DaliaDE$edu.cat)
DaliaDE$edu.cat <- factor(DaliaDE$edu.cat, levels = c("Lower education",
                                                                  "Medium education",
                                                                  "Higher education"))


# Filter: not eligible to vote, not german resident, below 18 at the time of election
DaliaDE <- filter(DaliaDE, 
                  vote_next_national_election != "I'm not eligible to vote" & 
                    residency == "Yes, as a citizen" & 
                    age > 16)
# 255 observations drop out

###############################################################################
# 2. data mining
###############################################################################

# residency
plot(DaliaDE$residency)

# create binary turnout variable
DaliaDE$turnout_exp <- DaliaDE$vote_next_national_election
DaliaDE$turnout_exp <- plyr::mapvalues(DaliaDE$turnout_exp, from = c("I'm not eligible to vote","No, I will definitely not vote", 
                                                                   "No, I will probably not vote",
                                                                   "Yes, I will definitely vote",
                                                                   "Yes, I will probably vote") , 
                                                             to = c(0,0,0,1,1))

### Tables ###
# Set general table setting

sjp.setTheme(geom.outline.color = "cadetblue",
             geom.label.size = 3.3,
             geom.label.color = "black",
             title.color = "black", 
             title.size = 1.3, 
             axis.angle.x = 45, 
             axis.textcolor = "black",
             base = theme_classic())

# Self-reported turnout
sjp.xtab(DaliaDE$turnout_exp, DaliaDE$gender,
         title = "Expected turnout by gender", 
         axis.titles = c("Self-reported turnout by gender", "Expected federal election turnout"),
         axis.labels = c("No, I will (probably) not vote", "Yes, I will (probably) vote"),
         geom.size = 0.5, 
         geom.colors = c("cadetblue","cadetblue3") ,
         legend.title = "Gender", 
         legend.labels = c("Male", "Female"), 
         coord.flip=TRUE, 
         hjust = "top", 
         show.total = FALSE,
         show.n = FALSE)
        
# with Dalia weights
sjp.xtab(DaliaDE$turnout_exp, DaliaDE$gender, weight.by=DaliaDE$weight,
         title = "Expected turnout by gender (unweighted)", 
         axis.titles = c("Vote intent by gender","Expected federal election turnout"),
         axis.labels = c("No, I will (probably) not vote", "Yes, I will (probably) vote"),
         geom.size = 0.5, legend.title = "Gender", legend.labels = c("Male", "Female"), 
         coord.flip=TRUE, hjust = "top",show.n = FALSE)

# why are the turnouts so high? What can we do about that?
# probably (self-)selection bias within demographic groups 

#last election vote

#ggplot(filter(DaliaDE,  
#              voted_party_last_election_de != "No vote" & 
#              voted_party_last_election_de != "Not able"), 
#       aes(x=voted_party_last_election_de)) +
#  geom_bar(aes(y = (..count..)/sum(..count..))) + # bar type
#  coord_flip() + # flip sides
#  scale_y_continuous(labels=scales::percent) + # percentages on y axis
#  ylab("Share of total voters") +
#  xlab("Parties") +
#  theme_bw()

# filter not vote and not able
DaliaDE_temp <- filter(DaliaDE, !(voted_party_last_election_de == "No vote" | voted_party_last_election_de == "Not able"))
DaliaDE_temp$voted_party_last_election_de <- factor(DaliaDE_temp$voted_party_last_election_de)

# last election vote without weights
sjp.frq(DaliaDE_temp$voted_party_last_election_de,
        title = "Party vote share last federal election (unweighted)", 
        axis.title = c("Party vote share", "Parties"),
        sort.frq = c("asc"),
        geom.size = 0.5,
        coord.flip=TRUE,
        show.axis.values = FALSE)

# weighted
sjp.frq(DaliaDE_temp$voted_party_last_election_de, 
        title = "Party vote share last federal election (weighted)", 
        axis.title = c("Party vote share","Parties"),
        sort.frq = c("asc"),
        geom.size = 0.5,
  #      show.ci = TRUE, # confidence intervals?? how calculated?
        coord.flip=TRUE,
        show.axis.values = FALSE,
        weight.by = DaliaDE_temp$weight)

# FAILURE: labels are completely mixed up after weighting

DaliaDE_temp <- filter(DaliaDE, vote_nextelection_de != "No vote")
DaliaDE_temp$vote_nextelection_de <- factor(DaliaDE_temp$vote_nextelection_de)

sjp.frq(DaliaDE_temp$vote_nextelection_de,
        title = "Vote next federal election (unweighted)", 
        axis.title = c("Parties", "Party vote share"),
        sort.frq = c("asc"),
        geom.size = 0.5,
        geom.colors = "cadetblue",
        coord.flip = TRUE,
        show.axis.values = FALSE)

sjp.frq(DaliaDE_temp$vote_nextelection_de,
        title = "Vote at next federal election", 
        axis.title = c("Party vote share","Parties"),
        sort.frq = c("asc"),
        geom.size = 0.6,
        coord.flip=TRUE,
        show.axis.values = FALSE,
        weight.by = DaliaDE_temp$weight,
        title.wtd.suffix = " (weighted)")
# mit weights verschieben sich wieder die label -> scheisse

rm(DaliaDE_temp)


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

# clean table
rownames(VoteLast) <- VoteLast$last.vote 
VoteLast <- select(VoteLast, c(2:ncol(VoteLast)))

# order rows and columns
VoteLast <- VoteLast[c("Union", "SPD", "Gruenen", "Linke", "FDP", "AfD"),]
VoteLast <- VoteLast[,c("Union", "SPD", "Gruenen", "Linke", "FDP", "AfD", "No vote", "Other")]

# loyality: last vote = next vote / total respondents per party (last election)
VoteLast$loyality <- diag(as.matrix(VoteLast))/rowSums(VoteLast)

# Table for 2013 -> 2017 voter transitions 
# Weil die Loyalitätsstroeme so fett sind lassen die anderen sich kaum unterscheiden :/
transitionPlot(as.matrix(VoteLast[,c(1:6)]),  
               box_txt = c("Union", "SPD", "Gruenen", "Linke", "FDP", "AfD"))

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

DaliaDE <- filter(DaliaDE, !(voted_party_last_election_de == "No vote" | 
                               voted_party_last_election_de == "Not able"))
DaliaDE$voted_party_last_election_de <- factor(DaliaDE$voted_party_last_election_de)

# create age.gr (like in election statistics)
DaliaDE$age.gr <- c("18-25", "26-35", 
                    "36-45", "46-60", "60+")[findInterval(DaliaDE$age, 
                                                 c(-Inf, 25.5, 35.5, 45.5, 60.5, Inf))]

### Create Strata ###

# Create Strata gender-age-party
plyr::count(DaliaDE, c('gender','age.gr','voted_party_last_election_de')) 
# 2*5*7 = 70 Cluster; four are empty

Strata.1 <- DaliaDE %>% group_by(gender, age.gr, voted_party_last_election_de) %>%
  tally() %>% complete(nesting(gender), age.gr, voted_party_last_election_de)
# replace missing
Strata.1$n[is.na(Strata.1$n)] <- 0

# Create Trata gender-age-education
# plyr::count(DaliaDE_temp, c('gender','age.gr', 'edu.cat', 'voted_party_last_election_de'))

# discuss small group size

#####################
### Direct method ###
#####################
# 1. Load exit poll data (either KAS or election statistics)
load("Wahlstatistik.RData")

# big problem: no AfD data in Exit Polls


# 2. Compute weights for gender, age, vote


# 3. Apply weights on Dalia Data

########################
### Indirect methods ###
########################
# 1. Get data from census
# 2. Compute weights.
# 3. Apply on data.class
# 4. Create deterministic or probabilistic measure of likely voter
# 5. Apply on Dalia data