### Dalia Import Script #######################################################


### Preparation ###############################################################
## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Election Forecasting/ElectionForecasting"), silent = TRUE)
try(setwd("C:\\Users\\Moritz\\Desktop\\ElectionForecasting"), silent = TRUE)

source("packages.R")


### Import December Data ######################################################

# types for excel import
types <- replicate(90, "text")
types[2] <- "numeric"
types[4] <- "numeric"

#Import dalia data as strings
DaliaDec <- read_excel("Raw/Dalia_research_challenge_europulse.xlsx",sheet=1, 
                     col_types=types, na="NA")
rm(types)

# change types
DaliaDec[3] <- as.factor(unlist(DaliaDec[3]))
for(i in 5:ncol(DaliaDec)) {
  DaliaDec[i] <- as.factor(unlist(DaliaDec[i]))
}

# change column names
names(DaliaDec) <- sub("\\[.+\\] (.+)", "\\1", names(DaliaDec))

# dataset with german users/sorting
DaliaDec <- DaliaDec %>% filter(country_code == "DE") %>% arrange(age, gender, degree_of_urbanisation, education_level)

# Create easy identifier
DaliaDec$identifier <- c(1:nrow(DaliaDec))
DaliaDec <- select(DaliaDec, identifier, weight, country_code, age, gender, degree_of_urbanisation, education_level, everything())
# weights vary across four demographic variables  

# Rename label (order in parties vector must equal order in vote_nextelection_de)
parties <- c("AfD", "Gruene", "Union", "Linke", "FDP", "No vote", "Other", "SPD")
levels(DaliaDec$vote_nextelection_de) <- parties

label_temp <- c("Not able", "No vote", "Other" ,"AfD", "Gruene", "Union", "Linke", "FDP", "SPD")
levels(DaliaDec$voted_party_last_election_de) <- label_temp
rm(label_temp)

levels(DaliaDec$education_level) <- c("High school or equivalent", 
                                     "No formal education", 
                                     "University or equivalent",
                                     "Some high school or secondary school",
                                     "Other/Rather not answer")

DaliaDec <- DaliaDec %>% mutate(gender = str_replace(gender, 
                                           "^female",
                                           "Female"),
                      gender = str_replace(gender, 
                                           "^male",
                                           "Male"))

# education category
DaliaDec$edu.cat <- ifelse(
  DaliaDec$education_level == "University or equivalent", 
  "Higher education", 
  DaliaDec$education_level) 
DaliaDec$edu.cat <- ifelse(
  DaliaDec$education_level == "No formal education",
  "Lower education",
  DaliaDec$edu.cat)
DaliaDec$edu.cat <- ifelse(
  DaliaDec$education_level == "Other/Rather not answer",
  "Lower education",
  DaliaDec$edu.cat)
DaliaDec$edu.cat <- ifelse(
  DaliaDec$education_level == "High school or equivalent",
  "Medium education",
  DaliaDec$edu.cat)
DaliaDec$edu.cat <- ifelse(
  DaliaDec$education_level == "Some high school or secondary school",
  "Medium education",
  DaliaDec$edu.cat)
DaliaDec$edu.cat <- factor(DaliaDec$edu.cat, levels = c("Lower education",
                                                        "Medium education",
                                                        "Higher education"))


# Filter: not eligible to vote, not german resident, below 18 at the time of election
DaliaDec <- filter(DaliaDec, 
                  vote_next_national_election != "I'm not eligible to vote" & 
                    residency == "Yes, as a citizen" & 
                    age > 16)
# 255 observations drop out

# only use relevant information
DaliaDec <- DaliaDec %>% select(uuid, identifier, weight, age, country_code,
                                education_level, edu.cat, gender, religion, 
                                employment_status, religion, vote_next_national_election,
                                voted_party_last_election_de,
                                vote_nextelection_de)


### compute variables #########################################################

# create binary turnout variable
DaliaDec$turnout_exp <- DaliaDec$vote_next_national_election
DaliaDec$turnout_exp <- plyr::mapvalues(DaliaDec$turnout_exp, 
                                        from = c("I'm not eligible to vote",
                                                 "No, I will definitely not vote", 
                                                 "No, I will probably not vote",
                                                 "Yes, I will definitely vote",
                                                 "Yes, I will probably vote") , 
                                        to = c(0,0,0,1,1))

# create AgeGroup (like in election statistics)
DaliaDec$AgeGroup <- c("18-25", "26-35", 
                       "36-45", "46-60", "60+")[findInterval(DaliaDec$age, 
                                                             c(-Inf, 25.5, 35.5, 45.5, 60.5, Inf))]

### load March data ###########################################################

### March data ################################################################

# load
DaliaMar <- read_csv("./Raw/March_Wave/data_coded_HertieSchool of Governance_2017-04-11_10-20.csv")

# create variable names
var.names <- names(DaliaMar) %>% str_replace("(.+Q\\d+_)|(\\[.+\\]\\s)", "") %>%
  str_replace("\\s.+", "") %>%
  str_replace("_[A-Z].+.|\\d", "") %>%
  str_replace("\\]", "")

colnames(DaliaMar) <- var.names   
colnames(DaliaMar)[8] <- c("gender")

# interesting vars: opinion_eu, opinion_government, political_views, likelihood_to_demonstrate
# frequency_of_voting, vote_next_national_election, vote_nextelection_de, voted_party_last_election_de,
# certainty_party_to_vote, ranking_party_de, bundeskanzler_candidate, frequent_sharing_of_politicalviews

# clean NAs (might be interesting to look for a pattern in NAs in this question)

# DaliaDec$voted_party_last_election_de <- factor(DaliaDec$voted_party_last_election_de)

# rename variables
DaliaMar$vote_nextelection_de <- str_replace(DaliaMar$vote_nextelection_de, 
                                                "(\\s.+lands)|(\\s.+land)|(CDU.+le\\s)|(Die\\s)|(\\s.+Partei)|(B.+Die\\s)",
                                                "")
DaliaMar$vote_nextelection_de <- str_replace(DaliaMar$vote_nextelection_de,
                                                "G.*?en", "Gruene")
DaliaMar$vote_nextelection_de <- str_replace(DaliaMar$vote_nextelection_de,
                                                "I would not vote", "No vote")


DaliaMar$voted_party_last_election_de <- str_replace(DaliaMar$voted_party_last_election_de, 
                                             "(Yes, I voted for\\s)|
                                             (Yes, but I voted for an)", "")

DaliaMar$voted_party_last_election_de <- str_replace(DaliaMar$voted_party_last_election_de, 
                                             "(\\s.+lands)|(\\s.+land)|(Die\\s)|
                                             (\\s.+Partei)|(B.+Die\\s)",
                                             "")

DaliaMar$voted_party_last_election_de <- str_replace(DaliaMar$voted_party_last_election_de, 
                                             "I wanted to vote but I wasn't able to",
                                             "Not able")
DaliaMar$voted_party_last_election_de <- str_replace(DaliaMar$voted_party_last_election_de, 
                                                     "Yes, but I voted for another party",
                                                     "Other")
DaliaMar$voted_party_last_election_de <- str_replace(DaliaMar$voted_party_last_election_de, 
                                                     "CDU.*Union",
                                                     "Union")
DaliaMar$voted_party_last_election_de <- str_replace(DaliaMar$voted_party_last_election_de, 
                                                     "FDP.*Partei",
                                                     "FDP")
DaliaMar$voted_party_last_election_de <- str_replace(DaliaMar$voted_party_last_election_de, 
                                                     "No, I did not vote",
                                                     "No vote")
DaliaMar$voted_party_last_election_de <- str_replace(DaliaMar$voted_party_last_election_de,
                                             "G.*?en", "Gruene")
  
DaliaMar$gender <- DaliaMar$gender %>% mapvalues(c("female", "male"), c("Female", "Male"))

### compute variables
DaliaMar$AgeGroup <- c("18-25", "26-35", 
                          "36-45", "46-60", "60+")[findInterval(DaliaMar$age, 
                                                                c(-Inf, 25.5, 35.5, 45.5, 60.5, Inf))]

#table(DaliaMar$religion)

# Education Category ##########################################################

# are coded with numbers
DaliaMar$edu.cat <- ifelse(DaliaMar$education_level == 4, 
                           "Higher education", 
                           DaliaMar$education_level) 
DaliaMar$edu.cat <- ifelse(DaliaMar$education_level == 1,
                           "Lower education",
                           DaliaMar$edu.cat)
DaliaMar$edu.cat <- ifelse(DaliaMar$education_level == 5,
                           "Lower education",
                           DaliaMar$edu.cat)
DaliaMar$edu.cat <- ifelse(DaliaMar$education_level == 3,
                           "Medium education",
                           DaliaMar$edu.cat)
DaliaMar$edu.cat <- ifelse(DaliaMar$education_level == 2,
                           "Medium education",
                           DaliaMar$edu.cat)
DaliaMar$edu.cat <- factor(DaliaMar$edu.cat, levels = c("Lower education",
                                                        "Medium education",
                                                        "Higher education"))
# create binary turnout variable
DaliaMar$turnout_exp <- DaliaMar$vote_next_national_election
DaliaMar$turnout_exp <- plyr::mapvalues(DaliaMar$turnout_exp, 
                                        from = c("I'm not eligible to vote",
                                                 "No, I will definitely not vote", 
                                                 "No, I will probably not vote",
                                                 "Yes, I will definitely vote",
                                                 "Yes, I will probably vote") , 
                                        to = c(0,0,0,1,1))

# drop unnessessary information
DaliaMar <- DaliaMar %>% select(uuid, weight, age, country_code,
                                education_level, AgeGroup, gender, religion, 
                                employment_status, religion, vote_next_national_election,
                                voted_party_last_election_de,
                                vote_nextelection_de, edu.cat, turnout_exp)

### March data mining #########################################################

# Charts
#ggplot(data = DaliaMar, mapping = aes(x = vote_nextelection_de, weight = weight)) + 
#  geom_bar()
#ggplot(data = DaliaMar, mapping = aes(x = vote_nextelection_de)) + 
#  geom_bar(aes(fill = gender))
#ggplot(data = DaliaMar, mapping = aes(x = fct_infreq(vote_nextelection_de))) + 
#  geom_bar(aes(fill = certainty_party_to_vote))
# -> Dalia weights cause hardly any changes
