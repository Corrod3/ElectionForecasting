### Title #####################################################################
# Script for computing weights
# by Moritz Hemmerlein and Alexander Sacharow
###############################################################################

### ToDo ######################################################################
# 1. Combine CDU + CSU
# 2. finish census
### End ToDO ##################################################################


# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Election Forecasting/ElectionForecasting"), silent = TRUE)
try(setwd("C:/Users/Moritz/Desktop/ElectionForecasting"), silent = TRUE)

source("packages.R")



### Election statistics #######################################################

# Get data Bundeswahlstatistik
# Wähler der Parteien nach Alter und Geschlecht
# A: lets remove underscore and plus
colnames <- c("parties",
              "age1825.2013", "age1825.2009",
              "age2635.2013", "age2635.2009",
              "age3645.2013", "age3645.2009",
              "age4660.2013", "age4660.2009",
              "age6169.2013", 
              "age60P.2013", "age60P.2009",
              "age70P.2013")


# read_csv2 delimits with ;
#Vote2013 <- read_csv2("./Weitere Daten/Bundeswahlstatistik_2013/Wählerschaft_der_Parteien_nach_Geschlecht_und_Alter.csv", 
#                           skip = 10, col_names = colnames)

Vote2013 <- read.csv2("./Weitere Daten/Bundeswahlstatistik_2013/Wählerschaft_der_Parteien_nach_Geschlecht_und_Alter.csv", 
                            skip = 10, sep=";", dec=",", header = FALSE, col.names = colnames)
rm(colnames)

# filter rows with no entries
Vote2013 <- filter(Vote2013, !is.na(age1825.2009))

# include gender
Vote2013$gender <- rep(c("Total", "Male", "Female"), each = 8)

# filter 5 age classes
#Vote2013 <- Vote2013 %>% 
#  select(everything(), -which(colnames(Vote2013) %in% c('age61_69.2013', 'age70+.2013')))

Vote2013 <- Vote2013 %>% select(-age6169.2013, -age70P.2013)

# select 2013 data
Vote2013 <- Vote2013 %>% select(parties, gender, ends_with("2013")) 

# define as numeric; delete commas from first column
Vote2013$age1825.2013 <- as.numeric(sub(",",".",Vote2013$age1825.2013))
Vote2013$age1825.2013 <- as.numeric(Vote2013$age1825.2013)

# check column sum
# Vote2013$sum <- rowSums(Vote2013[c(3:7)])

# rename
Vote2013 <- Vote2013 %>% mutate(
  parties = str_replace(parties, "G.*?E", "Gruene"),
  parties = str_replace(parties, "DIE LINKE", "Linke"),
  parties = str_replace(parties, "Sonstige", "Other"))

# save file
save(Vote2013, file = "./Processed/Vote_2013.RData")


###########################
### Total election results -----------------------------------------------------

# import election results
# changed the function because read.csv is less error prone
#Total2013 <- read_csv("./Weitere Daten/Bundeswahlstatistik_2013/Wahlergebnisse_2013.csv")
Total2013 <- read.csv("./Weitere Daten/Bundeswahlstatistik_2013/Wahlergebnisse_2013.csv")

# general election info
eligible.voter.2013 <- Total2013[[1,2]]
voter.2013 <- Total2013[[2,2]]
turnout.2013 <- voter.2013/eligible.voter.2013
invalid.votes.2013 <- Total2013[c(3,4),] 
Total2013 <- Total2013 %>% filter(!is.na(Total2013$Zweitstimmen_pct) &
                                      !is.na(Total2013$Erststimmen_pct))
# drop gueltig + ungueltige votes
Total2013 <- Total2013[-c(1:2),]


# collapse Other
Total2013 <- Total2013 %>% mutate(
  Parteien = ifelse(Parteien %in% c("CDU", "CSU", "SPD", "DIE LINKE", "AfD", "FDP", "GRUENE"), as.character(Parteien), "Other"),
  Parteien = str_replace(Parteien, c("DIE LINKE"), c("Linke")),
  Parteien = str_replace(Parteien, c("GRUENE"), c("Gruene"))
  ) %>% 
  group_by(Parteien) %>% 
  summarise_all(sum) %>% 
  arrange(desc(Zweitstimmen_pct))

colnames(Total2013)[1] <- "parties"

# how about combining CSU and CDU

# export
save(Total2013, file = "./Processed/Total_2013.RData")

### General turnout by gender #################################################

# load table
Turnout2013 <- read.csv2("./Weitere Daten/Bundeswahlstatistik_2013/Wahlbeteiligung_nach_Geschlecht_und_alter_seit_1983.csv", 
                         skip = 8)

# filter eligible voters and actual voters
Turnout2013 <- Turnout2013 %>% filter(X == "Insgesamt" | X == "Zusammen") %>% 
                  select(1:4) %>% mutate(X.1 = as.numeric(sub(",",".",X.1)))

colnames(Turnout2013) <- c("gender", "eligible.voter", "actual.voter", "turnout.within")
Turnout2013$gender <- c("Total", "Male", "Female")
Turnout2013$TurnoutOverall <- 
  Turnout2013$actual.voter/Turnout2013$eligible.voter[Turnout2013$gender == "Total"]
Turnout2013$Turnoutgender <- 
  Turnout2013$actual.voter/Turnout2013$actual.voter[Turnout2013$gender == "Total"]

### Zweitstimmen nach Geschlecht ##############################################

#VoteByGender <- read_csv2(
#  "./Weitere Daten/Bundeswahlstatistik_2013/Zweitstimmen_nach_Geschlecht_seit_1953.csv", 
#  skip = 6, col_names = c("Year", "CDU", "SPD", "FDP", "Linke", "Gruene", "CSU", "Other"),
#  col_types = "cddddddd")

VoteByGender <- read.csv2(
  "./Weitere Daten/Bundeswahlstatistik_2013/Zweitstimmen_nach_Geschlecht_seit_1953.csv", 
  skip = 6, col.names = c("Year", "CDU", "SPD", "FDP", "Linke", "Gruene", "CSU", "Other"))

# select only 2013 information
VoteByGender <- filter(VoteByGender, Year == "2013")

# add gender variable
VoteByGender$gender <- c("Total", "Male", "Female")

# add turnout
VoteByGender <- cbind(VoteByGender[,-1], Turnoutgender = Turnout2013$Turnoutgender)

# format to numeric
VoteByGender[,-8] <- apply(VoteByGender[,-8], 2, function(x) FUN = as.numeric(sub(",",".",x)))

# compute shares in total population for each strata
for(i in 1:nrow(VoteByGender)) {
  VoteByGender[i,c(1:7)] <- VoteByGender[i,c(1:7)]/100 * VoteByGender[i,9]
}

VoteByGender <- reshape2::melt(select(VoteByGender, -Turnoutgender), id.vars = "gender")
colnames(VoteByGender)[-1] <- c("parties","gender.vote")

#VoteByGender <- VoteByGender %>% gather(
#  one_of(names(VoteByGender[-c(length(VoteByGender)-1, length(VoteByGender))])), 
#  key = "parties", 
#  value = "gender.vote") %>% arrange(desc(gender)) %>% select(-Turnoutgender)

### Computing combined frequencies - Age and Gender across parties ###########

# Problem: get frequencies across age groups for AfD; no 2013 data; computable?

# add AfD to Sonstige
Total2013.v1 <- Total2013 %>% mutate(
  parties = str_replace(parties, c("AfD"), c("Other"))) %>%
  group_by(parties) %>%
  summarise_all(sum)

#Insgesamt <- rep(c(Insgesamt, NA, Turnout2013, NA), c(1, 4, 1, 1))
#Total2013.v1 <- rbind(Total2013.v1, Insgesamt)

### Join tibbles ##############################################################

#VoteAgeGender.2013 <- left_join(Vote2013, Total2013.v1, by = "parties")

VoteByGender$parties <- as.character(VoteByGender$parties)
VoteAgeGender.2013 <- left_join(Vote2013, VoteByGender, by = c("parties", "gender"))
                 
VoteAgeGender.2013 <- filter(VoteAgeGender.2013, !is.na(gender.vote))

VoteAgeGender.2013[,c(3:7)] <- (VoteAgeGender.2013[,c(3:7)] / 100) *
                                VoteAgeGender.2013$gender.vote

save(VoteAgeGender.2013, file = "./Processed/Vote_Age_Gender_2013.RData")


### Census data ###############################################################

#Census.AgegenderEdu <- read_csv2(
 # "./Weitere Daten/Census2011/Population_Age_Gender_Education.csv", 
 # skip = 6, na = c("/", ""))

Census.AgegenderEdu <- read.csv2(
  "./Weitere Daten/Census2011/Population_Age_Gender_Education.csv", 
  skip = 6, sep = ";",quote = "")

Census.AgegenderEdu[Census.AgegenderEdu == "/"] <- NA

Census.AgegenderEdu <- Census.AgegenderEdu[complete.cases(Census.AgegenderEdu[,2]),]

# change columnnames
names(Census.AgegenderEdu) <- names(Census.AgegenderEdu) %>% 
  str_replace("M.+ch", "male") %>%
  str_replace("Weiblich", "female") %>%
  str_replace("H.+ss", "edu.lvl") %>%
  str_replace("Insgesamt$", "total") %>%
  str_replace("Insgesamt.1", "total.pct")


# Baustelle:
for (i in 4:length(names(Census.AgegenderEdu))) {
  names(Census.AgegenderEdu)[i] <- 
    str_replace(names(Census.AgegenderEdu)[i] ,".\\d", "")
  names(Census.AgegenderEdu)[i] <- 
    str_c(names(Census.AgegenderEdu[i]), Census.AgegenderEdu[2,i], sep = ".")
  names(Census.AgegenderEdu)[i] <- 
    str_replace(names(Census.AgegenderEdu)[i], "\\s-\\s", ".")
  }


names(Census.AgegenderEdu) <- str_replace(names(Census.AgegenderEdu), "Unter\\s10", "below10")
names(Census.AgegenderEdu) <- str_replace(names(Census.AgegenderEdu), "\\s.+ter", "p")

