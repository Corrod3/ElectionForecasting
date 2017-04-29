# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Election Forecasting/ElectionForecasting"), silent = TRUE)
try(setwd("C:/Users/Moritz/Desktop/ElectionForecasting"), silent = TRUE)

source("packages.R")

###################################

### Election statistics -------------------------------------------------------

# Get data Bundeswahlstatistik
# Wähler der Parteien nach Alter und Geschlecht
colnames <- c("Parties",
              "age18_25.2013", "age18_25.2009",
              "age26_35.2013", "age26_35.2009",
              "age36_45.2013", "age36_45.2009",
              "age46_60.2013", "age46_60.2009",
              "age61_69.2013", 
              "age60+.2013", "age60+.2009",
              "age70+.2013")

coltype <- rep(c("c", "i"), times = c(1,12))

# read_csv2 delimits with ;
Wahlstatistik <- read_csv2("./Weitere Daten/Bundeswahlstatistik_2013/Wählerschaft_der_Parteien_nach_Geschlecht_und_Alter.csv", 
                           skip = 10, col_names = colnames) 

# filter rows with no entries
Wahlstatistik <- filter(Wahlstatistik, !is.na(age18_25.2009))

# include gender
Wahlstatistik$gndr <- rep(c("Total", "Male", "Female"), each = 8)

# filter 5 age classes
Wahlstatistik <- Wahlstatistik %>% 
  select(everything(), -which(colnames(Wahlstatistik) %in% c('age61_69.2013', 'age70+.2013')))

# select 2013 data
vote.2013 <- Wahlstatistik %>% select(Parties, gndr, ends_with("2013")) 

# define as numeric; delete commas from first column
vote.2013$age18_25.2013 <- as.numeric(scan(text = vote.2013$age18_25.2013, dec=",", sep="."))
vote.2013$age18_25.2013 <- as.numeric(vote.2013$age18_25.2013)

# check column sum
#vote.2013$sum <- rowSums(vote.2013[c(3:7)])

# rename
vote.2013 <- vote.2013 %>% mutate(
  Parties = str_replace(Parties, "G.*?E", "Gruene"),
  Parties = str_replace(Parties, "DIE LINKE", "Linke"),
  Parties = str_replace(Parties, "Sonstige", "Other"))

# save file
save(vote.2013, file = "./Processed/Vote_2013.RData")


###########################
### Total election results -----------------------------------------------------

total.2013 <- read_csv("./Weitere Daten/Bundeswahlstatistik_2013/Wahlergebnisse_2013.csv")

# general election info
eligible.voter.2013 <- total.2013[[1,2]]
voter.2013 <- total.2013[[2,2]]
turnout.2013 <- voter.2013/eligible.voter.2013
invalid.votes.2013 <- total.2013[c(3,4),] 
total.2013 <- total.2013 %>% filter(!is.na(total.2013$Zweitstimmen_pct) &
                                      !is.na(total.2013$Erststimmen_pct))
total.2013 <- total.2013[c(3:nrow(total.2013)),]

# collapse Other

total.2013 <- total.2013 %>% mutate(
  Parteien = ifelse(Parteien %in% c("CDU", "CSU", "SPD", "DIE LINKE", "AfD", "FDP", "GRUENE"), Parteien, "Other"),
  Parteien = str_replace(Parteien, c("DIE LINKE"), c("Linke")),
  Parteien = str_replace(Parteien, c("GRUENE"), c("Gruene"))
  ) %>% 
  group_by(Parteien) %>% 
  summarise_all(sum) %>% 
  arrange(desc(Zweitstimmen_pct))

colnames(total.2013)[1] <- "Parties"

save(total.2013, file = "./Processed/Total_2013.RData")



### General turnout by gender -------------------------------------------------

turnout.2013 <- read_csv2("./Weitere Daten/Bundeswahlstatistik_2013/Wahlbeteiligung_nach_Geschlecht_und_alter_seit_1983.csv", 
                           skip = 8, col_types = "cddddddddd")
turnout.2013 <- turnout.2013[,c(1:4)] %>% filter(X1 == "Insgesamt" | X1 == "Zusammen") 
colnames(turnout.2013) <- c("Gndr", "EligibleVoter", "ActualVoter", "TurnoutWithin")
turnout.2013$Gndr <- c("Total", "Male", "Female")
turnout.2013$TurnoutOverall <- 
  turnout.2013$ActualVoter/turnout.2013$EligibleVoter[turnout.2013$Gndr == "Total"]
turnout.2013$TurnoutGndr <- 
  turnout.2013$ActualVoter/turnout.2013$ActualVoter[turnout.2013$Gndr == "Total"]


#################################
### Zweitstimmen nach Geschlecht ----------------------------------------------

VoteByGndr <- read_csv2(
  "./Weitere Daten/Bundeswahlstatistik_2013/Zweitstimmen_nach_Geschlecht_seit_1953.csv", 
  skip = 6, col_names = c("Year", "CDU", "SPD", "FDP", "Linke", "Gruene", "CSU", "Other"),
  col_types = "cddddddd")

VoteByGndr <- filter(VoteByGndr, Year == "2013")
VoteByGndr$gndr <- c("Total", "Male", "Female")

VoteByGndr <- cbind(VoteByGndr[-1], TurnoutGndr = turnout.2013$TurnoutGndr)

for(i in 1:nrow(VoteByGndr)) {
  VoteByGndr[i,c(1:7)] <- VoteByGndr[i,c(1:7)]/100 * VoteByGndr[i,9]
}

VoteByGndr <- VoteByGndr %>% gather(
  one_of(names(VoteByGndr[-c(length(VoteByGndr)-1, length(VoteByGndr))])), 
  key = "Parties", 
  value = "GndrVote") %>% arrange(desc(gndr)) %>% select(-TurnoutGndr)


###################################################################
### Computing combined frequencies - Age and Gender across parties ------------

# Problem: get frequencies across age groups for AfD; no 2013 data; computable?

# add AfD to Sonstige
total.2013.v1 <- total.2013 %>% mutate(
  Parties = str_replace(Parties, c("AfD"), c("Other"))) %>%
  group_by(Parties) %>%
  summarise_all(sum)

#Insgesamt <- rep(c(Insgesamt, NA, turnout.2013, NA), c(1, 4, 1, 1))
#total.2013.v1 <- rbind(total.2013.v1, Insgesamt)

### Join tibbles --------------------------------------------------------------
#VoteAgeGndr.2013 <- left_join(vote.2013, total.2013.v1, by = "Parties")
VoteAgeGndr.2013 <- left_join(vote.2013, VoteByGndr, by = c("Parties", "gndr"))
                 
VoteAgeGndr.2013 <- filter(VoteAgeGndr.2013, !is.na(GndrVote))

VoteAgeGndr.2013[,c(3:7)] <- (VoteAgeGndr.2013[,c(3:7)] / 100) * VoteAgeGndr.2013$GndrVote

save(VoteAgeGndr.2013, file = "./Processed/Vote_Age_Gender_2013.RData")


### Census data ---------------------------------------------------------------

Census.AgeGndrEdu <- read_csv2(
  "./Weitere Daten/Census2011/Population_Age_Gender_Education.csv", 
  skip = 6, na = c("/", ""))

Census.AgeGndrEdu <- Census.AgeGndrEdu[complete.cases(Census.AgeGndrEdu[,2]),]

names(Census.AgeGndrEdu) <- names(Census.AgeGndrEdu) %>% 
  str_replace("M.+ch", "Male") %>%
  str_replace("Weiblich", "Female") %>%
  str_replace("H.+ss", "edu.lvl") %>%
  str_replace("Insgesamt$", "Total") %>%
  str_replace("Insgesamt_1", "Total_pct")

for (i in 4:length(names(Census.AgeGndrEdu))) {
  names(Census.AgeGndrEdu)[i] <- 
    str_replace(names(Census.AgeGndrEdu)[i] ,"_\\d", "")
  names(Census.AgeGndrEdu)[i] <- 
    str_c(names(Census.AgeGndrEdu[i]), Census.AgeGndrEdu[2,i], sep = "_")
  names(Census.AgeGndrEdu)[i] <- 
    str_replace(names(Census.AgeGndrEdu)[i], "\\s-\\s", "_")
  }

names(Census.AgeGndrEdu) <- str_replace(names(Census.AgeGndrEdu), "Unter\\s10", "<10")
names(Census.AgeGndrEdu) <- str_replace(names(Census.AgeGndrEdu), "\\s.+ter", "+")

