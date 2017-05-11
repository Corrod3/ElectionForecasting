### Script for Clean data of wave 2 ###########################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Election Forecasting/ElectionForecasting"), silent = TRUE)
try(setwd("C:\\Users\\Moritz\\Desktop\\ElectionForecasting"), silent = TRUE)

source("packages.R")





### Strata construction Dalia march ###########################################



Strata.1.March <- DaliaMar %>% filter(vote_nextelection_de != "No vote") %>% 
  group_by(gender, AgeGroup, vote_nextelection_de) %>% 
  tally() 

# strata percentages
Strata.1.March$n <- 100*(Strata.1.March$n/sum(Strata.1.March$n))

### Exit poll Strata ----------------------------------------------------------

# load weighting data
load("./Processed/Vote_Age_Gender_2013.RData")
load("./Processed/Total_2013.RData")

# AfD - share of total "Others"
AfD <- Total2013[8,"Zweitstimmen_pct"]/(Total2013[8,"Zweitstimmen_pct"]+
                                               Total2013[6,"Zweitstimmen_pct"])

# compute AfD share from their total vote share across all "Other" parties:
# assumption: Distribution of voters across age groups AND gender equal
# alternative solution: collect demographics of AfD voter data

AfDShares <- data.frame(parties = rep("AfD", 3), gender = c("Total", "Male", "Female"), 
                     "age1825" = as.numeric(rep(NA, 3)),
                     "age2635" = as.numeric(rep(NA, 3)),
                     "age3645" = as.numeric(rep(NA, 3)),
                     "age4660" = as.numeric(rep(NA, 3)),
                     "age60P" = as.numeric(rep(NA, 3)),
                     "gender.vote" = as.numeric(rep(NA, 3)), stringsAsFactors = FALSE)

# Add AfD to vote shares of Exitpoll
VoteAgeGender.2013 <- data.frame(rbind(as.matrix(VoteAgeGender.2013), as.matrix(AfDShares)), 
                                  stringsAsFactors = FALSE)
# format as.double (why not just as.numeric?)
VoteAgeGender.2013[,c(3:8)] <- sapply(VoteAgeGender.2013[,c(3:8)], as.double)

# compute AfD and others share
VoteAgeGender.2013[VoteAgeGender.2013$parties == "AfD", c(3:8)] <- 
  VoteAgeGender.2013[VoteAgeGender.2013$parties == "Other", c(3:8)]*(as.double(AfD))
VoteAgeGender.2013[VoteAgeGender.2013$parties == "Other", c(3:8)] <- 
  VoteAgeGender.2013[VoteAgeGender.2013$parties == "Other", c(3:8)]*(1-as.double(AfD))

# combine CDU/CSU
VoteAgeGender.2013 <- VoteAgeGender.2013 %>% mutate(
  parties = str_replace(parties, "CDU|CSU", "Union")
  ) %>% group_by(gender, parties) %>%
  summarise_all(sum) %>% filter(gender != "Total")

# reshape
Strata.1.ExitPoll <- dplyr::select(VoteAgeGender.2013, -gender.vote) %>% 
  gather(key = "AgeGroup", value = "vote.share", starts_with("age"))

# rename age group values
Strata.1.ExitPoll$AgeGroup <- mapvalues(Strata.1.ExitPoll$AgeGroup,
  c("age1825.2013", "age2635.2013", "age3645.2013", "age4660.2013", "age60P.2013"),
  c("18-25", "26-35", "36-45", "46-60", "60+"))


Strata.1.ExitPoll$vote.share <- Strata.1.ExitPoll$vote.share*100
Strata.1.ExitPoll <- rename(Strata.1.ExitPoll, vote.share.exit = vote.share)
Strata.1.March <- rename(
  Strata.1.March, parties = vote_nextelection_de, vote.share.dalia = n)

### compute weights
exitPollWeights <- left_join(Strata.1.ExitPoll, Strata.1.March, by = c("gender", "parties", "AgeGroup"))
exitPollWeights$wtGenderAgeParty <- (exitPollWeights$vote.share.exit)/exitPollWeights$vote.share.dalia
exitPollWeights <- rename(exitPollWeights, vote_nextelection_de = parties)

# assign weights to Dalia data
DaliaMar <- left_join(DaliaMar, exitPollWeights, by = c("gender", "vote_nextelection_de", "AgeGroup"))

### graph with new weights ----------------------------------------------------
position <- c("Union", "SPD", "Gruene", "Linke", "FDP", "AfD", "Other")
farben = c("AfD" = "#009dd1","Union" = "#222222", "FDP" = "#ffb700", "Gruene" = "#349f29", "Linke" = "#cc35a0", "SPD" = "#ce1b1b", "Other" = "grey")

# Plot function
shares.plot <- function(share.frame){
  ggplot(data = share.frame, aes(x = vote_nextelection_de, y = shares)) +
    geom_bar(aes(fill = vote_nextelection_de), stat = "identity") + 
    scale_fill_manual(values = farben) +
    scale_colour_manual(values = farben) +
    theme_classic() +
    scale_x_discrete(limits = position, name = "Major parties") +
    ylab("Vote Share in %") +
    guides(fill = FALSE) +
    geom_text(aes(label=paste0(round(shares,2),"%"), y=shares+1.1), size = 3.5)
  
  ggsave(file = "./Grafiken/plot.png")
}


# p1.unweighted <- DaliaMar %>% filter(vote_nextelection_de != "No vote") %>%  
#     ggplot(aes(x = vote_nextelection_de)) +
#     geom_bar(aes(fill = vote_nextelection_de)) + 
#                scale_fill_manual(values = farben) +
#               scale_colour_manual(values = farben) +
#    theme_classic() +
#    scale_x_discrete(limits = position, name = "Major parties") +
#   guides(fill = FALSE)
# 
# p1.weighted <- DaliaMar %>% filter(vote_nextelection_de != "No vote") %>%  
#   ggplot(aes(x = vote_nextelection_de, weight = wtGenderAgeParty)) +
#   geom_bar(aes(fill = vote_nextelection_de)) + 
#   scale_fill_manual(values = farben) +
#   scale_colour_manual(values = farben) +
#   theme_classic() +
#   scale_x_discrete(limits = position, name = "Major parties") +
#   guides(fill = FALSE)
#   
# grid.arrange(p1.unweighted, p1.weighted, ncol = 2)

uw.shares <- DaliaMar %>% 
  filter(vote_nextelection_de != "No vote") %>% 
  count(vote_nextelection_de) %>%
  mutate(shares = 100*n / sum(n))

w.shares <- DaliaMar %>% 
  filter(vote_nextelection_de != "No vote") %>% 
  count(vote_nextelection_de, wt = wtGenderAgeParty) %>%
  mutate(shares = 100*n / sum(n))

w.shares %>% shares.plot()
uw.shares %>% shares.plot()

# -> bei den gewichteten Daliawerten kommt am Ende genau das Ergebnis der Bundestagswahl raus.
# Ist ja auch logisch, wenn man die cluster so justiert, dass sie genau den exit polls entsprechen


### -> Gewichtung nach demographischen Faktoren aus den Exit Polls --------
# GenderAgeEdu? GenderAge
