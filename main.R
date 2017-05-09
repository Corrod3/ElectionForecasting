###############################################################################
## Election Forecasting 
## by: Alexander Sacharow & Moritz Hemmerlein
###############################################################################

### 0. Preparations ###########################################################

# Clear Global environment
rm(list=ls())

## Setting Working directory
try(setwd("D:/Eigene Datein/Dokumente/Uni/Hertie/Materials/Election Forecasting/ElectionForecasting"), silent = TRUE)
try(setwd("C:/Users/Moritz/Desktop/ElectionForecasting"), silent = TRUE)

source("packages.r")
source("functions.r")

### 1. get data + cleaning ####################################################

source("daliadata.R")
source("weightingdata.R") # add benchmark data

# delete non voters and unable voters
DaliaDec <- DaliaDec %>% filter(!(voted_party_last_election_de == "No vote" | 
                                  voted_party_last_election_de == "Not able" | 
                                  is.na(vote_nextelection_de) |
                                  is.na(voted_party_last_election_de)))

DaliaMar <- DaliaMar %>% filter(!(voted_party_last_election_de == "No vote" | 
                                    voted_party_last_election_de == "Not able" | 
                                    is.na(vote_nextelection_de) |
                                    is.na(voted_party_last_election_de)))

### 2. Strata #################################################################

### Dalia december gender-age-lastvote ########################################
S.GAV.DDec <- DaliaDec %>% filter(voted_party_last_election_de != "No vote" & 
                                  voted_party_last_election_de != "Not able") %>%
  droplevels() %>%  # drops unused factor "No vote" "not able"
  group_by(gender, AgeGroup, voted_party_last_election_de) %>%
  tally() %>% complete(nesting(gender), AgeGroup, voted_party_last_election_de)

S.GAV.DDec <- S.GAV.DDec %>% ungroup() %>%
                        dplyr::rename(parties = voted_party_last_election_de) %>% 
                        mutate(parties = as.character(parties), 
                               gender = as.character(gender))

# replace missing
S.GAV.DDec$n[is.na(S.GAV.DDec$n)] <- 0

# in percentages

S.GAV.DDec$n <- 100*(S.GAV.DDec$n/sum(S.GAV.DDec$n))

### Dalia march gender-age-last-vote ##########################################

S.GAV.DMar <- DaliaMar %>% filter(vote_nextelection_de != "No vote") %>% 
  group_by(gender, AgeGroup, vote_nextelection_de) %>% 
  tally() 

# strata percentages
S.GAV.DMar$n <- 100*(S.GAV.DMar$n/sum(S.GAV.DMar$n))

### Exit poll Strata ##########################################################

# load weighting data
load("./Processed/Vote_Age_Gender_2013.RData")
load("./Processed/Total_2013.RData")

# AfD - share of total "Others"
AfD.share <- as.numeric(Total2013[8,"Zweitstimmen_pct"]/
                       (Total2013[8,"Zweitstimmen_pct"] + 
                        Total2013[6,"Zweitstimmen_pct"]))

# compute AfD share from their total vote share across all "Other" parties:
# assumption: Distribution of voters across age groups AND gender equal

AfDrows <- data.frame(parties = rep("AfD", 3), gender = c("Total", "Male", "Female"), 
                        "age1825" = as.numeric(rep(NA, 3)),
                        "age2635" = as.numeric(rep(NA, 3)),
                        "age3645" = as.numeric(rep(NA, 3)),
                        "age4660" = as.numeric(rep(NA, 3)),
                        "age60P" = as.numeric(rep(NA, 3)),
                        "gender.vote" = as.numeric(rep(NA, 3)), stringsAsFactors = FALSE)

# Add AfD to vote shares of Exitpoll
S.GAV.Exit <- data.frame(rbind(as.matrix(VoteAgeGender.2013), as.matrix(AfDrows)), 
                                 stringsAsFactors = FALSE)
rm(AfDrows, VoteAgeGender.2013)

# format as.double 
S.GAV.Exit[,c(3:8)] <- sapply(S.GAV.Exit[,c(3:8)], as.double)

# compute AfD and others share
S.GAV.Exit[S.GAV.Exit$parties == "AfD", c(3:8)] <- 
  S.GAV.Exit[S.GAV.Exit$parties == "Other", c(3:8)]*(AfD.share)
S.GAV.Exit[S.GAV.Exit$parties == "Other", c(3:8)] <- 
  S.GAV.Exit[S.GAV.Exit$parties == "Other", c(3:8)]*(1-AfD.share)

# combine CDU/CSU
S.GAV.Exit <- S.GAV.Exit %>% mutate(parties = str_replace(parties, 
                                                          "CDU|CSU",
                                                          "Union")) %>%
                              group_by(gender, parties) %>%
                              summarise_all(sum) %>% filter(gender != "Total")

# reshape
S.GAV.Exit <- dplyr::select(S.GAV.Exit, -gender.vote) %>% 
  gather(key = "AgeGroup", value = "vote.share", starts_with("age"))

# rename age group values
S.GAV.Exit$AgeGroup <- mapvalues(S.GAV.Exit$AgeGroup,
                                  c("age1825.2013", "age2635.2013",
                                    "age3645.2013", "age4660.2013",
                                    "age60P.2013"),
                                  c("18-25", "26-35", "36-45", "46-60", "60+"))


S.GAV.Exit$vote.share <- S.GAV.Exit$vote.share*100
S.GAV.Exit <- rename(S.GAV.Exit, vote.share.exit = vote.share)
S.GAV.DMar <- rename(S.GAV.DMar,
                     parties = vote_nextelection_de,
                     vote.share.dalia = n)

### compute weights #####################################################################
# December wave 
# (has empty stratas!!! -> Poststratification not possible)
# Adjust 

S.GAV.Exit.A4 <- S.GAV.Exit %>% mutate(AgeGroup = str_replace(AgeGroup, 
                                                          "(46-60)|(60\\+)",
                                                          "45Plus")) %>%
  group_by(gender, parties, AgeGroup) %>%
  summarise_all(sum) 

S.GAV.DDec.A4 <- S.GAV.DDec %>% ungroup %>% 
  mutate(AgeGroup = str_replace(AgeGroup, "(46-60)|(60\\+)", "45Plus")) %>%
  group_by(gender, parties, AgeGroup) %>%
  summarise_all(sum) 

w.GAV.Exit.DDec <- left_join(S.GAV.Exit.A4,
                             S.GAV.DDec.A4,
                             by = c("gender","parties","AgeGroup"))
w.GAV.Exit.DDec$w.GAV.Exit.DDec <- (w.GAV.Exit.DDec$vote.share.exit)/w.GAV.Exit.DDec$n
w.GAV.Exit.DDec <- rename(w.GAV.Exit.DDec, voted_party_last_election_de = parties)

add <- w.GAV.Exit.DDec[w.GAV.Exit.DDec$AgeGroup == "45Plus",] 
add <- add %>% mutate(AgeGroup = "60+")
w.GAV.Exit.DDec <- rbind(w.GAV.Exit.DDec, add)
w.GAV.Exit.DDec <- w.GAV.Exit.DDec %>% ungroup() %>%
                        mutate(AgeGroup = str_replace(AgeGroup,"45Plus","46-60")) %>%
                                     select(-vote.share.exit,-n)
rm(add)

# assign weights to Dalia data
DaliaDec <- left_join(DaliaDec, w.GAV.Exit.DDec, 
                      by = c("gender", "voted_party_last_election_de",
                             "AgeGroup"))

# March wave
w.GAV.Exit.DMar <- left_join(S.GAV.Exit, S.GAV.DMar, by = c("gender", "parties", "AgeGroup"))
w.GAV.Exit.DMar$w.GAV.Exit.DMar <- (w.GAV.Exit.DMar$vote.share.exit)/w.GAV.Exit.DMar$vote.share.dalia
w.GAV.Exit.DMar <- w.GAV.Exit.DMar %>% rename(voted_party_last_election_de = parties) %>%
                        select(-vote.share.exit, -vote.share.dalia)

# assign weights to Dalia data
DaliaMar <- left_join(DaliaMar, w.GAV.Exit.DMar,
                      by = c("gender", "voted_party_last_election_de", "AgeGroup"))

### age - gender - religion weights with census data ##########################

# december data ###############################################################
S.GAR.DDec <- DaliaDec %>% 
  group_by(gender, AgeGroup30, religion.cat) %>% 
  tally() 

# strata percentages
S.GAR.DDec$n <- 100*(S.GAR.DDec$n/sum(S.GAR.DDec$n))

# march data ##################################################################
S.GAR.DMar <- DaliaMar %>% 
  group_by(gender, AgeGroup30, religion.cat) %>% 
  tally() 

# strata percentages
S.GAR.DMar$n <- 100*(S.GAR.DMar$n/sum(S.GAR.DMar$n))

# census strata ###############################################################
S.GAR.Census <- Census.AgeGenderRel
S.GAR.Census$n <- 100*(S.GAR.Census$n/sum(S.GAR.Census$n))

# weights for december data
w.GAR.Census.DDec <- left_join(S.GAR.Census,
                             S.GAR.DDec,
                             by = c("gender","religion.cat","AgeGroup30"))
w.GAR.Census.DDec$w.GAR.Census.DDec <- (w.GAR.Census.DDec$n.x)/w.GAR.Census.DDec$n.y

w.GAR.Census.DDec <- w.GAR.Census.DDec %>% select(-n.x, -n.y)

# assign weights to Dalia data
DaliaDec <- left_join(DaliaDec, w.GAR.Census.DDec,
                      by = c("gender", "religion.cat", "AgeGroup30"))

# weights for march data
w.GAR.Census.DMar <- left_join(S.GAR.Census,
                               S.GAR.DMar,
                               by = c("gender","religion.cat","AgeGroup30"))
w.GAR.Census.DMar$w.GAR.Census.DMar <- (w.GAR.Census.DMar$n.x)/w.GAR.Census.DMar$n.y

w.GAR.Census.DMar <- w.GAR.Census.DMar %>% select(-n.x, -n.y)

# assign weights to Dalia data
DaliaMar <- left_join(DaliaMar, w.GAR.Census.DMar,
                      by = c("gender", "religion.cat", "AgeGroup30"))


### graph with new weights ####################################################
position <- c("Union", "SPD", "Gruene", "Linke", "FDP", "AfD", "Other")
farben = c("AfD" = "#009dd1","Union" = "#222222", "FDP" = "#ffb700", 
           "Gruene" = "#349f29", "Linke" = "#cc35a0", "SPD" = "#ce1b1b",
           "Other" = "grey")


# unweighted Dalia Poll December ##############################################
Polls <- DaliaDec %>% 
  filter(vote_nextelection_de != "No vote") %>% 
  count(vote_nextelection_de) %>%
  mutate(shares = 100*n / sum(n)) %>% transpose()

colnames(Polls) <- Polls[1,]
Polls <- Polls[-1,]
Polls[,"method"] <- c("GAV.uw.DDec.count", "GAV.uw.DDec.pct")
Polls[,"date"] <- ymd(replicate(2,"2016-12-10"))

# weighted Dalia Poll December
Poll.w.DDec <- DaliaDec %>% 
  filter(vote_nextelection_de != "No vote") %>% 
  count(vote_nextelection_de, wt = w.GAV.Exit.DDec) %>%
  mutate(shares = 100*n / sum(n)) %>% t() %>% as.data.frame()
colnames(Poll.w.DDec) <- as.character(unlist(Poll.w.DDec[1,]))
Poll.w.DDec <- Poll.w.DDec[-1,]
Poll.w.DDec[,"method"] <- c("GAV.w.DDec.count", "GAV.w.DDec.pct")
Poll.w.DDec[,"date"] <- ymd(replicate(2,"2016-12-10"))

# add to other polls
Polls <- rbind(Polls, Poll.w.DDec)
rm(Poll.w.DDec)

# unweighted Dalia Poll March
Poll.uw.DMar <- DaliaMar %>% 
  filter(vote_nextelection_de != "No vote") %>% 
  count(vote_nextelection_de) %>%
  mutate(shares = 100*n / sum(n)) %>% t() %>% as.data.frame()
colnames(Poll.uw.DMar) <- as.character(unlist(Poll.uw.DMar[1,]))
Poll.uw.DMar <- Poll.uw.DMar[-1,]
Poll.uw.DMar[,"method"]  <- c("GAV.uw.DMar.count", "GAV.uw.DMar.pct")
Poll.uw.DMar[,"date"] <- ymd(replicate(2,"2017-03-20"))

# add to other polls
Polls <- rbind(Polls, Poll.uw.DMar)
rm(Poll.uw.DMar)

# weighted Dalia Poll March GAV ###############################################
Poll.w.DMar <- DaliaMar %>% 
  filter(vote_nextelection_de != "No vote") %>% 
  count(vote_nextelection_de, wt = w.GAV.Exit.DMar) %>%
  mutate(shares = 100*n / sum(n)) %>% t() %>% as.data.frame()
colnames(Poll.w.DMar) <- as.character(unlist(Poll.w.DMar[1,]))
Poll.w.DMar <- Poll.w.DMar[-1,]
Poll.w.DMar[,"method"]  <- c("GAV.w.DMar.count", "GAV.w.DMar.pct")
Poll.w.DMar[,"date"] <- ymd(replicate(2,"2017-03-20"))

# add to other polls
Polls <- rbind(Polls, Poll.w.DMar)
rm(Poll.w.DMar)
Polls <- tibble::remove_rownames(Polls)

# weighted Dalia Poll December GAR ############################################
Poll.w.DDec.GAR <- DaliaDec %>% 
  filter(vote_nextelection_de != "No vote" & turnout_exp == 1) %>% 
  count(vote_nextelection_de, wt = w.GAR.Census.DDec) %>%
  mutate(shares = 100*n / sum(n)) %>% t() %>% as.data.frame()
colnames(Poll.w.DDec.GAR) <- as.character(unlist(Poll.w.DDec.GAR[1,]))
Poll.w.DDec.GAR <- Poll.w.DDec.GAR[-1,]
Poll.w.DDec.GAR[,"method"] <- c("GAR.w.DDec.count", "GAR.w.DDec.pct")
Poll.w.DDec.GAR[,"date"] <- ymd(replicate(2,"2016-12-10"))

# add to other polls
Polls <- rbind(Polls, Poll.w.DDec.GAR)
rm(Poll.w.DDec.GAR)

# weighted Dalia Poll March GAR ############################################
Poll.w.DMar.GAR <- DaliaMar %>% 
  filter(vote_nextelection_de != "No vote" & turnout_exp == 1) %>% 
  count(vote_nextelection_de, wt = w.GAR.Census.DMar) %>%
  mutate(shares = 100*n / sum(n)) %>% t() %>% as.data.frame()
colnames(Poll.w.DMar.GAR) <- as.character(unlist(Poll.w.DMar.GAR[1,]))
Poll.w.DMar.GAR <- Poll.w.DMar.GAR[-1,]
Poll.w.DMar.GAR[,"method"] <- c("GAR.w.DMar.count", "GAR.w.DMar.pct")
Poll.w.DMar.GAR[,"date"] <- ymd(replicate(2,"2016-03-20"))

# add to other polls
Polls <- rbind(Polls, Poll.w.DMar.GAR)
rm(Poll.w.DMar.GAR)

# save 
save(Polls, file = "./Processed/polls.RData")

### Plots #####################################################################

# unweighted December
poll.GAV.uw.DDec <- Polls %>% dplyr::filter(grepl("GAV.uw.DDec",method)) %>% 
          select(-method,-date) %>%  t() %>% as.data.frame %>% 
          tibble::rownames_to_column("vote_nextelection_de") %>% 
          rename(shares = V2) %>% mutate(shares = as.numeric(as.character(shares)))

pp1 <- poll.GAV.uw.DDec %>% shares.plot() + 
  ggtitle("Unweighted December")
save(poll.GAV.uw.DDec, file = "./Processed/GAV_uw_DDec.RData")

# weighted December
poll.GAV.w.DDec <- Polls %>% dplyr::filter(grepl("GAV.w.DDec",method)) %>% 
  select(-method,-date) %>%  t() %>% as.data.frame %>% 
  tibble::rownames_to_column("vote_nextelection_de") %>% 
  rename(shares = V2) %>% mutate(shares = as.numeric(as.character(shares)))

pp2 <- poll.GAV.w.DDec %>% shares.plot() + 
  ggtitle("Weighted December")
save(poll.GAV.w.DDec, file = "./Processed/GAV_w_DDec.RData")

# unweighted March
poll.GAV.uw.DMar <- Polls %>% dplyr::filter(grepl("GAV.uw.DMar",method)) %>% 
  select(-method,-date) %>%  t() %>% as.data.frame %>% 
  tibble::rownames_to_column("vote_nextelection_de") %>% 
  rename(shares = V2) %>% mutate(shares = as.numeric(as.character(shares)))

pp3 <- poll.GAV.uw.DMar %>% shares.plot() + 
  ggtitle("Unweighted March")
save(poll.GAV.uw.DMar, file = "./Processed/GAV_uw_DMar.RData")

# weighted March
poll.GAV.w.DMar <- Polls %>% dplyr::filter(grepl("GAV.w.DMar",method)) %>% 
  select(-method,-date) %>%  t() %>% as.data.frame %>% 
  tibble::rownames_to_column("vote_nextelection_de") %>% 
  rename(shares = V2) %>% mutate(shares = as.numeric(as.character(shares)))

pp4 <- poll.GAV.w.DMar %>% shares.plot() +
  ggtitle("Weighted March")

save(poll.GAV.w.DMar, file = "./Processed/GAV_w_DMar.RData")

plot.final <- multiplot(pp1, pp2, pp3, pp4, cols=2)
ggsave(filename = "./Grafiken/Plot_Final.png", plot = plot.final)

### Raw for paper

last.vote.DaliaMar <- DaliaMar %>% 
  select(voted_party_last_election_de) %>%
  count(voted_party_last_election_de) %>%
  mutate(shares = 100*n / sum(n)) %>%
  as.data.frame() %>% rename(vote_nextelection_de = voted_party_last_election_de)

pp5 <- last.vote.DaliaMar %>% shares.plot() +
  ggtitle("Self-reported vote 2013")
pp6 <- poll.GAV.uw.DMar %>% shares.plot() +
  ggtitle("Vote intent 2017")

last.vote.Mar.plot <- multiplot(pp5, pp6, cols = 2)
ggsave(filename = "./Grafiken/LastVoteMar.png", plot = last.vote.Mar.plot)




### Gewichtung nach demographischen Faktoren aus den Exit Polls ###############


# SZ Graph, benchmark table, mse computing
source("main_benchmarking.r")
