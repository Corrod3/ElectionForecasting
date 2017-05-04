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

source("packages.R")

### 1. get data + cleaning ####################################################

source("daliadata.R")
source("weightingdata.R")
# add benchmark data

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
                        rename(parties = voted_party_last_election_de) %>% 
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

# format as.double (why not just as.numeric?)
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
DaliaDec <- left_join(DaliaDec, w.GAV.Exit.DDec, by = c("gender", "voted_party_last_election_de", "AgeGroup"))

# March wave
w.GAV.Exit.DMar <- left_join(S.GAV.Exit, S.GAV.DMar, by = c("gender", "parties", "AgeGroup"))
w.GAV.Exit.DMar$w.GAV.Exit.DMar <- (w.GAV.Exit.DMar$vote.share.exit)/w.GAV.Exit.DMar$vote.share.dalia
w.GAV.Exit.DMar <- w.GAV.Exit.DMar %>% rename(voted_party_last_election_de = parties) %>%
                        select(-vote.share.exit, -vote.share.dalia)

# assign weights to Dalia data
DaliaMar <- left_join(DaliaMar, w.GAV.Exit.DMar, by = c("gender", "voted_party_last_election_de", "AgeGroup"))


### graph with new weights ####################################################
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
  
  #ggsave(file = "./Grafiken/plot.png")
}


uw.shares <- DaliaMar %>% 
  filter(vote_nextelection_de != "No vote") %>% 
  count(vote_nextelection_de) %>%
  mutate(shares = 100*n / sum(n)) %>% transpose()

w.shares <- DaliaMar %>% 
  filter(vote_nextelection_de != "No vote") %>% 
  count(vote_nextelection_de, wt = w.GAV.Exit.DMar) %>%
  mutate(shares = 100*n / sum(n))

w.shares %>% shares.plot()
uw.shares %>% shares.plot()

w.sharesDec <- DaliaDec %>% 
  filter(vote_nextelection_de != "No vote") %>% 
  count(vote_nextelection_de, wt = w.GAV.Exit.DDec) %>%
  mutate(shares = 100*n / sum(n))

w.sharesDec %>% shares.plot()

save(w.shares, file = "./Processed/w.march.plot.RData")

### Gewichtung nach demographischen Faktoren aus den Exit Polls ###############









#####################
### Direct method ###
#####################
# 1. Load exit poll data (either KAS or election statistics)
load("./Processed/Vote_Age_Gender_2013.RData")

# next steps: compute percentages for Strata.2;
# merge CDU/CSU
# make both same format
# divide weights = vote.2013 / vote.2017


# big problem: no AfD data in Exit Polls from 2013 -> computable?


# 2. Compute weights for gender, age, vote



# 2.1 Compute weights for age group and vote


# 2.2 compute weights for age and gender without party



# 3. Apply weights on Dalia Data

########################
### Indirect methods ###
########################
# 1. Get data from census
# 2. Compute weights.
# 3. Apply on data.class
# 4. Create deterministic or probabilistic measure of likely voter
# 5. Apply on Dalia data
