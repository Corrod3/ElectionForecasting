### Data mining ###############################################################

source("packages.R")
source("daliadata.R")

### December

# residency
plot(DaliaDec$residency)


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
turnout.plot <- sjp.xtab(DaliaDec$turnout_exp, DaliaDec$gender,
                         title = "Expected turnout by gender", 
                         axis.titles = c("Self-reported turnout by gender",
                                         "Expected federal election turnout"),
                         axis.labels = c("No, I will (probably) not vote",
                                         "Yes, I will (probably) vote"),
                         geom.size = 0.5, 
                         geom.colors = c("cadetblue","cadetblue3") ,
                         legend.title = "Gender", 
                         legend.labels = c("Male", "Female"), 
                         coord.flip=TRUE, 
                         hjust = "top", 
                         show.total = FALSE,
                         show.n = FALSE)

# with Dalia weights
turnout.plot.w <- sjp.xtab(DaliaDec$turnout_exp, DaliaDec$gender, 
                           weight.by=DaliaDec$weight,
                           title = "Expected turnout by gender (unweighted)", 
                           axis.titles = c("Vote intent by gender",
                                           "Expected federal election turnout"),
                           axis.labels = c("No, I will (probably) not vote", 
                                           "Yes, I will (probably) vote"),
                           geom.size = 0.5, legend.title = "Gender", 
                           legend.labels = c("Male", "Female"), 
                           coord.flip=TRUE, hjust = "top",show.n = FALSE)

# why are the turnouts so high? What can we do about that?
# probably (self-)selection bias within demographic groups 
# A: do we have a possiblity to look tournout of 2013?
# according to our data the turnout 2013 was 78,03%, in reality it was 71,5%

 turnout.2013 <- 1 - 
                length(DaliaDec$voted_party_last_election_de[DaliaDec$age > 22 & 
                           DaliaDec$voted_party_last_election_de == "No vote"])/
                length(DaliaDec$voted_party_last_election_de[DaliaDec$age > 22 & 
                           DaliaDec$voted_party_last_election_de != "Not able"])


# filter not vote and not able
DaliaDec_temp <- filter(DaliaDec, 
                       !(voted_party_last_election_de == "No vote" |
                         voted_party_last_election_de == "Not able"
                         ))
DaliaDec_temp$voted_party_last_election_de <- 
                              factor(DaliaDec_temp$voted_party_last_election_de,
                                     levels = c("Union", "SPD",  "Linke", "Gruene", 
                                                "FDP" ,"AfD" , "Other" ))

# last election vote without weights
sjp.frq(DaliaDec_temp$voted_party_last_election_de,
        title = "Party vote share last federal election (unweighted)", 
        axis.title = c("Party vote share", "Parties"),
        sort.frq = c("asc"),
        geom.size = 0.5,
        coord.flip=TRUE,
        show.axis.values = FALSE)

# weighted
sjp.frq(DaliaDec_temp$voted_party_last_election_de, 
        title = "Party vote share last federal election (weighted)", 
        axis.title = c("Party vote share","Parties"),
        sort.frq = c("asc"),
        geom.size = 0.5,
  #      show.ci = TRUE, # confidence intervals?? how calculated?
        coord.flip=TRUE,
        show.axis.values = FALSE,
        weight.by = DaliaDec_temp$weight)

# FAILURE: labels are completely mixed up after weighting

# ggplot last election forecasts
# Problem: AfD over represented, FDP under representated (biased memory?)
#          CDU under representated
ggplot(DaliaDec_temp, aes(x = voted_party_last_election_de)) +
       geom_bar(aes(y = (..count..)/sum(..count..))) +
       theme_bw() +
       coord_flip() # flip sides

ggplot(DaliaDec_temp, aes(x = voted_party_last_election_de, weight = weight)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme_bw() +
  coord_flip() # flip sides


# remove No vote
DaliaDec_temp <- filter(DaliaDec, vote_nextelection_de != "No vote")
# reorder
DaliaDec_temp$vote_nextelection_de <- factor(DaliaDec_temp$vote_nextelection_de,
                                            levels = c("Union", "SPD",  
                                                       "Linke", "Gruene", 
                                                       "FDP" ,"AfD" , "Other"))

sjp.frq(DaliaDec_temp$vote_nextelection_de,
        title = "Vote next federal election (unweighted)", 
        axis.title = c("Parties", "Party vote share"),
        sort.frq = c("asc"),
        geom.size = 0.5,
        geom.colors = "cadetblue",
        coord.flip = TRUE,
        show.axis.values = FALSE)

sjp.frq(DaliaDec_temp$vote_nextelection_de,
        title = "Vote at next federal election", 
        axis.title = c("Party vote share","Parties"),
        sort.frq = c("asc"),
        geom.size = 0.6,
        coord.flip=TRUE,
        show.axis.values = FALSE,
        weight.by = DaliaDec_temp$weight,
        title.wtd.suffix = " (weighted)")

# mit weights verschieben sich wieder die label -> scheisse

# ggplot ohne verschobene labels
ggplot(DaliaDec_temp, aes(x = vote_nextelection_de, weight = weight)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme_bw() +
  coord_flip() # flip sides

#rm(DaliaDec_temp)

# Party ranking ################################################################
# comment to Dalia: Include in ranking: I prefer not to vote 
#                   -> allows to capture non-voters

# split ranking
# test with one string
# strsplit("Die Linke | Bündnis 90 / Die Grünen | 
#           SPD – Sozialdemokratische Partei Deutschlands | 
#           FDP - Freie Demokratische Partei | 
#           CDU/CSU – Christlich Demokratische Union/Christlich Soziale Union | 
#           AfD – Alternative für Deutschland"
#          , split = " | ", fixed = TRUE)

# list of all splitted strings
DaliaDec$ranking1 <- sapply(strsplit(as.character(DaliaDec$ranking_party_de), 
                                     split = " | ", fixed = TRUE),
                            function(x) x[[1]])
DaliaDec$ranking2 <- sapply(strsplit(as.character(DaliaDec$ranking_party_de), 
                                     split = " | ", fixed = TRUE),
                            function(x) x[[2]])
DaliaDec$ranking3 <- sapply(strsplit(as.character(DaliaDec$ranking_party_de), 
                                     split = " | ", fixed = TRUE),
                            function(x) x[[3]])
DaliaDec$ranking4 <- sapply(strsplit(as.character(DaliaDec$ranking_party_de), 
                                     split = " | ", fixed = TRUE),
                            function(x) x[[4]])
DaliaDec$ranking5 <- sapply(strsplit(as.character(DaliaDec$ranking_party_de), 
                                     split = " | ", fixed = TRUE),
                            function(x) x[[5]])
DaliaDec$ranking6 <- sapply(strsplit(as.character(DaliaDec$ranking_party_de), 
                                     split = " | ", fixed = TRUE),
                            function(x) x[[6]])



# How voters 1 to 2nd preferences are related
# plot function for flow chart
source("transitionplot.R")

# generate data frame relating rank1 to rank2 for transtion pot
SecPref <- as.data.frame(table(rank1 = DaliaDec$ranking1, 
                               rank2 = DaliaDec$ranking2))
SecPref <- spread(SecPref, rank2, Freq)

# transition plot (see transistion plot script for example)
transitionPlot(as.matrix(SecPref[,-1]),  
               box_txt = as.character(SecPref[,1]))

# most preferred party (consistency check with vote for next election/identify divergencies?)
DaliaDec$ranking1 <- as.factor(DaliaDec$ranking1)

# create data frame for experimenting -> adds ranking1 to DaliaDec_temp
DaliaDecPlot <- merge(DaliaDec_temp[,c("identifier", 
                                       "vote_nextelection_de")], 
                      DaliaDec[,c("identifier", "ranking1")],
                      by = "identifier")

# relabel and reorder factors
levels(DaliaDecPlot$ranking1) <- c("AfD", "Gruene", "Union", "Linke", "FDP", "SPD")
DaliaDecPlot$ranking1 <-
  factor(DaliaDecPlot$ranking1,
         levels = c("Union", "SPD",  "Linke", "Gruene", 
                    "FDP" ,"AfD"))

# remove individuals who could not choose their party in the ranking question
DaliaDecPlot <- DaliaDecPlot %>% filter(vote_nextelection_de != "Other")


# plot comparing number of vote intention and ranking1
vote.rank.plot <- ggplot(melt(DaliaDecPlot, id.vars = "identifier"), aes(x=value, fill = variable)) +
  geom_bar(position = 'dodge') +
  theme_bw() +
  coord_flip() # flip sides

# voter loyality ##############################################################
# idea: compare past self-reported voting with intended voting?!
# (how about non-voters / too young voters)

VoteLast <- as.data.frame(table(
  last.vote = DaliaDec$voted_party_last_election_de,
  next.vote = DaliaDec$vote_nextelection_de))
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
VoteLast <- VoteLast[c("Union", "SPD", "Gruene", "Linke", "FDP", "AfD"),]
VoteLast <- VoteLast[,c("Union", "SPD", "Gruene", "Linke", "FDP", "AfD", "No vote", "Other")]

# loyality: last vote = next vote / total respondents per party (last election)
VoteLast$loyality <- diag(as.matrix(VoteLast))/rowSums(VoteLast)

# Table for 2013 -> 2017 voter transitions 
# Weil die Loyalitätsstroeme so fett sind lassen die anderen sich kaum unterscheiden :/
transitionPlot(as.matrix(VoteLast[,c(1:6)]),  
               box_txt = c("Union", "SPD", "Gruene", "Linke", "FDP", "AfD"))

transitionMatrix <- as.matrix(VoteLast[,c(1:6)])
diag(transitionMatrix) <- 0
transitionPlot(transitionMatrix,  
               box_txt = c("Union", "SPD", "Gruene", "Linke", "FDP", "AfD"))

# alternative voter loyality: certainty_party_to_vote
ggplot(filter(DaliaDec, vote_nextelection_de != "I would not vote"),
       aes(x = vote_nextelection_de, fill = certainty_party_to_vote)) +
  geom_bar() +
  coord_flip() # flip sides

# likelihood to vote
ggplot(filter(DaliaDec, vote_nextelection_de != "I would not vote"),
       aes(x = vote_nextelection_de, fill = vote_next_national_election)) +
  geom_bar() +
  coord_flip() # flip sides


