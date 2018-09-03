library(dplyr)

# colors courtesy of https://gist.github.com/peterdalle/9088e2af257e59b8f5e3de1a8118d786
partyColors <<- c(V = "#aa2016", # a bit darker vs the color in the original source (#DA291C)
                 S = "#E8112d",
                 MP = "#83CF39",
                 C = "#009933",
                 FP = "#006AB3",
                 L = "#006AB3",
                 M = "#52BDEC",
                 KD = "#000077",
                 SD = "#b2b200", # a bit darker vs the color in the original source (#DDDD00)
                 FI = "#CD1B68",
                 PP = "#572B85",
                 `-` = "grey")

partyNames <<- c(V = "Vänsterpartiet",
                S = "Socialdemokraterna",
                MP = "Miljöpartiet",
                C = "Centern",
                FP = "Folkpartiet",
                L = "Liberalerna",
                M = "Moderaterna",
                KD = "Kristdemokraterna",
                SD = "Sverigedemokraterna",
                FI = "Feministiskt initiativ",
                PP = "Piratpartiet",
                `-` = "(Inget parti)")

par(mfrow=c(3,4)) 
image(as.matrix(1:10, 1:10), col=partyColors[["V"]], main=partyNames[["V"]])
image(as.matrix(1:10, 1:10), col=partyColors[["S"]], main=partyNames[["S"]])
image(as.matrix(1:10, 1:10), col=partyColors[["MP"]], main=partyNames[["MP"]])
image(as.matrix(1:10, 1:10), col=partyColors[["C"]], main=partyNames[["C"]])
image(as.matrix(1:10, 1:10), col=partyColors[["FP"]], main=partyNames[["FP"]])
image(as.matrix(1:10, 1:10), col=partyColors[["L"]], main=partyNames[["L"]])
image(as.matrix(1:10, 1:10), col=partyColors[["M"]], main=partyNames[["M"]])
image(as.matrix(1:10, 1:10), col=partyColors[["KD"]], main=partyNames[["KD"]])
image(as.matrix(1:10, 1:10), col=partyColors[["SD"]], main=partyNames[["SD"]])
image(as.matrix(1:10, 1:10), col=partyColors[["FI"]], main=partyNames[["FI"]])
image(as.matrix(1:10, 1:10), col=partyColors[["PP"]], main=partyNames[["PP"]])

load(countrySpecificPath("all_votes.rda"))

# translate the vote column
all_votes$vote[all_votes$vote == "Frånvarande"] <- "Absent"
all_votes$vote[all_votes$vote == "Nej"] <- "Against"
all_votes$vote[all_votes$vote == "Avstår"] <- "Abstained"
all_votes$vote[all_votes$vote == "Ja"] <- "For"

# make party column uppercase
all_votes$party <- toupper(all_votes$party)

# debug - echo the unique party values in the data
levels(factor(all_votes$party))

# Folkpartiet changed name to Liberalerna - still the same party so we treat all FP votes as L votes
all_votes$party[all_votes$party == "FP"] <- "L"

# Only include votes from a specific period
all_votes <- all_votes %>%
  filter(period %in% c("2014/15", "2015/16", "2016/17", "2017/18"))

getSelectableVotingTopics <<- function(all_votes) {
    selectableVotingTopics <<- grep(unique(all_votes$topic_voting), pattern = "", value=TRUE)
}

# optionally filter votes on specific topics (an empty pattern = no filter)
pattern <<- ""
