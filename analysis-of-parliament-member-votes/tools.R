if (!require(cluster)) install.packages("cluster", dependencies = TRUE);
if (!require(ggdendro)) install.packages("ggdendro", dependencies = TRUE);
if (!require(ape)) install.packages("ape", dependencies = TRUE);
if (!require(RColorBrewer)) install.packages("RColorBrewer", dependencies = TRUE);
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE);
if (!require(tidyr)) install.packages("tidyr", dependencies = TRUE);
if (!require(parallel)) install.packages("parallel", dependencies = TRUE);
if (!require(dendextend)) install.packages("dendextend", dependencies = TRUE);

library(cluster)
library(ggdendro)
library(ape)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(parallel)
library(dendextend)

loadCountrySpecificData <- function() {
  
  message("loadCountrySpecificData country:")
  cat(str(country))
  
  countrySpecificPath <<- function(path) {
    paste("./", country, "/", path, sep="")
  }
  
  source(countrySpecificPath("parliament_voting_data.R"))
  
  scores <<- c(`Absent` = 0, `Against` = -2, `Abstained` = -1, `For` = 2)
  
}

getVotesThatMatchesTopicPattern <- function(pattern) {
  if (length(pattern) == 0 || pattern[1] == "") {
    votingTopicsThatMatchesPattern <- unique(all_votes$topic_voting)
  } else {
    votingTopicsThatMatchesPattern <- unique(unlist(sapply(pattern, function(w) {
      grep(unique(all_votes$topic_voting), pattern = w, value = TRUE, fixed = TRUE)
    })))
    # votingTopicsThatMatchesPattern <- grep(unique(all_votes$topic_voting), pattern = pattern, value = TRUE)
  }
  selectionOfVotes <- all_votes %>%
    filter(topic_voting %in% votingTopicsThatMatchesPattern)
}

crunchVotingData <- function(selectionOfVotes) {
  
  voterIdsVsPartiesOccurances <- table(selectionOfVotes$voter_id, selectionOfVotes$party)
  voterIdsAndAllTheirParties <- apply(voterIdsVsPartiesOccurances, 1, function(x) paste(colnames(voterIdsVsPartiesOccurances)[x>0], collapse=","))
  voterIdsAndTheirMostFrequentParty <- apply(voterIdsVsPartiesOccurances, 1, function(x) paste(colnames(voterIdsVsPartiesOccurances)[which.max(x)], collapse=","))
  
  voterIdsVsVoterNameOccurances <- table(selectionOfVotes$voter_id, selectionOfVotes$voter_name)
  voterIdsAndTheirVoterName <- apply(voterIdsVsVoterNameOccurances, 1, function(x) paste(colnames(voterIdsVsVoterNameOccurances)[x>0], collapse=","))
  
  # replace the vote column with their numeric scores
  selectionOfVotes$vote_score <- scores[as.character(selectionOfVotes$vote)]
  
  votersAndTheirVotes_ <- spread(selectionOfVotes[,c("voter_id","vote_score","id_voting")], key = id_voting, value = vote_score)
  voterIds <- votersAndTheirVotes_[,1]
  rownames(votersAndTheirVotes_) <- voterIds
  
  votersAndTheirVotes <- votersAndTheirVotes_[,-1,drop=FALSE] # removes the voter_id column, leaving only the votes (columns) of each voter (rows)
  
  # only include parliament members that have voted on at least 90% of the votings
  voteFilter <- rowMeans(is.na(votersAndTheirVotes)) < 0.1
  partyRepresentedByEachVote <- voterIdsAndTheirMostFrequentParty[voteFilter]
  consistentVotersAndTheirVotes <- votersAndTheirVotes[voteFilter,]
  consistentVotersAndTheirVoterName <- voterIdsAndTheirVoterName[voteFilter]
  dVotes <- dist(consistentVotersAndTheirVotes)
  
  ag <- agnes(dVotes, method = "average")
  hc = as.hclust(ag)
  labels(hc) <- paste(consistentVotersAndTheirVoterName[order.hclust(hc)], " - ", voterIdsAndAllTheirParties[voteFilter][order.hclust(hc)], sep="")
  return(list(hc=hc, partyRepresentedByEachVote=partyRepresentedByEachVote))
}

phyloPlot <- function(hc, plotType, tipColor, title="") {
  if (plotType %in% c("phylogram", "cladogram")) {
    par(mar=c(0,0,0,0), xpd=NA)
    cex <- 2.5
    rotateDegrees <- 0
  } else {
    par(mar=c(1,1,2,1), xpd=NA) # , font=2, family="mono")
    cex <- 0.4
    rotateDegrees <- -85
  }
  if (plotType == "unrooted") {
    edgeWidth <- 1
  } else {
    edgeWidth <- 2
  }
  
  plot(as.phylo(hc), type = plotType, cex = cex,
       tip.color = tipColor,
       edge.width = edgeWidth,
       no.margin = TRUE,
       main=title,
       rotate.tree=rotateDegrees)
}

phyloPlotPlain <- function(hc, plotType, tipColor, title="") {
  plot(as.phylo(hc), type = plotType, cex = 0.85,
       tip.color = tipColor,
       main=title,
       rotate.tree=-85)
}

plotVotingDirectionPartyOverview <- function(selectionOfVotes) {
  
  differentKindsOfVotes <- c("For","Absent","Abstained","Against")
  partiesAndTheirVotes <- table(factor(selectionOfVotes$party),factor(selectionOfVotes$vote, differentKindsOfVotes))[,differentKindsOfVotes]
  tt<-with(selectionOfVotes, partiesAndTheirVotes)
  partiesAndTheirVotes_sortedByMostFrequent <- tt[order(tt[,2], decreasing=T),]
  
  mosaicplot(partiesAndTheirVotes_sortedByMostFrequent, off = c(0,0), border="white",
             color=c("green3", "grey", "red4", "red2"), las=2,
             main="")
}
