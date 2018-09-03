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

differentKindsOfVotes <<- c("For","Absent","Abstained","Against")

loadCountrySpecificData <- function() {
  
  message("loadCountrySpecificData country:")
  cat(str(country))
  
  countrySpecificPath <<- function(path) {
    paste("./", country, "/", path, sep="")
  }
  
  source(countrySpecificPath("parliament_voting_data.R"))
  
  scores <<- c(`Absent` = 0, `Against` = -2, `Abstained` = -1, `For` = 2)
  
}

getVotesThatMatchesTopicPatterns <- function(patterns) {
  if (length(patterns) == 0 || patterns[1] == "") {
    votingTopicsThatMatchesPatterns <- unique(all_votes$topic_voting)
  } else {
    votingTopicsThatMatchesPatterns <- unique(unlist(sapply(patterns, function(w) {
      grep(unique(all_votes$topic_voting), pattern = w, value = TRUE, fixed = TRUE)
    })))
  }
  selectionOfVotes <- all_votes %>%
    filter(topic_voting %in% votingTopicsThatMatchesPatterns)
}

getVotesThatMatchesVotingIds <- function(voting_ids) {
  if (length(voting_ids) == 0 || voting_ids[1] == "") {
    votingTopicsThatMatchesVotingId <- unique(all_votes$id_voting)
  } else {
    votingTopicsThatMatchesVotingId <- unique(unlist(sapply(voting_ids, function(w) {
      grep(unique(all_votes$id_voting), pattern = w, value = TRUE, fixed = TRUE)
    })))
  }
  selectionOfVotes <- all_votes %>%
    filter(id_voting %in% votingTopicsThatMatchesVotingId)
}

addUserVotes <- function(selectionOfVotes, userVotes) {
  if (length(userVotes) == 0) {
    return
  }
  selectableVotings <- getSelectableVotings(selectionOfVotes)
  index <- match(userVotes$id_voting, selectableVotings$id_voting)
  userVotes$voter_id <- "-1"
  userVotes$party <- "ME"
  userVotes$vote <- as.character(userVotes$vote)
  userVotes$id_voting <- as.character(userVotes$id_voting)
  userVotes$voter_name <- "Me"
  userVotes$voter_district <- "Here"
  userVotes$voting_db_record_id <- sample(1000000000:2000000000,nrow(userVotes),replace=T)
  # voting metadata
  userVotes$topic_voting <- selectableVotings[index, ]$topic_voting
  userVotes$date_meeting <- selectableVotings[index, ]$date_meeting
  userVotes$period <- selectableVotings[index, ]$period
  userVotes$duf_db_record_id <- selectableVotings[index, ]$duf_db_record_id
  userVotes$document_db_record_id <- selectableVotings[index, ]$document_db_record_id
  userVotes$voting_related_document_ids <- selectableVotings[index, ]$voting_related_document_ids
  userVotes$more_info_url_voting <- selectableVotings[index, ]$more_info_url_voting
  userVotes$description_voting <- selectableVotings[index, ]$description_voting
  selectionOfVotes <- rbind(selectionOfVotes, userVotes)
}

getSelectableVotings <<- function(all_votes) {
  selectableVotingTopics <- getSelectableVotingTopics(all_votes)
  votesWithSelectableVotingTopics <- all_votes %>%
    filter(topic_voting %in% selectableVotingTopics)
  selectableVotings <- votesWithSelectableVotingTopics %>% distinct(id_voting, .keep_all = TRUE) # removes duplicates in relation to the id_voting column
  selectableVotings <- votesWithSelectableVotingTopics %>% distinct(topic_voting, .keep_all = TRUE) # removes duplicates in relation to the topic_voting column
}

getSelectableVotingChoices <<- function(all_votes) {
  selectableVotings <- getSelectableVotings(all_votes)
  selectableVotingTopics <- setNames(selectableVotings$id_voting,selectableVotings$topic_voting)
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
  
  partiesAndTheirVotes <- table(factor(selectionOfVotes$party),factor(selectionOfVotes$vote, differentKindsOfVotes))[,differentKindsOfVotes]
  tt<-with(selectionOfVotes, partiesAndTheirVotes)
  partiesAndTheirVotes_sortedByMostFrequent <- tt[order(tt[,2], decreasing=T),]
  
  mosaicplot(partiesAndTheirVotes_sortedByMostFrequent, off = c(0,0), border="white",
             color=c("green3", "grey", "red4", "red2"), las=2,
             main="")
}
