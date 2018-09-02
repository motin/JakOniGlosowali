library(cluster)
library(ggdendro)
library(ape)
library(RColorBrewer)
library(dplyr)
library(tidyr)

# country to work with
# country <- "pl"
country <- "se"

countrySpecificPath <- function(path) {
  paste("./", country, "/", path, sep="")
}

source(countrySpecificPath("parliament_voting_data.R"))

scores <- c(`Absent` = 0, `Against` = -2, `Abstained` = -1, `For` = 2)

getSpeakerDendro <- function(params) {
  cat(params)
  typ <- params[1]
  pattern <- params[-1]
  
  if (length(pattern) == 0 || pattern[1] == "") {
    tytulyGlos <- unique(all_votes$topic_voting)
  } else {
    tytulyGlos <- unique(unlist(sapply(pattern, function(w) {
      grep(unique(all_votes$topic_voting), pattern = w, value = TRUE, fixed = TRUE)
    })))
  }
  
  selVotes <- all_votes %>%
    filter(topic_voting %in% tytulyGlos)
  
  tabi <- table(selVotes$voter_id, selVotes$party)
  clubs <- apply(tabi, 1, function(x) paste(colnames(tabi)[x>0], collapse=","))
  clubs2 <- apply(tabi, 1, function(x) paste(colnames(tabi)[which.max(x)], collapse=","))
  
  selVotes$vote <- scores[as.character(selVotes$vote)]
  
  tVotes <- spread(selVotes[,c(1,3,4)], key = id_voting, value = vote)
  rownames(tVotes) <- paste(" ", tVotes[,1], " - ", clubs[as.character(tVotes[,1])], " ",sep="")
  tVotes <- tVotes[,-1]
  
  # tylko Ci w sejmie na ponad 90% glosowan
  cVotes <- clubs2[rowMeans(is.na(tVotes)) < 0.1]
  tVotes <- tVotes[rowMeans(is.na(tVotes)) < 0.1,]
  
  dVotes <- dist(tVotes)
  
  ag <- agnes(dVotes, method = "average")
  hc = as.hclust(ag)
  
  par(mar=c(1,1,2,1), xpd=NA, font=2, family="mono")
  
  plot(as.phylo(hc), type = typ, cex = 0.85,
       tip.color = partyColors[cVotes],
       main=paste(paste(pattern, collapse = "\n"), "(głosowań:",length(unique(selVotes$id_voting)),")"),
       rotate.tree=-85)
}



getSpeakerDendro2 <- function(params) {
  typ <- params[1]
  pattern <- params[-1]
  cat(pattern)
  
  if (length(pattern) == 0 || pattern[1] == "") {
    tytulyGlos <- unique(all_votes$topic_voting)
  } else {
    tytulyGlos <- unique(unlist(sapply(pattern, function(w) {
      grep(unique(all_votes$topic_voting), pattern = w, value = TRUE, fixed = TRUE)
    })))
  }
  
  selVotes <- all_votes %>%
    filter(topic_voting %in% tytulyGlos)
  
  tabi2 <- table(selVotes$party,selVotes$vote)[,c(4,1,3,2)]
  par(mar=c(1,1,2,1), xpd=NA)
  mosaicplot(tabi2, off = c(0,0), border="white", 
             color=c("green3", "grey", "red4", "red2"), las=2,
             main="")
}
