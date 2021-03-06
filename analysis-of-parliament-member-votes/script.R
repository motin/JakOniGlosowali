# set working directory to the the path of the current open file in RStudio
if (!require(rstudioapi)) install.packages("rstudioapi", dependencies = TRUE);
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# country to work with
# country <- "pl"
country <- "se"

# include data and common functions
source("tools.R")

# load country-specific data
loadCountrySpecificData()

# settings for plot exports
defaultWidth <- 2000
defaultHeight <- 2000
defaultPointSize <- 40
pdf.options(encoding='ISOLatin2.enc')

numCores <- detectCores() # get the number of cores available

selectionOfVotes <- getVotesThatMatchesTopicPatterns(pattern)

# with additional user votes
# selectionOfVotes <- getVotesThatMatchesVotingIds("8DB8105A-2ACC-44C2-972D-B7A5F8DF07D9")
# userVotes <- data.frame(id_voting=c("8DB8105A-2ACC-44C2-972D-B7A5F8DF07D9"), vote=c("For"))
userVotes <- data.frame(voting_id=character(),vote=character())
if (nrow(userVotes) > 0) {
  selectionOfVotes <- addUserVotes(selectionOfVotes, userVotes)
}

png(countrySpecificPath("plotA_mosaicplot.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
plotVotingDirectionPartyOverview(selectionOfVotes)
dev.off()

votingData <- crunchVotingData(selectionOfVotes)

hc <- votingData$hc
partyRepresentedByEachVote <- votingData$partyRepresentedByEachVote

png(countrySpecificPath("plot5_fan.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
phyloPlot(hc, plotType="fan", partyColors[partyRepresentedByEachVote])
dev.off()

png(countrySpecificPath("plot4_unrooted.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
phyloPlot(hc, plotType="unrooted", partyColors[partyRepresentedByEachVote])
dev.off()

png(countrySpecificPath("plot5alt_radial.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
phyloPlot(hc, plotType="radial", partyColors[partyRepresentedByEachVote])
dev.off()

png(countrySpecificPath("plot3_phylogram.png"),
    width=as.integer(defaultWidth*2.0),
    height=as.integer(defaultHeight*2.0),
    pointsize=15,
)
phyloPlot(hc, plotType="phylogram", partyColors[partyRepresentedByEachVote])
dev.off()

png(countrySpecificPath("plot3_phylogram_long.png"),
    width=as.integer(defaultWidth*2.0),
    height=as.integer(defaultHeight*4.0),
    pointsize=15,
)
phyloPlot(hc, plotType="phylogram", partyColors[partyRepresentedByEachVote])
dev.off()

png(countrySpecificPath("plot3alt_cladogram.png"),
    width=as.integer(defaultWidth*2.0),
    height=as.integer(defaultHeight*2.0),
    pointsize=15,
)
phyloPlot(hc, plotType="cladogram", partyColors[partyRepresentedByEachVote])
dev.off()

# plots not mentioned in the original article

uniqueVoterIds <- levels(factor(selectionOfVotes$voter_id))

distMat <- matrix(NA, length(uniqueVoterIds),  length(uniqueVoterIds))
colnames(distMat) <- uniqueVoterIds
rownames(distMat) <- uniqueVoterIds

recalculateDistMat <- TRUE
if (recalculateDistMat) {
  system.time({
    
    iMax <- (length(uniqueVoterIds)-1)
    jMax <- length(uniqueVoterIds)
    
    message("Pre-calculating lists of personal votes")
    results_selectionOfVotes <- mclapply(1:jMax,
                                         FUN=function(i) {
                                           selectionOfVotesI <- selectionOfVotes %>%
                                             filter(voter_id %in% uniqueVoterIds[i])
                                         },
                                         mc.cores = numCores)
    
    message("Summarizing intersections between member votes")
    for (i in 1:iMax) {
      cat("\n",i," ", uniqueVoterIds[i]," ")
      for (j in i:jMax) {
        selectionOfVotesI <- results_selectionOfVotes[[i]]
        selectionOfVotesJ <- results_selectionOfVotes[[j]]
        
        selIJ <- merge(selectionOfVotesI[,c(1,2,3,4)], selectionOfVotesJ[,c(1,2,3,4)], by="id_voting")
        
        distMat[i,j] = mean(selIJ$vote.x == selIJ$vote.y)
        distMat[j,i] = mean(selIJ$vote.x == selIJ$vote.y)
        cat(".")
      }
    }
    
  })
  save(distMat, file = countrySpecificPath("distMat.rda"))
} else {
  load(file = countrySpecificPath("distMat.rda"))
}

rem <- which(rowMeans(is.na(distMat)) > 0.01)
distMatR <- distMat[-rem, -rem]
rownames(distMatR) <- uniqueVoterIds[-rem]
colnames(distMatR) <- uniqueVoterIds[-rem]

if (!require(MASS)) install.packages("MASS", dependencies = TRUE);
library(MASS)

space <- isoMDS(as.dist(1.001-distMatR), k=2)
df <- data.frame(space$points, parties=voterIdsAndTheirMostFrequentParty[-rem], voterIds=uniqueVoterIds[-rem], voterNames=voterIdsAndTheirVoterName[-rem])

if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE);
library(ggplot2)

# a plot not mentioned in the original article
png(countrySpecificPath("plotB_aes.png"),
    width=as.integer(defaultWidth/3),
    height=as.integer(defaultHeight/3),
    pointsize=defaultPointSize
)
ggplot(df, aes(X1, X2, color=parties, label=voterNames)) +
  geom_text(size=4) +
  theme_bw() + scale_shape_manual(values=LETTERS)
dev.off()

ag2 <- agnes(as.dist(1.001-t(distMatR)), method = "average")

colors <- brewer.pal(9,"Set1")

hc2 = as.hclust(ag2)
labels(hc2) <- paste(df$voterNames[order.hclust(hc2)], " - ", df$parties[voteFilter][order.hclust(hc2)], sep="")

par(mar=c(0,0,2,0))

# another plot not mentioned in the original article
png(countrySpecificPath("plotC_phylo.png"),
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize
)
plot(as.phylo(hc2), type = "fan", cex = 0.4,
     tip.color = colors[as.numeric(factor(voterIdsAndTheirMostFrequentParty[-rem]))],
     main=pattern)
dev.off()
