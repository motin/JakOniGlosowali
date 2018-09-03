partyColors <<- c(PO = "orange3", PiS = "blue4", RP = "gold3", PSL="green4", SLD="red3", SP="blue1",
                  `niez.` = "grey", ID="gold4", TR="gold3", KPSP="blue2", BiG="orange2",
                  ZP="blue2", BC ="blue2" )
# Nieobecny = Absent, Przeciw = Against, Wstrzymał się = Abstained, Za = For

if (exists("all_votes")) {
  voteDataCountry <- all_votes$country[1]
} else {
  voteDataCountry <- NULL
}
if (is.null(voteDataCountry) || voteDataCountry != "pl") {
  message("Loading country data since it is not loaded yet")
  
  load(countrySpecificPath("all_votes.rda"))
  all_votes$country[1] <- country
  
  # translate the vote column
  all_votes$vote[all_votes$vote == "Nieobecny"] <- "Absent"
  all_votes$vote[all_votes$vote == "Przeciw"] <- "Against"
  all_votes$vote[all_votes$vote == "Wstrzymał się"] <- "Abstained"
  all_votes$vote[all_votes$vote == "Za"] <- "For"
  
  # the data column "club" stands for which party/parties the member was a member of
  # translating here to "party" for clarity
  colnames(all_votes)[colnames(all_votes)=="club"] <- "party"
  
  # renaming column "surname_name" to "voter_name" for clarity
  colnames(all_votes)[colnames(all_votes)=="surname_name"] <- "voter_name"
  
  # using column "voter_name" as a surrogate for "voter_id" (works as long as members have unique names)
  all_votes$voter_id <- all_votes$voter_name
  
  # debug
  # head(grep(unique(all_votes$topic_voting), pattern = "szkolnict", value = TRUE))
  
  # create our own grouping of voting topics, since the original topic_voting values are different even
  # when they represent the same topic of voting
  message("Creating our own grouping of voting topics")
  # nazwy ustaw = names of laws
  # ustawy = bill
  # zmianie ustawy = change of the bill
  votings <- all_votes %>% distinct(id_voting, .keep_all = TRUE) # removes duplicates in relation to the id_voting column
  votings_a <- dplyr::filter(votings, grepl("ustawy o",topic_voting))
  votings_b <- votings_a
  votings_b$selectable_topic_of_voting <- sapply(votings_b$topic_voting, function(x) {
    x <- paste(strsplit(x, split= "ustawy o")[[1]][-1], collapse= "ustawy o")
  })
  #votings_c$selectable_topic_of_voting <- (gsub(votings_b$selectable_topic_of_voting, pattern="^.*ustawy o *", replacement = ""))
  votings_c <- votings_b
  votings_c$selectable_topic_of_voting <- (gsub(votings_b$selectable_topic_of_voting, pattern="^ zmianie ustawy - *", replacement = ""))
  votings_d <- votings_c
  votings_d$selectable_topic_of_voting <- (gsub(votings_c$selectable_topic_of_voting, pattern="^ zmianie ustawy o *", replacement = ""))
  votings_e <- votings_d
  votings_e$selectable_topic_of_voting <- (gsub(votings_d$selectable_topic_of_voting, pattern="^ *", replacement = ""))
  votings_f <- votings_e
  votings_f$selectable_topic_of_voting <- (gsub(votings_e$selectable_topic_of_voting, pattern=" *[-,\\(].*$", replacement = ""))
  votings_g <- votings_f
  names(votings_g$selectable_topic_of_voting) <- paste("ustawa o", votings_g$selectable_topic_of_voting)
  
  # we have now reduced the values in votings_g$selectable_topic_of_voting to represent
  # different legislations that we should be able to filter on
  # replace the occurances of the original topic_votings with the selectable topic voting
  look <- data.frame(match=votings_g$topic_voting, replacewith=votings_g$selectable_topic_of_voting)
  index <- match(all_votes$topic_voting, look$match)
  all_votes$selectable_topic_of_voting <- look[index, ]$replacewith
  # use as topic_voting, putting the longer original topic_voting as description_voting instead
  all_votes$description_voting <- all_votes$topic_voting
  all_votes$topic_voting <- as.character(all_votes$selectable_topic_of_voting)
}

getSelectableVotingTopics <<- function(all_votes) {
  selectableVotingTopics <- names(which(table(votings_g$selectable_topic_of_voting) > 5))
  names(selectableVotingTopics) <- paste("ustawa o", selectableVotingTopics)
  selectableVotingTopics
}

# optionally filter votes on specific topics (an empty pattern = no filter)
pattern <<- "o ochronie zwierząt" # about animal protection
pattern <<- "szkolnict" # school
pattern <<- ""
