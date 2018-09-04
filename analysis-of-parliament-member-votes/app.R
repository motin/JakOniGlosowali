if (!require(shiny)) install.packages("shiny", dependencies = TRUE);
if (!require(shiny.i18n)) {
  if (!require(devtools)) install.packages("devtools", dependencies = TRUE);
  devtools::install_github("Appsilon/shiny.i18n", dependencies = TRUE);
}

source("tools.R")

translator <- Translator$new(translation_csvs_path = "translations")

# https://github.com/daattali/advanced-shiny/tree/master/reactive-dedupe
dedupe <- function(r) {
  makeReactiveBinding("val")
  observe(val <<- r(), priority = 10)
  reactive(val)
}

ui <- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  
  includeCSS("style.css"),
  
  uiOutput("page_content")
)

server <- function(input, output) {
  
  # Use to restore hard-coded form data
  restored <- data.frame(id_voting=as.character(c(
  )), vote = as.character(c(
  )))
  
  updateCountrySpecificData <- reactive({
    # country to work with
    country <<- input$selected_country
    message("updateCountrySpecificData - country: ")
    cat(str(country))
    
    if (length(country) == 0 || ! country %in% c("se", "pl")) {
      message("No valid country selected yet. Defaulting to sweden")
      country <<- "se"
    }
    
    loadCountrySpecificData()
    req(all_votes)
    
    
    selectableVotingChoices <- head(getSelectableVotingChoices(all_votes),1000) # max 1000 choices to prevent UI generation from freezing
    sortedSelectableVotingChoices <- selectableVotingChoices[order(names(selectableVotingChoices), decreasing = FALSE)]
    
    return (list(all_votes=all_votes, sortedSelectableVotingChoices=sortedSelectableVotingChoices))
  })
  
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    } else {
      message("No valid language selected yet. Defaulting to en")
      selected <- "en"
    }
    translator$set_translation_language(selected)
    translator
  })
  
  output$page_content <- renderUI({
    countrySpecificData <- updateCountrySpecificData()
    sortedSelectableVotingChoices <- countrySpecificData$sortedSelectableVotingChoices
    all_votes <- countrySpecificData$all_votes
    ui <- tagList(
      fluidRow(column(9, h2(i18n()$t("intro-html")), p(i18n()$t("general-instructions-html")), h4(i18n()$t("user-vote-instructions-title")), p(i18n()$t("user-vote-instructions-html"))),
               column(3, selectInput("selected_language",
                                     i18n()$t("change-language"),
                                     choices = translator$languages[! translator$languages %in% c("refs")],
                                     selected = translator$translation_language),
                      selectInput("selected_country",
                                  i18n()$t("change-country"),
                                  choices = c("se", "pl"),
                                  selected = country),
                      selectInput("plotType", 
                                  i18n()$t("plot-type"), 
                                  choices = c("fan", "phylogram", "cladogram", "unrooted", "radial"), 
                                  selected = input$plotType))
      ),
      fluidRow(column(3, br(),p(i18n()$t("statutes-filter-instruction"))),
               column(9, selectInput("voting_ids", "", choices = sortedSelectableVotingChoices, selected = voting_ids(), multiple = TRUE, width = "100%"))
      )
    )
    
    if (country == "se") {
      ui <- tagAppendChild(ui,
                           fluidRow(column(12, HTML(i18n()$t("general-instructions-country-se-html"))))
      )
    }
    
    # Generate ui for personal votes
    selectableVotings <- getSelectableVotings(all_votes)
    # for(voting_id in sortedSelectableVotingChoices){
    for (voting_id in voting_ids()){
      newInputId <- paste("user_vote[",voting_id,"]", sep="")
      newInputValue <- "None"
      # if (voting_id %in% voting_ids()){
      if (newInputId %in% isolate(names(input))) {
        newInputValue <- isolate(input[[newInputId]])
      } else {
        # check if should be restored from previous input
        valueToRestore <- restored$vote[restored$id_voting == voting_id]
        if (length(valueToRestore) > 0) {
          newInputValue <- valueToRestore
        }
      }
      userVoteInput <- radioButtons(newInputId, label = i18n()$t("your-vote"),
                                    choices = c("None",differentKindsOfVotes), 
                                    selected = newInputValue, inline = TRUE, width = "100%")
      voting <- selectableVotings[selectableVotings$id_voting == voting_id, ]
      title <- voting$topic_voting
      date <- voting$date_meeting
      if (!is.na(voting$description_voting) && length(voting$description_voting) > 0) {
        description <- p(voting$description_voting)
      } else {
        description <- ""
      }
      document_id <- voting$voting_related_document_ids
      row <- fluidRow(column(12, HTML(paste("<h4>",title,"</h4>"))))
      foohtml <- paste('<div id="main" class="votering box-stroke">
                       <p><b>Beslutsdatum:</b> ',date,'</p>
                       ',description,'
                       <p><a href="http://www.riksdagen.se/sv/global/sok/?q=',document_id,'&st=1">Sök på förslaget på riksdagen.se &rarr;</a></p>
                       <p><a href="http://data.riksdagen.se/dokument/',document_id,'">Läs förslagsdokumentet i sin helhet &rarr;</a></p>
                       </div>', sep="")
      row2 <- fluidRow(column(12, HTML(paste("",foohtml,""))))
      row3 <- fluidRow(column(12, userVoteInput))
      ui <- tagAppendChildren(ui, row, row2, row3)
      # } else {
      #   userVoteInput <- conditionalPanel("false",radioButtons(newInputId, label = i18n()$t("your-vote"),
      #                                  choices = c("None",differentKindsOfVotes), 
      #                                 selected = newInputValue, inline = TRUE, width = "100%"))
      #   ui <- tagAppendChildren(ui, userVoteInput)
      # }
    }
    
    ui <- tagAppendChildren(ui,
                            conditionalPanel("input.selected_country !== ''",
                                             fluidRow(column(12, plotOutput("speakerDendro", width = 1000, height = 1000)))
                                             # , fluidRow(column(12, plotOutput("votingDirectionPartyOverview", width = 1000, height = 500)))
                            ),
                            fluidRow(column(12, HTML(i18n()$t("footer-html"))))
    )
    ui
  })
  
  plotType <- reactive({
    input$plotType
  })
  
  voting_ids <- reactive({
    result <- input$voting_ids
    
    if (length(result) == 0) {
      message("No valid voting_ids selected yet. Defaulting to restore previous values")
      result <- restored$id_voting
    }
    
    result
    
  })
  
  extractUserVotesFromInput <- dedupe(reactive({
    userVotesVotingIdColumn <- character()
    userVotesColumn <- character()
    for(voting_id in voting_ids()){
      inputId <- paste0("user_vote[", voting_id, "]", sep="")
      vote <- input[[inputId]]
      if (!is.null(vote) && vote != "None") {
        userVotesVotingIdColumn <- append(userVotesVotingIdColumn, voting_id)
        userVotesColumn <- append(userVotesColumn, vote)
      }
    }
    userVotes <- data.frame(id_voting=userVotesVotingIdColumn,vote=userVotesColumn)
  }))
  
  getSpeakerDendro <- function(voting_ids, plotType) {
    message("getSpeakerDendro - voting_ids: ")
    print(voting_ids)
    message("getSpeakerDendro - plotType: ")
    message(plotType)
    
    selectionOfVotes <- getVotesThatMatchesVotingIds(voting_ids)
    userVotes <- extractUserVotesFromInput()
    if (nrow(userVotes) > 0) {
      selectionOfVotes <- addUserVotes(selectionOfVotes, userVotes)
    }
    
    votingData <- crunchVotingData(selectionOfVotes)
    
    plotTitle <- paste(paste(pattern, collapse = "\n"), "(",i18n()$t("votings"),": ",length(unique(selectionOfVotes$id_voting)),")", sep="")
    par(mar=c(1,1,2,1), xpd=NA, font=2, family="mono")
    phyloPlotPlain(votingData$hc, plotType, partyColors[votingData$partyRepresentedByEachVote], plotTitle)
  }
  
  getVotingDirectionPartyOverview <- function(voting_ids) {
    message("getVotingDirectionPartyOverview - voting_ids: ")
    print(voting_ids)
    
    selectionOfVotes <- getVotesThatMatchesVotingIds(voting_ids)
    userVotes <- extractUserVotesFromInput()
    if (nrow(userVotes) > 0) {
      selectionOfVotes <- addUserVotes(selectionOfVotes, userVotes)
    }
    
    par(mar=c(1,1,2,1), xpd=NA)
    plotVotingDirectionPartyOverview(selectionOfVotes)
  }
  
  output$speakerDendro <- renderPlot({
    withProgress(message = i18n()$t("progress-message"),
                 detail = i18n()$t("progress-detail"), value = 0, {
                   countrySpecificData <- updateCountrySpecificData()
                   getSpeakerDendro(voting_ids(), plotType())
                 })
  })
  
  output$votingDirectionPartyOverview <- renderPlot({
    withProgress(message = i18n()$t("progress-message"),
                 detail = i18n()$t("progress-detail"), value = 0, {
                   countrySpecificData <- updateCountrySpecificData()
                   getVotingDirectionPartyOverview(voting_ids())
                 })
  })
  
}

shinyApp(ui = ui, server = server)
