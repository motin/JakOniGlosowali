if (!require(shiny)) install.packages("shiny", dependencies = TRUE);
if (!require(shiny.i18n)) {
  if (!require(devtools)) install.packages("devtools", dependencies = TRUE);
  devtools::install_github("Appsilon/shiny.i18n", dependencies = TRUE);
}

source("tools.R")

translator <- Translator$new(translation_csvs_path = "translations")

ui <- fluidPage(
  tags$head(tags$script("(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                        (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                        m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                        })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
                        ga('create', 'UA-5650686-6', 'auto');
                        ga('send', 'pageview');")),
  
  includeCSS("style.css"),
  
  uiOutput("page_content")
  )

server <- function(input, output) {
  
  updateCountrySpecificData <- reactive({
    # country to work with
    country <<- input$selected_country
    message("Country: ")
    cat(str(country))
    
    if (length(country) == 0 || ! country %in% c("se", "pl")) {
      message("No valid country selected yet. Defaulting to sweden")
      country <<- "se"
    }
    
    loadCountrySpecificData()
    req(all_votes)
    
    selectableVotingTopics <- sort(getSelectableVotingTopics(all_votes))
    
    return (list(all_votes=all_votes, selectableVotingTopics=selectableVotingTopics))
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
    selectableVotingTopics <- countrySpecificData$selectableVotingTopics
    ui <- tagList(
      fluidRow(column(9, HTML(i18n()$t("intro-html"))),
               column(3, selectInput("selected_language",
                                     i18n()$t("change-language"),
                                     choices = translator$languages[! translator$languages %in% c("refs")],
                                     selected = translator$translation_language),
                      selectInput("selected_country",
                                  i18n()$t("change-country"),
                                  choices = c("se", "pl"),
                                  selected = input$selected_country),
                      selectInput("plotType", 
                                  i18n()$t("plot-type"), 
                                  choices = c("fan", "phylogram", "cladogram", "unrooted", "radial"), 
                                  selected = input$plotType))
      ),
      fluidRow(column(3, br(),p(i18n()$t("statutes-filter-instruction"))),
               column(9, selectInput("pattern", "", choices = selectableVotingTopics, selected = pattern(), multiple = TRUE, width = "100%"))
      )
    )
    
    # Generate ui for personal votes    
    for(i in pattern()){
      input <- radioButtons("radio", label = i18n()$t("your-vote"),
                            choices = c("None",differentKindsOfVotes), 
                            selected = "None", inline = TRUE, width = "100%")
      row <- fluidRow(column(12, HTML(paste("<h4>",i,"</h4>"))))
      row2 <- fluidRow(column(12, input))
      ui <- tagAppendChildren(ui, row, row2)
    }
    
    ui <- tagAppendChildren(ui,
                            conditionalPanel("input.selected_country !== ''",
                                             fluidRow(column(12, plotOutput("speakerDendro", width = 1000, height = 1000))),
                                             fluidRow(column(12, plotOutput("votingDirectionPartyOverview", width = 1000, height = 500)))
                            ),
                            fluidRow(column(12, HTML(i18n()$t("footer-html"))))
    )
    ui
  })
  
  plotType <- reactive({
    input$plotType
  })
  
  pattern <- reactive({
    input$pattern
  })
  
  getSpeakerDendro <- function(pattern, plotType) {
    message("getVotingDirectionPartyOverview - pattern: ")
    message(str(pattern))
    message("getVotingDirectionPartyOverview - plotType: ")
    message(plotType)
    
    selectionOfVotes <- getVotesThatMatchesTopicPattern(pattern)
    
    votingData <- crunchVotingData(selectionOfVotes)
    
    plotTitle <- paste(paste(pattern, collapse = "\n"), "(",i18n()$t("votings"),length(unique(selectionOfVotes$id_voting)),")")
    par(mar=c(1,1,2,1), xpd=NA, font=2, family="mono")
    phyloPlotPlain(votingData$hc, plotType, partyColors[votingData$partyRepresentedByEachVote], plotTitle)
  }
  
  getVotingDirectionPartyOverview <- function(pattern) {
    message("getVotingDirectionPartyOverview - pattern: ")
    message(str(pattern))
    
    selectionOfVotes <- getVotesThatMatchesTopicPattern(pattern)
    
    par(mar=c(1,1,2,1), xpd=NA)
    plotVotingDirectionPartyOverview(selectionOfVotes)
  }
  
  output$speakerDendro <- renderPlot({
    withProgress(message = i18n()$t("progress-message"),
                 detail = i18n()$t("progress-detail"), value = 0, {
                   getSpeakerDendro(pattern(), plotType())
                 })
  })
  
  output$votingDirectionPartyOverview <- renderPlot({
    withProgress(message = i18n()$t("progress-message"),
                 detail = i18n()$t("progress-detail"), value = 0, {
                   getVotingDirectionPartyOverview(pattern())
                 })
  })
  
}

shinyApp(ui = ui, server = server)
