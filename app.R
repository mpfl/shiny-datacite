library(httr)
library(ggplot2)
library(purrr)
library(shiny)
#library(tidyjson)

ui <- fluidPage(
  titlePanel("Shiny DataCite stats"),
  fluidRow(
    column(3,
    selectInput("consortium", label = "Consortium",
                choices = c("Select a consortium" = "none", 
                            "Australian Research Data Commons" = "ardc",
                            "National Library of New Zealand" = "doinzco"))
    ),
    column(3,
      selectInput("legacy", "Legacy bucket", choices = c("Select a consortium..." = "none"))
    ),
    column(3,
      numericInput("own", "Own repos in bucket", value = 0, min = 0),
    ),
    column(3,
      actionButton("showClients", "Show client stats")
    )
  ),
  verbatimTextOutput("memberCount"),
  verbatimTextOutput("providers"),
  verbatimTextOutput("clients"),
  verbatimTextOutput("debug"),
  plotOutput("clientTypePie")
)
server <- function(input, output, session) {

  # Observers

  observeEvent(input$consortium, {
    if (input$consortium == "none") {
      updateSelectInput(inputId = "legacy", choices = c("Select a consortium first" = "none"))
    } else {
      updateSelectInput(inputId = "legacy", choices = append("none", providerIds()))
    }
  })
  
  # Normal functions
  
  getClientData <- function(provider) {
    url <- "https://api.datacite.org/clients"
    queryString <- list("provider-id" = provider, "page[size]" = 1000)
    response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
    content(response, "parsed")[[1]]
  }
  
  # Reactive functions
  
  allClientIds <- reactive({
    providers() %>% map(~ .$relationships$clients$data) %>% map_depth(2, ~ .$id) %>% unlist %>% sort
  })
  
  allClientData <- reactive({
    id <- showNotification("Retrieving from DataCite...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    providerIds() %>% map(~ getClientData(.))
  }) %>%
  bindEvent(input$showClients)
  
  clientIds <- reactive({
    providers() %>% list.filter(id==input$legacy) %>% map(~ .$relationships$clients$data) %>% map_depth(2, ~ .$id) %>% unlist %>% sort
  })

  allProviders <- reactive({
    providers() %>% map(~ .[["id"]])
  })
  
  providers <- reactive({
    url <- "https://api.datacite.org/providers"
    queryString <- list("consortium-id" = input$consortium, "page[size]" = 1000)
    response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
    content(response, "parsed")[[1]]
  })
  
  clientData <- reactive({
    providerIds
  })
  
  providerIds <- reactive({
    providers() %>% map(~ .$id)
  })
  
  providerNames <- reactive({
    providers() %>% map(~ .$attributes$name)
  })
  
  memberCount <- reactive({
    if ((input$consortium == "none")) {
      0
    } else if ((input$consortium != "none") && (input$legacy == "none")) {
      length(providerNames())
    } else if ((input$consortium != "none") && (input$legacy != "none")) {
      length(providers()) + length(clientIds()) - input$own
    }
  })
  
  # Set outputs
  
  output$memberCount <- renderText({
    memberCount()
  })
  
  output$providers <- renderPrint({
    if (input$consortium != "none")  {
      paste((providerNames()))
    }
  })
  
  output$clients <- renderPrint({
    if (input$consortium != "none") {
      allClientData() %>% map_depth(2, ~ .$attributes$clientType) %>% unlist %>% table %>% as.data.frame
    }
  })
  
  output$clientTypePie <- renderPlot({
    if (input$consortium != "none") {
      allClientData() %>% map_depth(2, ~ .$attributes$clientType) %>% unlist %>% table %>% as.data.frame %>% ggplot(aes(x="", y=Freq, fill=.)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0)
    }
  })
  
  #output$debug <- renderPrint({})
  
}
shinyApp(ui, server)