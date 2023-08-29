library(httr)
library(shiny)
library(tidyjson)

ui <- fluidPage(
  titlePanel("Shiny DataCite stats"),
  sidebarLayout(
    sidebarPanel (
      selectInput("consortium", label = "Consortium",
                  choices = c("Select a consortium" = "none", 
                              "Australian Research Data Commons" = "ardc",
                              "National Library of New Zealand" = "doinzco")),
      selectInput("legacy", "Legacy bucket", choices = c("Select a consortium..." = "none")),
      numericInput("own", "Own repos in bucket", value = 0, min = 0)
    ),
    mainPanel(
      verbatimTextOutput("memberCount"),
      verbatimTextOutput("providers"),
      verbatimTextOutput("clients"),
      verbatimTextOutput("debug")
    )
  )
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
  
  # Reactive functions
  
  clients <- reactive({
#    url <- "https://api.datacite.org/clients"
#    queryString <- list("provider-id" = input$legacy)
#    response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
#    content(response, "parsed")[[1]]
    sort(sapply(providers()[[input$legacy]], function(x) x[["relationships"]][["clients"]][["data"]][["id"]]))
  })
  
  clientIds <- reactive({
    sort(sapply(clients(), function(x) x[["id"]]))
  })
  
  providers <- reactive({
    url <- "https://api.datacite.org/providers"
    queryString <- list("consortium-id" = input$consortium)
    response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
    content(response, "parsed")[[1]]
  })
  
  providerIds <- reactive({
    providers() %>% spread_all() %>% .$id %>% sort
  })
  
  providerNames <- reactive({
    providers() %>% spread_all() %>% .$attributes.name %>% sort
  })
  
  memberCount <- reactive({
    if ((input$consortium != "none") && (input$legacy == "none")) {
      length(providerNames())
    } else if ((input$consortium != "none") && (input$legacy != "none")) {
      length(providers()) + length(clients()) - input$own
    }
  })
  
  # Set outputs
  
  output$memberCount <- renderPrint({
    memberCount()
  })
  
  
  output$providers <- renderPrint({
    if (input$consortium != "none")  {
      paste((providerNames()))
    }
  })
  
  output$clients <- renderPrint({
    if (input$legacy != "none") {
      paste((clientIds()))
    }
  })
  
  output$debug <- renderPrint({
    providers() %>% spread_all()
  })
  
}
shinyApp(ui, server)