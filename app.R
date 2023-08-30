library(httr)
library(purrr)
library(rlist)
library(shiny)
#library(tidyjson)

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
  
  allClientIds <- reactive({
    providers() %>% map(~ .$relationships$clients$data) %>% map_depth(2, ~ .$id) %>% unlist %>% sort
  })
  
  clientIds <- reactive({
    providers() %>% list.filter(id==input$legacy) %>% map(~ .$relationships$clients$data) %>% map_depth(2, ~ .$id) %>% unlist %>% sort
  })
  
  allProviders <- reactive({
    providers() %>% map(~ .[["id"]])
  })
  
  providers <- reactive({
    url <- "https://api.datacite.org/providers"
    queryString <- list("consortium-id" = input$consortium)
    response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
    content(response, "parsed")[[1]]
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
    if (input$legacy != "none")  {
      
    }
  })
  
}
shinyApp(ui, server)