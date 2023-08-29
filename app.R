library(httr)
library(shiny)

ui <- fluidPage(
  selectInput("consortium", label = "Consortium",
              choices = c( "Australian Research Data Commons" = "ardc",
                          "National Library of New Zealand" = "doinzco")),
  selectInput("legacy", "Legacy bucket", choices = c("Select a consortium" = "none")),
  verbatimTextOutput("summary")
)
server <- function(input, output, session) {
  # Create a reactive expression

  observeEvent(input$consortium, {
    updateSelectInput(inputId = "legacy", choices = providerIds())
  })
  
  providers <- reactive({
    url <- "https://api.test.datacite.org/providers"
    queryString <- list("consortium-id" = input$consortium)
    response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
    content(response, "parsed")
  })
  
  providerIds <- reactive({
    sort(sapply(providers()[[1]], function(x) x[["id"]]))
  })
  
  output$summary <- renderPrint({
    # Use a reactive expression by calling it like a function
    if (input$consortium != "none")  {
      paste(length(providerIds()))
    }
  })
  
}
shinyApp(ui, server)