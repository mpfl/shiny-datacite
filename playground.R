library(httr)
library(ggplot2)
library(purrr)

getConsortiumDOIStats <- function(consortium) {
  url <- "https://api.datacite.org/dois"
  queryString <- list("consortium-id" = consortium, "page[size]" = 1)
  response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
  content(response, "parsed")$meta
}

getClientData <- function(provider) {
  url <- "https://api.datacite.org/clients"
  queryString <- list("provider-id" = provider, "page[size]" = 1000)
  response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
  content(response, "parsed")$data
}

getProviders <- function(consortium) {
    url <- "https://api.datacite.org/providers"
    queryString <- list("consortium-id" = consortium, "page[size]" = 1000)
    response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
    content(response, "parsed")
}

providers <- getProviders("ardc")
providerIds <- providers %>% .$data %>% map(~ .$id) %>% unlist
allClientIds <- providers %>% .$data %>% map(~ .$relationships$clients$data) %>% map_depth(2, ~ .$id) %>% unlist

allClientData <- providerIds %>% map(~ getClientData(.))

clientId <- allClientData %>% map_depth(2, ~.$id) %>% unlist
clientTypes <- allClientData %>% map_depth(2, ~ .$attributes$clientType) %>% unlist
dateCreated <- allClientData %>% map_depth(2, ~ .$attributes$created) %>% unlist %>% as.Date
yearCreated <- allClientData %>% map_depth(2, ~ .$attributes$created) %>% unlist %>% map(~ format(as.Date(., format="%Y-%m-%d"),"%Y")) %>% unlist
monthCreated <- allClientData %>% map_depth(2, ~ .$attributes$created) %>% unlist %>% map(~ format(as.Date(., format="%Y-%m-%d"),"%m")) %>% unlist
dayCreated <- allClientData %>% map_depth(2, ~ .$attributes$created) %>% unlist %>% map(~ format(as.Date(., format="%Y-%m-%d"),"%d")) %>% unlist


clientTable <- data.frame(clientId, clientTypes, dateCreated, yearCreated, monthCreated, dayCreated)

doiStats <- getConsortiumDOIStats("ardc")
