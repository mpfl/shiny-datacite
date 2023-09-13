library(httr)
library(ggplot2)
library(purrr)

getConsortiumDOIStats <- function(consortium) {
  url <- "https://api.datacite.org/dois"
  queryString <- list("consortium-id" = consortium, "page[size]" = 1)
  response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
  content(response, "parsed")$meta
}

getConsortiumDOIStatsByYear <- function(consortium, year) {
  url <- "https://api.datacite.org/dois"
  queryString <- list("consortium-id" = consortium, "page[size]" = 1, published = year)
  response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/vnd.api+json"))
  list(year = content(response, "parsed")$meta)
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

doiStats <- getConsortiumDOIStats("ardc") # Get aggregate DOI stats for the consortium

# Chart DOIs minted by year

statsByYear <- getConsortiumDOIStatsByYear(consortium = "ardc", year = 2020)
resourceTypes <- data.frame("type" = statsByYear %>% .$resourceTypes %>% map(~ .$title) %>% unlist, "total" = statsByYear %>% .$resourceTypes %>% map(~ .$count) %>% unlist)
yearlyPublishedStats <- data.frame("year" = doiStats %>% .$published %>% map(~ .$id) %>% unlist, "total" = doiStats %>% .$published %>% map(~ .$count) %>% unlist)

published <- ggplot(yearlyPublishedStats, aes(year, total))
published +
  geom_col(fill = "#00B0D5", colour="black") +
  labs(title = "DOIs published per year") +
  xlab("Year") + ylab("DOIs published") +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ',',
                                  decimal.mark = '.'))

# Collect categories per year

yearlyResourceTypeStats <- yearlyPublishedStats %>% .$year %>% map(~ getConsortiumDOIStatsByYear(consortium = "ardc", year = .))
refinedYearlyResourceTypeStats <- yearlyResourceTypeStats %>% map(~ .$resourceTypes)

# Chart top ten minters of all time

topTenMinters <- data.frame("repo" = doiStats %>% .$clients %>% map(~ .$title) %>% unlist, "count" = doiStats %>% .$clients %>% map(~ .$count) %>% unlist)
minters <- ggplot(topTenMinters, aes(reorder(repo, count, decreasing = TRUE), count))
minters +
  geom_col(fill = "#00B0D5", colour="black") +
  labs(title = "Top ten minters") +
  xlab("Client") + ylab("DOIs published") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ',',
                                  decimal.mark = '.'))


