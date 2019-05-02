library(tidyverse)

library(jsonlite)

tba_auth_key <- fromJSON(read_file("authkey.json"))

baseURL <- "https://www.thebluealliance.com/api/v3"

getTBAData <- function(url) {
  req <- httr::GET(url, httr::add_headers("X-TBA-Auth-Key" = tba_auth_key$tba_auth_key))
  json <- httr::content(req, as = "text")
  data <- fromJSON(json, flatten=TRUE) 
  return(data)
}

getEvents <- function(year) {
  url <- paste0(baseURL,
                "/events/",
                year)
  return(getTBAData(url))
}

getEventMatches <- function(year, event_code) {
  url <- paste0(baseURL,
                "/event/",
                year,
                event_code,
                "/matches")
  return(getTBAData(url))
}

getTeamsPage <- function(page) {
  url <- paste0(baseURL,
                "/teams/",
                page)
  return(getTBAData(url))
}

getAllTeams <- function() {
  page = 0
  teams = tibble()
  while(TRUE) {
    new_teams <- getTeamsPage(page)
    if(length(new_teams) == 0) {
      break
    }
    teams <- teams %>% bind_rows(new_teams)
    page <- page + 1
  }
  return(teams)
}

getDistricts <- function(year) {
  url <- paste0(baseURL,
                "/districts/",
                year)
  return(getTBAData(url))
}

getEventTeams <- function(eventKey) {
  url <- paste0(baseURL,
                 "/event/",
                 eventKey,
                 "/teams")
  return(getTBAData(url))
}

getDistrictTeams <- function(district) {
  url <- paste0(baseURL,
                "/district/",
                district,
                "/teams")
  return(getTBAData(url))
}
  
getAllMatches <- function(district) {
  url <- paste0(baseURL,
                "/district/",
                district,
                "/events")
  events <- getTBAData(url)
  matches <- events$key %>% map(function(eventKey) { 
    url <- paste0(baseURL,
                  "/event/",
                  eventKey,
                  "/matches") 
    getTBAData(url)}) %>% bind_rows %>%
    mutate(blue.robot1 = map_chr(alliances.blue.team_keys, ~toString(.[[1]])),
           blue.robot2 = map_chr(alliances.blue.team_keys, ~.[[2]]),
           blue.robot3 = map_chr(alliances.blue.team_keys, ~.[[3]]),
           red.robot1 = map_chr(alliances.red.team_keys, ~.[[1]]),
           red.robot2 = map_chr(alliances.red.team_keys, ~.[[2]]),
           red.robot3 = map_chr(alliances.red.team_keys, ~.[[3]])) %>%
    select(-alliances.red.team_keys, -alliances.blue.team_keys, -videos,
           -alliances.blue.dq_team_keys, -alliances.red.dq_team_keys,
           -alliances.red.surrogate_team_keys, -alliances.blue.surrogate_team_keys)
}

getAllEventMatches <- function(eventKey) {
  events <- c(eventKey)
  url <- paste0(baseURL,
                "/event/",
                eventKey,
                "/matches") 
  events <- getTBAData(url) %>%
    mutate(blue.robot1 = map_chr(alliances.blue.team_keys, ~toString(.[[1]])),
           blue.robot2 = map_chr(alliances.blue.team_keys, ~.[[2]]),
           blue.robot3 = map_chr(alliances.blue.team_keys, ~.[[3]]),
           red.robot1 = map_chr(alliances.red.team_keys, ~.[[1]]),
           red.robot2 = map_chr(alliances.red.team_keys, ~.[[2]]),
           red.robot3 = map_chr(alliances.red.team_keys, ~.[[3]])) %>%
    select(-alliances.red.team_keys, -alliances.blue.team_keys, -videos,
           -alliances.blue.dq_team_keys, -alliances.red.dq_team_keys,
           -alliances.red.surrogate_team_keys, -alliances.blue.surrogate_team_keys)
}

getDistrictEvents <- function(district) {
  url <- paste0(baseURL,
                "/district/",
                district,
                "/events")
  return(getTBAData(url))
}
