library(tidyverse)
library(xml2)
library(rvest)
library(forecast)

team <- c(
  "ARI",
  "ATL",
  "BAL",
  "BOS",
  "CHC",
  "CHW",
  "CIN",
  "CLE",
  "COL",
  "DET",
  "HOU",
  "KCR",
  "LAA",
  "LAD",
  "MIA",
  "MIL",
  "MIN",
  "NYM",
  "NYY",
  "OAK",
  "PHI",
  "PIT",
  "SDP",
  "SEA",
  "SFG",
  "STL",
  "TBR",
  "TEX",
  "TOR",
  "WSN"
)

teamLeague <- c(
  "ARI",
  "ATL",
  "BAL",
  "BOS",
  "CHN",
  "CHA",
  "CIN",
  "CLE",
  "COL",
  "DET",
  "HOU",
  "KCA",
  "ANA",
  "LAN",
  "MIA",
  "MIL",
  "MIN",
  "NYN",
  "NYA",
  "OAK",
  "PHI",
  "PIT",
  "SDN",
  "SEA",
  "SFN",
  "SLN",
  "TBA",
  "TEX",
  "TOR",
  "WAS"
)

lookup <- data.frame(team, teamLeague)

downloadSummary <- function(team,
                            start_yr = 2019,
                            end_yr = start_yr,
                            only_completed = TRUE) {
  output = data.frame()
  
  for (yr in start_yr:end_yr) {
    webpage_url <-
      str_glue(
        "https://www.baseball-reference.com/teams/{team}/{yr}-schedule-scores.shtml#team_schedule"
      )
    webpage <- xml2::read_html(webpage_url)
    
    temp <- html_table(webpage)[[1]][-3] %>%
      rename("Location" = 4) %>%
      mutate(
        Location = case_when(Location == "@" ~ "Away",
                             TRUE ~ "Home"),
        HomeTeam = case_when(Location == "Home" ~ Tm,
                             TRUE ~ Opp)
      )
    
    if (only_completed == TRUE) {
      temp <- temp %>%
        filter(Inn != "Game Preview, and Matchups")
    }
    
    output <- temp %>%
      mutate(year = yr) %>%
      filter(Tm != "Tm") %>%
      bind_rows(output)
    print(str_glue("finished year {yr}"))
    if (yr != end_yr) {
      Sys.sleep(3)
    } #Crawl delay per robots.txt
  }
  
  return(output)
}


downloadBox <- function (schedule) {
  print("Status will update every 20 games")
  box <- data.frame()
  
  schedule <- schedule %>%
    left_join(lookup, by = c("HomeTeam" = "team")) %>%
    separate(Date, into = c(NA, "dm"), sep = ",") 
  
  schedule$doubleHeader <- str_extract(schedule$dm, "\\(\\d\\)") %>% 
    str_extract("\\d") %>% 
    replace_na("0")
  
  schedule$dm <- str_remove(schedule$dm, "\\(\\d\\)")
  
  schedule <- schedule %>%
    mutate_at(vars(starts_with("year")), as.character()) %>%
    mutate(Date = mdy(str_c(dm, year, sep = ", ")))
  
  for (i in 1:nrow(schedule)) {
    HA = schedule$Location[i]
    D = str_remove_all(schedule$Date[i], "-")
    T = schedule$teamLeague[i]
    DH = schedule$doubleHeader[i]
    webpage_url <-
      str_glue("https://www.baseball-reference.com/boxes/{T}/{T}{D}{DH}.shtml")
    webpage <- xml2::read_html(webpage_url)
    
    temp <- html_table(webpage)[[1]][-c(3:4), -c(1:2)]
    
    extract_line <- function(r, suf) {
      data <- temp[r, ] %>%
        rename_all(paste0, suf)
    }
    
    if (HA == "Away") {
      tm <- extract_line(1, ".tm")
      opp <- extract_line(2, ".opp")
      combine <- tm %>%
        bind_cols(opp)
    } else {
      tm <- extract_line(2, ".tm")
      opp <- extract_line(1, ".opp")
      combine <- tm %>%
        bind_cols(opp)
    }
    
    box <- box %>%
      bind_rows(combine)
    if (i%%20 == 0) {print(str_glue("finished {i} games"))}
    if (i != nrow(schedule)) {
      Sys.sleep(3)
    } #Crawl delay per robots.txt
  }
  
  schedule <- schedule %>% 
    bind_cols(box)
  
  return(schedule)
}

