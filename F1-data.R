
# Get F1 data: http://ergast.com/mrd/
# Limitations of ergast API:
#  * Max 4 queries per second
#  * Max 200 queries per hour

library(jsonlite)
library(data.table)


# Get race results between 2008 and 2016

get_data_for_round <- function(year, round) {
  raw <- fromJSON(paste("http://ergast.com/api/f1/", year, "/", round, "/results.json", sep = ""))
  df <- raw$MRData$RaceTable$Races$Results[[1]]
  
  df$Year <- year
  df$Round <- round
  
  df$driverId <- df$Driver$driverId
  df$driver_givenName <- df$Driver$givenName
  df$driver_familyName <- df$Driver$familyName
  df$driver_birth <- df$Driver$dateOfBirth
  df$driver_nationality <- df$Driver$nationality
  df$Driver <- NULL
  
  df$constructorId <- df$Constructor$constructorId
  df$constructor_name <- df$Constructor$name
  df$constructor_nationality <- df$Constructor$nationality
  df$Constructor <- NULL
  
  df$time_millis <- df$Time$millis
  df$time_text <- df$Time$time
  df$Time <- NULL
  
  df$fastestlap_rank <- df$FastestLap$rank
  df$fastestlap_time <- df$FastestLap$Time$time
  df$fastestlap_speed <- df$FastestLap$AverageSpeed$speed
  df$FastestLap <- NULL
  
  return(df)
}

rounds_2008_2016 = c(18, 17, 19, 19, 20, 19, 19, 19, 21)

race_list <- list()
race_index <- 1
for (year in 2008:2016) {
  for (round in 1:rounds_2008_2016[year-2007]) {
    message(paste("Getting data for round", round, "in year", year))
    df <- get_data_for_round(year, round)
    race_list[[race_index]] <- df
    race_index <- race_index + 1
    Sys.sleep(0.5)
  }
}

racedata <- rbindlist(race_list, fill = TRUE)
# Get driver's list
racedata$driver_wiki_name <- paste(racedata$driver_givenName, racedata$driver_familyName, sep = "_")
racedata[racedata$driver_wiki_name =="Nelson_Piquet Jr.", ]$driver_wiki_name <- "Nelson_Piquet"
racedata$driver_wiki_name <- gsub(x = racedata$driver_wiki_name,  pattern = " ", replacement = "_")

save(racedata, file = "racedata_raw.rdata")

# Get race schedule between 2008 and 2016

get_schedule <- function(year) {
  raw <- fromJSON(paste("http://ergast.com/api/f1/", year, ".json", sep = ""))
  df <- raw$MRData$RaceTable$Races
  
  colnames(df)[1:2] <- c("Year", "Round")
  
  df$url <- NULL
  df$circuit_id <- df$Circuit$circuitId
  df$circuit_name <- df$Circuit$circuitName
  df$Circuit <- NULL
  
  return(df)
}

schedule_list <- list()
schedule_index <- 1
for (year in 2008:2016) {
  message(paste("Getting schedule for year", year))
  df <- get_schedule(year)
  schedule_list[[schedule_index]] <- df
  schedule_index <- schedule_index + 1
  Sys.sleep(0.5)
}

schedule_data <- rbindlist(schedule_list)
save(schedule_data, file = "scheduledata_raw.rdata")

# Ratings: http://www.f1-fansite.com/f1-results/all-time-f1-driver-rankings/

# https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents/Fernando_Alonso/daily/2015100100/2015103100
# Unfortunately wikipedia API supports only queris starting from 2015
# Legacy daily stats can be acquired from http://stats.grok.se/


unique_drivers <- unique(racedata$driver_wiki_name)
unique_constructors <- unique(racedata$constructor_name)

wiki_list <- list()
wiki_index <- 1
for (driver in unique_drivers) {
  for(year in 2008:2015) { # There is no legacy data after 2015
  message(paste("Driver:", driver, " Year:", year))
    for(month in 1:12) {
      raw <- fromJSON(paste("http://stats.grok.se/json/en/", year, formatC(month, width = 2, flag="0"), "/", driver, sep = ""))
      
      df <- stack(raw$daily_views)
      df$ind <- as.character(df$ind)
      colnames(df) <- c("views", "date")
      df$driver_wiki_name <- driver
      
      wiki_list[[wiki_index]] <- df
      wiki_index <- wiki_index + 1
    }
  }
}
wiki_views <- rbindlist(wiki_list)
save(wiki_views, file = "wikidata_raw.rdata")


# Getting wiki page view for the year 2016
# https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents/Fernando_Alonso/daily/20160101/20160131

wiki_list <- list()
wiki_index <- 1
for (driver in unique_drivers) {
  # jsonlite have som problem using special characters for the new API
  d <- driver
  d <- gsub(pattern = "é", replacement = "e", x = d)
  d <- gsub(pattern = "ä", replacement = "a", x = d)
  d <- gsub(pattern = "ö", replacement = "o", x = d)
  d <- gsub(pattern = "ô", replacement = "o", x = d)
  d <- gsub(pattern = "É", replacement = "E", x = d)
  d <- gsub(pattern = "ü", replacement = "u", x = d)
  
  message(paste("Driver:", driver, " Year: 2016"))
  wurl <- paste("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents/", d, "/daily/20160101/20161231", sep = "")
  raw <- fromJSON(wurl)
  
  df <- raw[[1]]
  df <- df[, c("views", "timestamp")]
  colnames(df) <- c("views", "date")
  
  df$date <- paste( substr(df$date, 1, 4), "-", substr(df$date, 5, 6), "-", substr(df$date, 7, 8), sep = "")
  
  df$driver_wiki_name <- driver
  
  wiki_list[[wiki_index]] <- df
  wiki_index <- wiki_index + 1
  
  Sys.sleep(0.5)
}

wiki_views_2016 <- rbindlist(wiki_list)
save(wiki_views_2016, file = "wikidata_2016_raw.rdata")

# ---=== Additional data cleaning ===---

# Merge wiki view DFs
wiki_views <- rbind(wiki_views, wiki_views_2016)
# Convert to date
wiki_views$date <- as.Date(wiki_views$date, "%Y-%m-%d")
# Some date were invalid like 30th if February, thse were converted to NA
wiki_views <- subset(wiki_views, !is.na(date))
wiki <- wiki_views

# Convert to date
schedule_data$date <- as.Date(schedule_data$date, "%Y-%m-%d")
# Create unique race id
schedule_data$id <- paste(schedule_data$Year, schedule_data$Round, sep = "_")
schedules <- schedule_data

race <- racedata
race$number <- as.numeric(race$number)
race$position <- as.numeric(race$position)
race$points <- as.numeric(race$points)
race$status <- as.factor(race$status)
race$grid <- as.numeric(race$grid)
race$laps <- as.numeric(race$laps)
race$driver_birth <- as.Date(race$driver_birth, "%Y-%m-%d")
race$driver_nationality <- as.factor(race$driver_nationality)
race$constructor_name <- as.factor(race$constructor_name)
race$constructor_nationality <- as.factor(race$constructor_nationality)
race$fastestlap_rank <- as.numeric(race$fastestlap_rank)
race$fastestlap_speed <- as.numeric(race$fastestlap_speed)
regex_pattern <- "(\\d+):(\\d+)\\.(\\d+)"
race$fastestlap_time_seconds <-
  60 * as.numeric(gsub(x = race$fastestlap_time, pattern = regex_pattern, replacement = "\\1")) +
  as.numeric(gsub(x = race$fastestlap_time, pattern = regex_pattern, replacement = "\\2")) +
  0.001 * as.numeric(gsub(x = race$fastestlap_time, pattern = regex_pattern, replacement = "\\3"))
regex_pattern <- "((\\d+):){0,1}(\\d+)\\.(\\d+)"
race$time_after_1st_seconds <- ifelse(startsWith(race$time_text, "+"),
                              60 * as.numeric(gsub(x = race$time_text, pattern = regex_pattern, replacement = "0\\2")) +
                                as.numeric(gsub(x = race$time_text, pattern = regex_pattern, replacement = "0\\3")) +
                                0.001 * as.numeric(gsub(x = race$time_text, pattern = regex_pattern, replacement = "0\\4")), # Follow up
                              0) # winner
race$id <- paste(race$Year, race$Round, sep = "_")

race$time_text <- NULL
race$fastestlap_time <- NULL
race$positionText <- NULL

# ---=== Save dataset ===---
save(race, schedule, wiki, file = "f1.rdata")
