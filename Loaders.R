if (! "seedCost" %in% ls()) {
  source("Tourny.R")
}

# hard-coded version
bracketFile <- "data/bracket2017.csv"
testBracketFile <- "data/bracket2016.csv"
entryPattern <- "Entry-.*rds"

# adaptable version
year <- 2022
bracketFile <- paste0("data/bracket", year, ".csv")
testBracketFile <- paste0("data/bracket", year - 1, ".csv")
entryPattern <- paste0("Entry-",year,".*rds")

# Player entries

update_teams <- function(teams, canonical_teams) {
  team_split <-
    cbind(
      teams,
      stringr::str_split_fixed(teams, "/", n = 2)
    )
  ifelse(
    team_split[,3] %in% canonical_teams,
    team_split[,3],
    ifelse(
      team_split[,2] %in% canonical_teams,
      team_split[,2],
      team_split[,1])
  )
}

LoadEntries <-
  function(
    path = "data/Entries/", year = 2022)
  {
    # rdsFile <- paste0(path, "Entries.rds")
    # if (file.exists(rdsFile)) {
    #   return(readRDS(rdsFile))
    # }

    entryPattern <- paste0("Entry-",year,".*rds")
    efiles <- dir(path, pattern = entryPattern, full.names = TRUE)
    res <- list()
    # read files in order; newer entries with same email will clobber older ones
    for (f in sort(efiles)) {
      e <- readRDS(f)

      # update based on the play-in game winners
        e$teams <- update_teams(e$teams, Bracket$team) # gsub(names(playin)[i], playin[i], e$teams)
        names(e$teamsLogical) <- update_teams(names(e$teamsLogical), Bracket$team) # gsub(names(playin[i]), playin[i], names(e$teamsLogical))

      # ensure the order is the same for all the logical vectors and matches the bracket order
      e$teamsLogical <- e$teamsLogical[Bracket$team]

      res[[e$email]] <- e
    }
    res
  }

LoadGameScores <- function(path = "data/Scores/2022/Mens/", pattern = "M-.*2022.*\\.csv") {
  gfiles <- dir(path, pattern = pattern, full.names = TRUE)
  if (length(gfiles) < 1)
    return(tibble(game_number = NA, winner_01 = NA, home = NA, away = NA, hscore = NA, ascore = NA) %>% head(0))

  res <- list()
  # read files in order; newer entries with same email will clobber older ones
  for (f in sort(gfiles)) {
    g <- read.csv(f, stringsAsFactors = FALSE)
    res[[paste0(g$home, "-", g$away)]] <- g
  }
  bind_rows(res) %>% addWL()
}

### Read in bracket data
LoadBracket <- function(file = NULL) {
  if (! is.null(file) ) {
    bracket <- read.csv(file, as.is = TRUE)
  } else {
    bracket <-
      if (file.exists(bracketFile)) {
        read.csv(bracketFile, as.is=TRUE)
      } else {
        read.csv(testBracketFile, as.is=TRUE)
      }
  }

  bracket %>%
    mutate(cost = seedCost[seed],
           cost.old = seedCostOld[seed],
           # which regions play in final 4 is determined by order of appearence in bracket.csv
           slot = ( as.numeric(factor(region, levels = unique(region), # c('east','west','south','midwest'),
                                      ordered=TRUE)) * 100 + order(seedOrder)[seed] )
    ) %>%
    arrange(slot)
}

completedGames <- function(scores, bracket) {
  if (nrow(scores) < 1) return(scores)

  scores %>%
    scheduledGames(bracket) %>%
    filter(!is.na(hscore) & !is.na(ascore)) %>%
    mutate(wscore = ifelse(winner == home, hscore, ascore),
           lscore = ifelse(winner == home, ascore, hscore),
           score = paste(wscore, "-", lscore),
           result = paste(winner, wscore, "over", loser, lscore)
    ) %>%
    arrange(game)
}

