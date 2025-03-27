library(tidyverse)
seedOrder   <- c(1,16, 8,9, 5,12, 4,13, 6,11, 3,14, 7,10, 2,15)
seedCost    <- c(42,30,23,20,14,14,12,9, 7,8,8,6,3,2,1,1)
seedCostOld <- c(22,18,15,12,10, 9, 7, 6,5,4,3,2,1,1,1,1)
seedCost    <- c(48,35,29,23, 17,15,13,11, 10,9,8,7, 4,3,1,1)



nRounds <- function(bracket)  ceiling(log2(nrow(bracket)))

nGames <- function(bracket) 2^nRounds(bracket) - 1

homeTeam_ <- function(id, bracket, results) {
  if (id > nGames(bracket)) return(NA)
  if (id > nGames(bracket) / 2) {
    return( bracket[2 * id - nGames(bracket), "team"] )
  }
  winner(id * 2, bracket, results)
}

awayTeam_ <- function(id, bracket, results) {
  if (id > nGames(bracket)) return(NA)
  if (id > nGames(bracket) / 2) {
    return( bracket[2 * id + 1 - nGames(bracket), "team"] )
  }

  winner(id * 2 + 1, bracket, results)
}

winner_ <- function(id, bracket, results) {
  if (id > nGames(bracket)) {
    return(bracket[id - nGames(bracket), "team"])
  }
  Score <- results %>%
    filter(home == homeTeam(id, bracket, results),
           away == awayTeam(id, bracket, results))
  if (nrow(Score) != 1) {
    return(NA)
  }
  if (Score$hscore > Score$ascore) return (Score$home)
  return(Score$away)
}

gameScore_ <- function(id, bracket, results) {
  if (id > nGames(bracket)) { return(NA) }

  Score <- results %>%
    filter(home == homeTeam(id, bracket, results),
           away == awayTeam(id, bracket, results))
  if (nrow(Score) != 1) { return(" - ") }
  paste(Score$ascore, "-", Score$hscore, sep="")
}

homeScore_ <- function(id, bracket, results) {
    if (id > nGames(bracket)) { return(NA) }

    Score <- results %>%
    filter(home == homeTeam(id, bracket, results),
           away == awayTeam(id, bracket, results))
    if (nrow(Score) != 1) {return(NA)}
    Score$hscore
  }

awayScore_ <- function(id, bracket, results) {
    if (id > nGames(bracket)) { return(NA) }

    Score <- results %>%
    filter(home == homeTeam(id, bracket, results),
           away == awayTeam(id, bracket, results))
    if (nrow(Score) != 1) {return(NA)}
    Score$ascore
  }


loser_ <- function(id, bracket, results) {
  Score <- results %>%
    filter(home == homeTeam(id, bracket, results),
           away == awayTeam(id, bracket, results))
  if (nrow(Score) != 1) {
    return(NA)
  }
  if (Score$hscore > Score$ascore) return (Score$away)
  return(Score$home)
}
winner <- Vectorize(winner_, vectorize.args="id")
loser <- Vectorize(loser_, vectorize.args="id")
gameScore <- Vectorize(gameScore_, vectorize.args="id")
homeTeam <- Vectorize(homeTeam_, vectorize.args="id")
awayTeam <- Vectorize(awayTeam_, vectorize.args="id")
homeScore <- Vectorize(homeScore_, vectorize.args="id")
awayScore <- Vectorize(awayScore_, vectorize.args="id")

allGames <- function(bracket, results, TBA=FALSE, incomplete.only = FALSE) {
  names <-
    gsub( "NA", "TBA",
          paste(
            awayTeam(1:nGames(bracket), bracket, results),
            " vs. ",
            homeTeam(1:nGames(bracket), bracket, results),
            " (",
            gameScore(1:nGames(bracket), bracket, results),
            ")",
            sep="")
    )
  res <- as.list(1:nGames(bracket))
  names(res) <- names
  res[
    (TBA | !grepl("TBA", names)) &
    (!incomplete.only |
     is.na(winner(1:nGames(bracket), bracket, results)))
    ]
}

scheduledGames <- function(results, bracket) {
  merge(
    results,
    data.frame(
      home = homeTeam(1:nGames(bracket), bracket=bracket, results=results),
      away = awayTeam(1:nGames(bracket), bracket=bracket, results=results),
      game = 1:nGames(bracket)
      ),
    all.x = FALSE, all.y=FALSE)
}

gameIndex <- function( round, game, num_teams=64) {
  teams_remaining <- num_teams / 2^(round-1)
  teams_out <- num_teams - teams_remaining
  games_played <- teams_out
  num_teams - (games_played + game)
}

possibleMatchups <- function( B ) {
  B$team <- as.character(B$team)
  size <- nrow(B)
  total_rounds <- log2(size)
  res <- list()

  for (r in 1:total_rounds) {
    for (g in 1:(size/(2^r))) {
      # print(c(r,g,gameIndex(r, g)) )
      home <- if (r>1) res[[gameIndex(r-1, 2*g-1)]]$teams else B$team[2*g-1]
      away <- if (r>1) res[[gameIndex(r-1, 2*g)]]$teams else B$team[2*g]
      res[[gameIndex(r,g)]] <- list(round = r, game=g, index = gameIndex(r,g),
                                    home = home, away=away,  teams = c( home, away)
      )
    }
  }
  res
}

# Given a set of teams, produce a list.  Each element of the list represents
# a tournament game and contains a vectors of teams that could win that game
# (they must be alive, be among the teams provided, and have a chance to play in
# the specified game or have already won the game)


chancesToWin <- function( teams, bracket, games, matchups = possibleMatchups(bracket)) {
  bracket <- addTeamStatus(bracket, games)
  lapply( matchups, function(x) {
    teams_with_possible_win <- subset(bracket, alive | (wins >= x$round))$team
    intersect( intersect(x$teams, teams_with_possible_win), teams )
  } )
}

chancesToLose <- function( teams, bracket, games, matchups = possibleMatchups(bracket)) {
  bracket <- addTeamStatus(bracket, games)
  lapply( matchups, function(x) {
    teams_with_possible_win <- subset(bracket, alive | (wins >= x$round))$team
    setdiff( intersect(x$teams, teams_with_possible_win), teams )
  } )
}


mightWin <- function(teams, bracket, games, matchups = possibleMatchups(bracket)) {
  sapply(chancesToWin(teams, bracket, games, matchups), function(x) as.numeric(length(x) > 0))
}

goingToWin <- function(teams, bracket, games, matchups = possibleMatchups(bracket)) {
  1 - sapply(chancesToLose(teams, bracket, games, matchups), function(x) as.numeric(length(x) > 0))
}

maxPossible <- function( teams, bracket, games, matchups = possibleMatchups(bracket)) {
  sum( mightWin(teams, bracket, games, matchups) )
}


minGuaranteed <- function( teams, bracket, games, matchups = possibleMatchups(bracket)) {
  sum( goingToWin(teams, bracket, games, matchups) )
}


# head2head <- function(teams1, teams2, bracket, games, matchups = possibleMatchups(bracket)) {
#   c(
#     sum( mightWin(setdiff(teams1, teams2), bracket, games, matchups) ) -
#       sum( goingToWin(setdiff(teams2, teams1), bracket, games, matchups) )
#   )
# }
#


guaranteedWins <- function( teams, bracket, games, matchups = possibleMatchups(bracket)) {
  bracket <- addTeamStatus(bracket, games)
  guaranteed_win <- sapply( matchups, function(x) {
    teams_with_possible_win_this_round <- subset(bracket, alive | (wins >= x$round))$team
    possible_winners <- intersect( x$teams, teams_with_possible_win_this_round)   # in game but have not lost earlier
    length( intersect( possible_winners, teams) ) == length(possible_winners)
  } )
  sum( guaranteed_win )
}

winsByRound <- function( teams, bracket, games, as.character = TRUE ) {
  # games <- addWL(games)
  bracket <- addTeamStatus(bracket, games)
  wins <- filter(bracket, team %in% teams)$wins
  rounds <- max(bracket$wins)
  cwins <- colSums( outer(wins, 1:rounds, function(x,y) x >= y) )
  if (! as.character ) return(cwins)
  # cwins <- rev( cumsum( rev(table(wins[wins>0])) ) )
  # paste0(sprintf("%02d",sum(wins)), " = ", paste(cwins, collapse = " + ") )
  paste(cwins, collapse = " + ")
}

addTeamStatus <- function(bracket, games) {
  if (all(c("alive", "wins", "cost") %in% names(bracket))) return(bracket)
  if (! all(c("winner", "loser") %in% names(games)) ) games <- addWL(games)
  bracket$alive <- ! sapply(as.character(bracket$team), function(t) t %in% as.character(games$loser))
  bracket$wins <- sapply(as.character(bracket$team), function(t) sum( as.character(games$winner) == t, na.rm=TRUE ) )
  bracket$cost <- seedCost[bracket$seed]
  # bracket$cost.old <- seedCostOld[bracket$seed]
  bracket
}

addWL <- function(games) {
  if (all(c("winner", "loser") %in% names(games))) return(games)
  if (! all(c("home", "away", "hscore", "ascore") %in% names(games)) )
    stop("The games data frame should have home, away, hscore, and ascore columns, but it does not.")

  mutate(
    games,
    winner = ifelse( hscore > ascore, as.character(home), as.character(away)),
    loser = ifelse( hscore < ascore, as.character(home), as.character(away))
  )
}

resultsTable <- function(entries, bracket, games, matchups = possibleMatchups(bracket)) {
  E <- entries
  Bracket <- addTeamStatus(bracket, games)
  MatchUps <- matchups

  Results <- data.frame(
    check.names=FALSE,
    email = sapply( E, function(x) x$email ),
    name  = sapply( E, function(x) x$name ),
    dept  = sapply( E, function(x) x$dept),
    score = sapply( E, function(x) sum( x$teamsLogical * Bracket$wins ) ),
    `score details` =  sapply( E, function(x) winsByRound(x$teams, Bracket)),
    "guaranteed wins" = sapply(E,
                               function(x)
                                 guaranteedWins( x$teams, bracket = Bracket, matchups = MatchUps ) ),
    "max possible" =
      sapply(E,
             function(x)
               maxPossible( x$teams, bracket = Bracket, games = games, matchups = MatchUps )
             ),
    "teams remaining" =
      sapply( E,
              function(x) {
                BracketLeft <- Bracket |> ungroup() %>% filter(x$teamslogical & alive) %>% arrange(seed)
                numberLeft <- sum( x$teamslogical * Bracket$alive )
                paste(
                  sprintf("%02d", numberLeft), ": ",
                  paste(BracketLeft$team, " (", BracketLeft$seed, ")",
                        sep="",
                        collapse=", "),
                  collapse="")
              }
      ),
    "points remaining" =
      sapply( E,
              function(x) {
                pointsLeft <- sum(x$teamslogical * Bracket$cost * Bracket$alive)
                pointsLeft
              }
      ),
    "teams lost" =
      sapply( E,
              function(x) {
                BracketLost <- Bracket |> ungroup() %>% filter(x$teamslogical & !alive) %>% arrange(seed)
                numberLost <- sum( x$teamslogical * (!Bracket$alive) )
                paste(
                  sprintf("%02d", numberLost), ": ",
                  paste(BracketLost$team, " (", BracketLost$seed, ")",
                        sep="",
                        collapse=", "),
                  collapse="")
              }
      )
  )
  Results <- Results %>% arrange(desc(score))
  rownames(Results) <- Results$email
  Results %>% select(-email)
}

if (FALSE) {
  Bracket <- read.file("data/bracket2014.csv", as.is=TRUE)
  # Below orders things properly IF regions are in correct order
  # Else need to reorder regions first
  # Bracket %>% sample() %>% arrange(region, order(seedOrder)[seed])
  Results <- read.file("data/Results.csv", as.is=TRUE)

  nRounds(Bracket)
  homeTeam( 63, Bracket, Results)
  awayTeam( 63, Bracket, Results)
  winner( 63, Bracket, Results)
  homeTeam( 31, Bracket, Results)
  awayTeam( 31, Bracket, Results)
  winner( 31, Bracket, Results)
  Results %>% tail(4)
  allGames( Bracket, Results %>% head(30), TBA = FALSE)
}

updateGameScores <- function(GS, home, away, hscore, ascore) {
  plyr::rbind.fill(
    GS[!(GS$home == home & GS$away == away), ],   # remove previous score if it exists
    addWL(dplyr::tibble(home=home, away=away, hscore=hscore, ascore=ascore))
  )
}

teamData <- function(E = LoadEntries(), B = LoadBracket() %>% addTeamStatus(LoadGameScores())) {
  M <- do.call(rbind, lapply( E, function(x) x$teamsLogical ) )
  if (length(E) > 0) {
    row.names(M) <- sapply( E, function(x) x$name)
  }

  data.frame(check.names=FALSE,
             Team = colnames(M),
             Seed = B$seed, Region = B$region,
             "Number of players selecting" = apply(M,2,sum),
             Wins = B$wins)
}
