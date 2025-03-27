
# Run this once when Sweet16 is set and before any other games
# This appears to be superceded by WhoCanWin-setup.R

source('server.R')
source('loaders.R')

Entries <- load_entries_from_pins(board = board, year = config$year)

saveStandings <- function() {
  Sweet16Standings <-
      resultsTable(entries = Entries, bracket = LoadBracket(),
                   games = LoadGameScores(),
                   matchups = possibleMatchups(Bracket))
  saveRDS(Sweet16Standings, file = "data/Sweet16Standings.rds")
}

# Run this once when Sweet16 is set and before any other games

saveWinsMatrix <- function() {
  WM <- winsMatrix(rounds = 4)
  rownames(WM) <- aliveTeams()
  saveRDS(WM, file = "data/Sweet16winMatrix.rds")
}

# B = total number of bits

bitsOf <- function(x, B = 16) {
  bitops::bitShiftR(x, 0:(B-1)) %% 2
}

gamesPlayedIn <- function(team, rounds) {
  bitops::bitShiftR(team + 2^rounds - 1, 1:rounds)
}

winningOutcomes <- function(team, rounds) {
  bitsOf(team + 2^rounds - 1)[1:rounds]
}

winsObtained <-
  Vectorize(
    function(team, rounds, results) {
      sum( cumprod( winningOutcomes(team, rounds) == bitsOf(results)[gamesPlayedIn(team, rounds)] ) )
    }
  )

# rows are teams; cols are full tournaments; entries are # of wins by team in that tournament run
winsMatrix <- function(rounds) {
  sapply(0:(2^(2^rounds-1) - 1), function(x) winsObtained(1:(2^rounds), rounds, x) )
}

aliveTeams <- function(bracket = LoadBracket() %>% addTeamStatus(LoadGameScores())) {
  bracket[bracket$alive, "team"]
}


# The intent here is to see how many winning combos there are for
# each entrant after the first two rounds have been played.
# Standings contains the standings after two rounds.
# results is a vector of wins for the teams in M (after the
# first two rounds) and is used to remove scenaries from M that
# are no longer possible.

WhoCanWin <- function(Entries, M, Standings,
                      results = (teamData() %>% filter(Team %in% rownames(M)))$Wins - 2,
                      break.ties = TRUE) {
  scores <- Standings$score
  names(scores) <- rownames(Standings)
  if (break.ties) {
    M <- M + 1/10^(5-M)
  }
  M <- M[, apply(M, 2, function(x) all(x >= results)), drop = FALSE]
  L <- lapply(
    Entries,
    function(e) colSums(
      M[intersect(e$teams, rownames(M)), , drop = FALSE], na.rm = TRUE) +
      scores[e$email]
  )
  Outcomes <- do.call(rbind, L)
  # rownames(Outcomes) <- rownames(Standings)
  WinningScores <-
    Outcomes %>% apply(2, base::max, na.rm = TRUE)
  LosingScores <-
    Outcomes %>% apply(2, base::min, na.rm = TRUE)

  res <- tibble(
    name = Standings$name,
    email = row.names(Standings)
  )
  res[["winning scenarios"]] <-
    sapply(res$email, function(em)
      sum( WinningScores == Outcomes[em, ], na.rm = TRUE ))
  res[["losing scenarios"]] <-
    sapply(res$email, function(em)
      sum( LosingScores == Outcomes[em, ], na.rm = TRUE ))
  res %>% mutate(
    `win percent` = round(100 * `winning scenarios` / dim(M)[2], 2),
    `lose percent` = round(100 * `losing scenarios` / dim(M)[2], 2),
  )
}

WhoCanWinOld <- function(Entries, M, Standings, results = (teamData() %>% filter(Team %in% rownames(M)))$Wins - 2,
                      break.ties = TRUE) {
  scores <- Standings$score
  names(scores) <- row.names(Standings)
  if (break.ties) {
    M <- M + 1/10^(5-M)
  }
  M <- M[, apply(M, 2, function(x) all(x >= results)), drop = FALSE]
  L <- lapply(
    Entries,
    function(e) colSums(
      M[intersect(e$teams, rownames(M)), , drop = FALSE], na.rm = TRUE) +
      scores[e$email]
  )
  Outcomes <- do.call(rbind, L)
  rownames(Outcomes) <- rownames(Standings)
  WinningScores <-
    Outcomes %>% apply(2, base::max, na.rm = TRUE)
  LosingScores <-
    Outcomes %>% apply(2, base::min, na.rm = TRUE)

  res <- tibble(
    name = Standings$name,
    email = row.names(Standings)
  )
  res[["winning scenarios"]] <-
    sapply(res$email, function(em)
      sum( WinningScores == Outcomes[em, ], na.rm = TRUE ))
  res[["losing scenarios"]] <-
    sapply(res$email, function(em)
      sum( LosingScores == Outcomes[em, ], na.rm = TRUE ))
  res %>% mutate(
    `win percent` = round(100 * `winning scenarios` / dim(M)[2], 2),
    `lose percent` = round(100 * `losing scenarios` / dim(M)[2], 2),
    )
}

