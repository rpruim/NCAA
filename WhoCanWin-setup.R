
# Run this once when Sweet16 is set and before any other games

# use reactive functions here with isolate().

# Set this true when ready to save as pins

write_pins <- FALSE

source('server.R')

# need to also run come code from inside
# shinyServer(function(input, output, session) { }

source('Tourny.R')

Entries <- load_entries_from_pins(board = board, year = config$year)

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

aliveTeams <- function(
  bracket = LoadBracket() |> addTeamStatus(LoadGameScores())
) {
  bracket[bracket$alive, "team"]
}


# The intent here is to see how many winning combos there are for
# each entrant after the first two rounds have been played.
# Standings contains the standings after two rounds.
# results is a vector of wins for the teams in M (after the
# first two rounds) and is used to remove scenaries from M that
# are no longer possible.

WhoCanWin <- function(
  Entries,
  M,
  Standings,
  results = (teamData() |> filter(Team %in% rownames(M)))$Wins - 2,
  break.ties = TRUE
) {
  scores <- Standings$score
  names(scores) <- rownames(Standings)
  if (break.ties) {
    M <- M + 1 / 10^(5 - M)
  }
  M <- M[, apply(M, 2, function(x) all(x >= results)), drop = FALSE]
  L <- lapply(
    Entries,
    function(e) {
      colSums(
        M[intersect(e$teams, rownames(M)), , drop = FALSE],
        na.rm = TRUE
      ) +
        scores[e$email]
    }
  )
  Outcomes <- do.call(rbind, L)
  # rownames(Outcomes) <- rownames(Standings)
  WinningScores <-
    Outcomes |> apply(2, base::max, na.rm = TRUE)
  LosingScores <-
    Outcomes |> apply(2, base::min, na.rm = TRUE)

  res <- tibble(
    name = Standings$name,
    email = row.names(Standings)
  )
  res[["winning scenarios"]] <-
    sapply(res$email, function(em) {
      sum(WinningScores == Outcomes[em, ], na.rm = TRUE)
    })
  res[["losing scenarios"]] <-
    sapply(res$email, function(em) {
      sum(LosingScores == Outcomes[em, ], na.rm = TRUE)
    })
  res |>
    mutate(
      `win percent` = round(100 * `winning scenarios` / dim(M)[2], 2),
      `lose percent` = round(100 * `losing scenarios` / dim(M)[2], 2),
    )
}


## Do the Sweet 16 computations

Sweet16StandingsM <-
  isolate(
    resultsTable(entries = Entries, bracket = BracketM(),
                 games = GameScoresM(),
                 matchups = possibleMatchups(BracketM()))
  )

Sweet16StandingsW <-
  isolate(
    resultsTable(entries = Entries, bracket = BracketW(),
                 games = GameScoresW(),
                 matchups = possibleMatchups(BracketW()))
  )


# Save Men's Sweet16 to pins


if (write_pins) {
  Sweet16StandingsM |>
    my_pin_write(board = board, name = 'Sweet16StandingsM')


  WM <- winsMatrix(rounds = 4)
  b <- isolate(BracketM() |> addTeamStatus(GameScoresM()))
  rownames(WM) <- aliveTeams(b) |> pull(team)
  WM |>
    my_pin_write(board = board, name = "WM-M")
  tm <- isolate(TM())
  tc <- tournament_completions(tm, max_games_remaining = 15)
  tc |>
    my_pin_write(name = 'TCM', board = board)
  entries <- isolate(Entries())
  em <- build_entry_matrix(entries, ext = "M")
  h2h <- head2head(tm, em, tc, result = "data.frame")
  h2h |>
    my_pin_write(name = "H2HM", board = board)

  psm <- tc |>
    apply(2, function(x, e = em) {
      contest_scores(x, e)
    })
  psm |> round(12) |> my_pin_write(name = 'PossibleScoresM', board = board)

  psm |>
    apply(2, which.max) |>
    tibble(winner = _) |>
    group_by(winner) |>
    summarise(scenarios = n()) |>
    mutate(
      winner = rownames(em)[winner],
      p = scenarios / sum(scenarios)
    ) |>
    mutate(
      winner = reorder(winner, scenarios)
    ) |>
    my_pin_write(name = "WinnersTableM", board = board)
}

if (write_pins) {
  Sweet16StandingsW |>
    my_pin_write(board = board, name = 'Sweet16StandingsW')
  WM <- winsMatrix(rounds = 4)
  b <- isolate(BracketW() |> addTeamStatus(GameScoresW()))
  rownames(WM) <- aliveTeams(b) |> pull(team)
  WM |>
    my_pin_write(board = board, name = "WM-W")
  tw <- isolate(TW())
  tc <- tournament_completions(tw, max_games_remaining = 15)
  tc |>
    my_pin_write(name = 'TCW', board = board)
  entries <- isolate(Entries())
  ew <- build_entry_matrix(entries, ext = "W")
  h2h <- head2head(tw, ew, tc, result = "data.frame")
  h2h |>
    my_pin_write(name = "H2HW", board = board)

  psw <- tc |>
    apply(2, function(x, e = ew) {
      contest_scores(x, e)
    })
  psw |> round(12) |> my_pin_write(name = 'PossibleScoresW', board = board)

  psw |>
    apply(2, which.max) |>
    tibble(winner = _) |>
    group_by(winner) |>
    summarise(scenarios = n()) |>
    mutate(
      winner = rownames(ew)[winner],
      p = scenarios / sum(scenarios)
    ) |>
    mutate(
      winner = reorder(winner, scenarios)
    ) |>
    my_pin_write(name = "WinnersTableW", board = board)
}

if (write_pins) {
  ## individual bracket winners removed from competition
  mm <- apply(psm, 2, max)
  mw <- apply(psw, 2, max)

  zerom <- apply(psm, 1, function(x) x == mm) |> t()
  zerow <- apply(psw, 1, function(x) x == mw) |> t()

  psm[zerom] <- 0
  psw[zerow] <- 0

  denom <- ncol(psm) * ncol(psw)
  n <- nrow(em)
  ps <-
    sapply(1:n, function(x) {
      outer(psm[x, ], psw[x, ], "+")
    }) |>
    t()
  if (nrow(ps) != n) {
    ps <- t(ps)
  }
  ps |> round(12) |> my_pin_write(name = 'PossibleScoresC', board = board)
  ps |>
    apply(2, which.max) |>
    tibble(winner = _) |>
    group_by(winner) |>
    summarise(scenarios = n()) |>
    mutate(
      winner = rownames(em)[winner],
      p = scenarios / sum(scenarios)
    ) |>
    mutate(
      winner = reorder(winner, scenarios)
    ) |>
    my_pin_write(name = 'WinnersTableC', board = board)
}