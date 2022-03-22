
#' @export
seedOrder   <- c(1,16, 8,9, 5,12, 4,13, 6,11, 3,14, 7,10, 2,15)
#' @export
seedCostOld <- c(22,18,15,12,10, 9, 7, 6,5,4,3,2,1,1,1,1)
#' @export
seedCost    <- c(48,35,29,23, 17,15,13,11, 10,9,8,7, 4,3,1,1)

#' Create a tournament with no games played
#'
#' Create a tournament with no games played
#'
#' @param rounds Number of rounds in a complete single elimination tournament
#' @export
tournament_init <-
  function(names, seeds = NULL, label = "G") {
    rounds <- ceiling(log2(length(names)))
    res <- rep(NA, 2^rounds - 1)
    if (length(names) != 1 + length(res)) {
      warning('Not enough teams to fill the tournament.')
    }
    attr(res, "team_names") <- names
    attr(res, "seeds") <- seeds
    names(res) <- paste0(label, seq_along(res))
    class(res) <- unique(c('tournament', class(res)))
    res
  }

#' @export
#' @inheritParams contest_standings
n_games <- function(tournament) {
  length(tournament)
}

#' @export
#' @inheritParams ngames
n_teams <- function(tournament) {
  1 + length(tournament)
}

#' @export
#' @inheritParams ngames
n_games_remaining <-
  function(tournament) {
    sum(is.na(tournament))
  }

#' @export
winner <- function(tournament, game = 1L:n_games(tournament)) {
  tournament[game] |> as.vector()
}

#' @export
loser <- function(tournament, game = 1L:n_games(tournament)) {
  force(game)
  tournament <- c(tournament, 1:n_teams(tournament))
  (tournament[2*game] + tournament[2*game + 1] - tournament[game]) |> as.vector()
}

#' @export
alive <- function(tournament, teams = 1L:n_teams(tournament)) {
  ! teams %in% loser(tournament)
}

#' @export
home_team <- function(tournament, game = 1L:n_games(tournament)) {
  force(game)
  tournament <- c(tournament, 1:n_teams(tournament))
  tournament[2 * game] |> as.vector()
}

#' @export
home_team_name <- function(tournament, game = 1L:n_games(tournament)) {
  attr(tournament, "team_names")[home_team(tournament, game)]
}

#' @export
away_team_name <- function(tournament, game = 1L:n_games(tournament)) {
  attr(tournament, "team_names")[away_team(tournament, game)]
}

#' @export
team_names <- function(tournament, teams = 1L:n_teams(tournament)) {
  attr(tournament, "team_names")[teams]
}

#' @export
away_team <- function(tournament, game = 1L:n_games(tournament)) {
  force(game)
  tournament <- c(tournament, 1:n_teams(tournament))
  tournament[2 * game + 1] |> as.vector()
}

#' @export
all_games <- function(tournament, unplayed.only = FALSE, determined.only = FALSE) {
  n <- n_games(tournament)
  names <-
    gsub( "NA", "TBD",
          paste(
            away_team_name(tournament, 1L:n),
            " vs. ",
            home_team_name(tournament, 1L:n),
            sep="")
    )
  res <- 1L:n
  names(res) <- names
  res[(!unplayed.only | is.na(tournament[1:n])) & (!determined.only | !grepl("TBD", names))]
}

#' Tabular report of contest standings

#' Tabular report of contest standings
#'
#' @inheritParams contest_scores
#' @export
contest_standings <- function(tournament, entries, bracket) {
  if (sum(wins(tournament), na.rm = TRUE) < 1) {
    return(
      dplyr::tibble(
        email = attr(entries, "email"),
        name = attr(entries, "name"),
        dept = attr(entries, "dept"),
        score = NA,
        dusty_score = NA,
        `score details` = NA,
        `guaranteed wins` = NA,
        `max possible` = NA,
        `teams remaining` = NA,
        `points remaining` = NA,
        `teams lost` = NA,
      )
    )
  }
  Bracket <- bracket |>
    mutate(
      cost = seedCost[seed],
      alive = alive(tournament)
    )
  PW <- possible_winners(tournament)
  Results <-
    dplyr::tibble(
      email = attr(entries, "email"),
      name = attr(entries, "name"),
      dept = attr(entries, "dept"),
      score = contest_scores(tournament, entries, dust = FALSE),
      dusty_score = contest_scores(tournament, entries, dust = TRUE),
      `score details` =
        round_by_round(tournament, entries) |>
        apply(1, function(x) paste(sum(x), " = ", paste(x, collapse = " + "))),
      `guaranteed wins` =
        apply(entries, 1, function(x) {
          purrr::map_int(PW, function(pw) { all(pw %in% which(as.logical(x))) }) |> sum()
        } ),
      `max possible` =
        apply(entries, 1, function(x) {
          purrr::map_int(PW, function(pw) { any(pw %in% which(as.logical(x))) }) |> sum()
        } ),
      `teams remaining` =
        apply( entries, 1,
               function(x) {
                 BracketLeft <- Bracket %>% filter(x & alive) %>% arrange(seed)
                 numberLeft <- sum( x * Bracket[['alive']] )
                 paste(
                   sprintf("%02d", numberLeft), ": ",
                   paste(BracketLeft[['team']], " (", BracketLeft[['seed']], ")",
                         sep="",
                         collapse=", "),
                   collapse="")
               }
        ),
      `points remaining` =
        apply(entries, 1, function(x) { sum(x * Bracket[['cost']] * Bracket[['alive']]) } ),
      `teams lost` =
        apply(entries, 1,
              function(x) {
                BracketLost <- Bracket %>% filter(x & !alive) %>% arrange(seed)
                numberLost <- sum( x * (!Bracket[['alive']]) )
                paste(
                  sprintf("%02d", numberLost), ": ",
                  paste(BracketLost[['team']], " (", BracketLost[['seed']], ")",
                        sep="",
                        collapse=", "),
                  collapse="")
              }
        )
    )
  Results <- Results %>% arrange(desc(dusty_score))
  # rownames(Results) <- Results[['email']]
  Results %>% select(-dusty_score)
}
# Store tournament with 2^n teams as vector of length 2^n - 1
#   * initialize as rep(NA,2^n - 1)
#   * game g pairs winners of games 2*g and 2*g + 1
#   * consider initials teams as winners of games 2^n:(2^(n+1) - 1)

#' Compute Contest Scores
#'
#' Compute scores for each entrant. Each entrant receives one point for each win by one of the
#'   entrants selected teams.
#'
#' @param tournament a 0-1 vector of tournament game outcomes. Games are played from high index to
#'   low index. Game g pairs the winners of games `2*g` and `2*g + 1`.
#' @param entries a matrix with a row for each entrant and a column for each team.
#'   Entries indicate if the team was chosen by the entrant.
#' @param dust a logical indicating whether tie breaking dust should be added to the scores.
#'
#' @export
contest_scores <- function(tournament, entries, dust = TRUE) {
  W <- wins(tournament)
  if (dust) {
    # 7 dust amounts for wins 0 through 6
    eps = c(0, rev(1 / cumprod(5 * c(1, 2, 5, 10, 20, 100))))
    W <- W + eps[1 + W]
  }
  entries %*% W |>
    as.vector() |>
    setNames(rownames(entries))
}

#' @export
max_possible_scores <-
  function(tournament, entries, dust = TRUE) {
    pw <- possible_winners(tournament)
    purrr::map_int(1:nrow(entries),
           function(e) {purrr::map_int(pw, function(x) any(which(as.logical(entries[e,])) %in% x)) |> sum() }
    ) |>
      as.vector() |>
      setNames(rownames(entries))
  }


#' @returns a list containing a vector of possible winners for each game
#' @export
possible_winners <-
  function(tournament) {
    games <- seq_along(tournament) |> rev()
    tournament <- c(tournament, c(1:(length(tournament) + 1)))
    l <- tournament |> as.list()
    for(g in games) {
      if (is.na(tournament[g])) {
        l[[g]] <- c(l[[2*g]], l[[2*g + 1]]) |> as.vector()
      } else {
        l[[g]] <- tournament[g] |> as.vector()
      }
    }
    l[sort(games)]
  }

#' @returns a list containing a opponents for each game or NA if unknown.
#' @export
opponents <-
  function(tournament) {
    games <- seq_along(tournament) |> rev()
    tournament <- c(tournament, c(1:(length(tournament) + 1)))
    l <- tournament |> as.list()
    for(g in games) {
      l[[g]] <- c(tournament[[2*g]], tournament[[2*g + 1]]) |> as.vector()
    }
    l[sort(games)]
  }



#' Round-by-round scores
#'
#' Compute an entrant-by-round matrix of round scores for each entrant.
#'
#' @inheritParams contest_scores
#' @returns A matrix with a row for each entrant and a column for each round of the tournament.
#'   Entries indicate the number of wins for each player in each of the rounds.
#'
#' @importFrom purrr map map_int
#' @export
round_by_round <- function(tournament, entries) {
  W <- wins(tournament)
  res <-
    purrr::map(
    1:nrow(entries),
    function(e) {
      w <- W[as.logical(entries[e, ])]
      purrr::map_int(1:max(W, na.rm = TRUE), function(r) sum(w >= r, na.rm = TRUE))
    }
  )
  do.call(rbind, res)
}

#' Team Names
#'
#' Get team names from a tournament object.
#'
#' @inheritParams contest_scores
#' @export
team_names <- function(tournament) {
  attr(tournament, "team_names")
}

#' Update tournament based on game outcomes
#'
#' Update tournament based on game outcomes
#'
#' @inheritParams contest_scores
#' @param games a vector of game indices
#' @param results a 0-1 vector of results for `games`. 0 indicates that the lower indexed team won.
#'   1 indicates that the higher indexed team won.
#' @export
tournament_update <-
  function(tournament, games, results) {
    # add team indices to end of tournament vector
    t <- c(tournament, 1L:(length(tournament) + 1))
    r <- rev(order(games))
    games <- games[r]
    results <- rep(results, length.out = length(games))
    results <- results[r]
    for(i in seq_along(games)) {
      t[games[i]] <-
        t[if (results[i]) {2 * games[i] + 1} else {2 * games[i]}]
    }
    res <- t[seq_along(tournament)] |> setNames(names(tournament))
    attr(res, 'team_names') <- attr(tournament, 'team_names')
    class(res) <- unique(c('tournament', class(res)))
    res
  }

#' Compute team wins
#'
#' Use tournament results to compute number of wins for each team in the tournament
#'
#' @param tournament a tournament vector
#' @returns an integer vector containing the number of wins for each team
#' @importFrom purrr map_int
#' @export

wins <- function(tournament) {
  tournament |>
    teams_idx() |>
    purrr::map_int(function(t) sum(tournament == t, na.rm = TRUE))
}

teams_idx <- function(tournament) {
  1L:(1 + length(tournament))
}

#' @export
head2head <-
  function(tournament, entries,
           TC = tournament_completions(tournament, max_games_remaining = max_games_remaining),
           ScoresM =  TC |> apply(2, function(x, e = entries) {contest_scores(x, e)}),
           team_names = dimnames(ScoresM)[[1]],
           max_games_remaining = 15) {
  n_e <- nrow(ScoresM)
  ordered_names <- team_names[order(apply(ScoresM, 1, mean))]
  res <-
    outer(1:n_e, 1:n_e,  # columns, rows
          Vectorize(function(x,y) sum(ScoresM[x, ] > ScoresM[y, ]))) |>
    as.table()

  rownames(res) <- 1:n_e
  colnames(res) <- 1:n_e

  res |>
    as.data.frame() |>
    setNames(c("key", "other", "scenarios")) |>
    mutate(
      key_name = team_names[key] |>
        factor( levels = ordered_names ), # ScoresM |> apply(1, mean) |> sort() |> rev() |> names() ),
      other_name = team_names[other] |>
        factor( levels = ordered_names ), # ScoresM |> apply(1, mean) |> sort() |> rev() |> names() ),
      key_abbrv = key_name |> abbreviate(6) |>
        factor( levels = ordered_names |> abbreviate(6)), # ScoresM |> apply(1, mean) |> sort() |> rev() |> names() |> abbreviate(6) ),
      other_abbrv = other_name |> abbreviate(6) |>
        factor( levels = ordered_names |> abbreviate(6))  # ScoresM |> apply(1, mean) |> sort() |> rev() |> names() |> abbreviate(6) )
    )
  }

#' @export
winners_table <-
  function(
    tournament, entries,
    TC = tournament_completions(tournament, max_games_remaining = max_games_remaining),
    ScoresM =  TC |> apply(2, function(x, e = entries) {contest_scores(x, e)}),
    max_games_remaining = 15)
  {
    ScoresM |>
      apply(2, which.max) %>%
      tibble(winner = .) |>
      group_by(winner) |>
      summarise(scenarios = n()) |>
      mutate(
        winner = rownames(entries)[winner],
        p = scenarios / sum(scenarios)
      ) |>
      mutate(
        winner = reorder(winner, scenarios)
      )
  }

#' Create scores table for a modeling competition
#'
#' Create scores table for a modeling competition.
#'
#' @export
scores_table <- function(tournament, entries) {
  scores <- contest_scores(tournament, entries, dust = FALSE)
  dusty_scores <- contest_scores(tournament, entries, dust = TRUE)
  tibble(
    player = row.names(entries),
    player_abbrv = abbreviate(row.names(entries), 6),
    score = scores,
    dusty_score = dusty_scores
  )

}

#' @export
entry_teams <- function(tournament, entries) {
  apply(entries, 1, function(x) paste(team_names(tournament)[as.logical(x)], collapse = ", ")) |>
    as.vector()
}

