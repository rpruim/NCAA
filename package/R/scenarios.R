
#' Create Scenario Matrix
#'
#' Create a bit matrix of possible outcomes of unknown games.
#' Note: This gets large fast, so the input is capped at 15
#'
#' @importFrom bitops bitAnd
#' @param n_games number of games with undetermined outcome
#' @param max_games_remaining maximum number of games remaining (to avoid long computation time)
#' @examples
#' scenario_matrix(5)
#' @export

scenario_matrix <- function(n_games = 15, max_games_remaining = 15) {
  if (n_games > max_games_remaining) {
    warning ("Increase max_games_remaining to create such a large matrix.")
    return(matrix(nrow = n_games, ncol = 0))
  }

  if (n_games < 1) return(matrix(nrow = 0, ncol = 0))

  bits(seq(2^n_games), bit_width = n_games) |> as.logical() |> as.numeric() |> matrix(nrow = n_games)
}

bits0 <- function(x, bit_width = 15) {
  bitops::bitAnd(x, 2^(0:(bit_width - 1))) # > 1) |> as.numeric()
}

bits <- Vectorize(bits0, "x")


#' All Possible Tournament Completions
#'
#' Compute a matrix in which each column is one of the possible ways the tournament could end.
#'
#' @inheritParams contest_scores
#' @returns a matrix with one column for each possible way the tournament could end and one row
#'   for each team.  Values indicate number of wins for a team in one of the outcomes.
#'
#' @export
tournament_completions <- function(tournament, max_games_remaining = 15) {
  na_idx <- which(is.na(tournament))

  # nothing to do here; but return tournament as 1-col matrix
  if (length(na_idx) < 1) return(matrix(tournament, ncol = 1))

  M <- scenario_matrix(length(na_idx), max_games_remaining = max_games_remaining)
  if (ncol(M) == 0) {
    # decide how to handle if called before we get to Sweet 16.
    # return NULL?
  }
  apply(M, 2, function(x, t = tournament) { t |> tournament_update(na_idx, x) })
}
