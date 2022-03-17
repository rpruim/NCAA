
#' Create Scenario Matrix
#'
#' Create a bit matrix of possible outcomes of unknown games.
#' Note: This gets large fast, so the input is capped at 15
#'
#' @importFrom bitops bitAnd
#' @param n_games number of games with undetermined outcome
#' @param safe a logical indicating whether use should be blocked when `n_games` is large.
#' @examples
#' scenario_matrix(5)
#' @export

scenario_matrix <- function(n_games = 15, safe = TRUE) {
  if (n_games > 15 && safe) stop ("Set safe = FALSE to create such a large matrix.")

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
#' @inheritParams scores
#' @returns a matrix with one column for each possible way the tournament could end.
#'
#' @export
tournament_completions <- function(tournament, safe = TRUE) {
  na_idx <- which(is.na(tournament))

  # nothing to do here; but return tournament as 1-col matrix
  if (length(na_idx) < 1) return(matrix(tournament, ncol = 1))

  M <- scenario_matrix(length(na_idx), safe = safe)
  apply(M, 2, function(x, t = tournament) { t |> update_tournament(na_idx, x) })
}
