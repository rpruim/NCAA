% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix_and_vector.R
\name{winner}
\alias{winner}
\alias{winner_team_name}
\alias{loser}
\alias{loser_team_name}
\alias{home_team}
\alias{away_team}
\title{Information about teams involved in a game}
\usage{
winner(tournament, game = 1L:n_games(tournament))

winner_team_name(tournament, game = 1L:n_games(tournament))

loser(tournament, game = 1L:n_games(tournament))

loser_team_name(tournament, game = 1L:n_games(tournament))

home_team(tournament, game = 1L:n_games(tournament))

away_team(tournament, game = 1L:n_games(tournament))
}
\arguments{
\item{tournament}{A tournament object}

\item{game}{A numeric vector of game numbers}
}
\description{
Information about teams involved in a game
}
