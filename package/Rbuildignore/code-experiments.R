
# library(madness)
source('../../package/R/data_utils.R')
source('../../package/R/matrix_and_vector.R')
source('../../package/R/scenarios.R')

library(ggformula)

BracketM <- load_bracket('data/2022/bracket-2022-M.csv')
BracketW <- load_bracket('data/2022/bracket-2022-W.csv')

TMinit <- tournament_init(names = BracketM[['team']], seeds = BracketM[['seed']], label = "M")
TWinit <- tournament_init(names = BracketW[['team']], seeds = BracketW[['seed']], label = "W")

Entries <- load_entries_from_dropbox(TMinit, path = "data/2022/Entries/", year = 2022)
Entries0 <- load_entries_from_dropbox(TMinit, path = "data/2022/Entries/", year = 2022, keep.all = TRUE)

EM <- build_entry_matrix(Entries, ext = "M")
EW <- build_entry_matrix(Entries, ext = "W")

ScoresM <- LoadGameScores()

ScoresM <- load_game_scores('data/Scores/2022/Mens/scores-2022-M.csv')
TM <-
  tournament_init(names = Bracket2022M[['team']], seeds = Bracket2022M[['seed']], label = "M") |>
  tournament_update(ScoresM[['game_number']], ScoresM[['winner_01']])

contest_standings(TM, EM, Bracket2022M)

round_by_round(TM, EM) |> str()
purrr::map_int(round_by_round(TM, EM), 1, sum)


alive(TM)

n_games_remaining(TM)
home_team(TM)
away_team(TM)
winner(TM)
loser(TM)
all_games(TM)
all_games(TM, determined.only = TRUE)
all_games(TM, unplayed.only = TRUE)
all_games(TM, determined.only = TRUE, unplayed.only = TRUE)


rownames(EM) |> abbreviate(4)


W <- wins(TM); W

TM |> scores(EM)
TM |> scores(EM, dust = FALSE)


round_by_round(TM, EM) |> apply(1, function(x) paste(paste(x, collapse = " + "), " = ", sum(x)))

possible_winners(TM)
opponents(TM)

# debugonce(max_possible_scores)
max_possible_scores(TM, EM)
scores(TM, EM, dust = FALSE)

tcm <-
  TM |> tournament_completions(max = 19)

PossibleScoresM <-
  tcm |>
  apply(2, function(x, e = EM) {scores(x, e)})

WinnersTableM <-
  PossibleScoresM |>
  apply(2, which.max) %>%
  tibble(winner = .) |>
  group_by(winner) |>
  summarise(scenarios = n()) |>
  mutate(
    winner = rownames(EM)[winner],
    p = scenarios / sum(scenarios)
  ) |>
  mutate(
    winner = reorder(winner, scenarios)
  )



  WinnersTableM |> gf_col(winner ~ p) |>
    gf_labs(x = "percent of scenarios that win") |>
    gf_refine(scale_x_continuous(labels = scales::label_percent()))

WinningScoreM <-
  PossibleScoresM |>
  round() |>
  apply(2, max) %>%
  tibble(score = .)

WinningScoreM |>
  gf_histogram(~ score, binwidth = 1)

dim(TCM)
dim(PossibleScoresM)

# H2H <-
#   outer(1:nrow(E), 1:nrow(E), Vectorize(function(x,y) sum(ScoresM[x, ] > ScoresM[y, ]) - sum(ScoresM[x,] < ScoresM[y,]))) |>
#   as.table() |>
#   as.data.frame() |>
#   setNames(c("winner", "loser", "scenarios")) |>
#   mutate(
#     winner = LETTERS[winner] |>
#       factor( levels = ScoresM |> apply(1, mean) |> sort() |> rev() |> names() ),
#     loser = LETTERS[loser] |>
#       factor( levels = ScoresM |> apply(1, mean) |> sort() |> rev() |> names() )
#   )

h2hm <-
  head2head(ScoresM = PossibleScoresM)

# H2HM |>
h2hm |>
  gf_tile(scenarios ~ loser_abbrv + winner_abbrv) |>
  gf_labs(fill = "wins - losses") |>
  gf_refine(
    scale_fill_gradient2(low = "red", high = "steelblue", mid = "gray90")
  ) |>
  plotly::ggplotly()

H2HM |>
  gf_tile(scenarios ~ loser_name + winner_name) |>
  gf_labs(fill = "wins - losses") |>
  gf_refine(
    scale_fill_gradient2(low = "red", high = "steelblue", mid = "gray80")
  ) |>
  plotly::ggplotly()
# rownames(H2H) <- colnames(H2H) <- LETTERS[1:25]
# H2H |> d3heatmap::d3heatmap()


# Grid <-
#   expand.grid(x = 1:25, y =1:25) |>
#   mutate(
#     data = ScoresM[c(x, y), ] |> apply(2, diff) |> tibble()
#   )
#
# Plots <-
#   1:nrow(Grid) |>
#   lapply(function(r) { gf_histogram(~ (y - x), data = x) })


tournament <- T; entries <- E

tibble(
  player = row.names(entries),
  score = scores(tournament, entries, dust = FALSE),
  score_d = scores(tournament, entries, dust = TRUE),
  score_details =  round_by_round(tournament, entries) |>
    apply(1, function(x)
      paste(sum(x), " = ", paste(x, collapse = " + ")))
) |>
  arrange(score_d)

RbR <-
  round_by_round(tournament, entries) |>
  apply(1, function(x) sprintf(x, fmt = "%02d") |> rev() |> paste(collapse = "")) |>
  sort() |>
  rev() |>
  readr::parse_number()

RbR
outer(RbR, RbR, function(x, y) x > y)

scores_table(T, E)

entry_teams(T, E)
