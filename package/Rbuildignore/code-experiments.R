
library(madness)
library(ggformula)

BracketM <<- LoadBracket('data/bracket2022.csv')
BracketW <<- LoadBracket('data/bracket2022w.csv')

TMinit <<- tournament_init(names = BracketM[['team']], seeds = BracketM[['seed']], label = "M")
TWinit <<- tournament_init(names = BracketW[['team']], seeds = BracketW[['seed']], label = "W")

Entries <<- load_entries_from_files(TMinit, path = "data/Entries/2022/", year = 2022)
Entries0 <<- load_entries_from_files(TMinit, path = "data/Entries/2022/", year = 2022, keep.all = TRUE)

EM <<- build_entry_matrix(Entries, ext = "M")
EW <<- build_entry_matrix(Entries, ext = "W")

ScoresM <- LoadGameScores()
T <-
  tournament_init(names = Bracket2022M[['team']], seeds = Bracket2022M[['seed']], label = "M") |>
  tournament_update(ScoresM[['game_number']], ScoresM[['winner_01']])

contest_status(T, EM, Bracket2022M)

round_by_round(T, EM) |> str()
purrr::map_int(round_by_round(T, EM), 1, sum)


alive(T)

home_team(T)
away_team(T)
winner(T)
loser(T)
all_games(T)
all_games(T, determined.only = TRUE)
all_games(T, unplayed.only = TRUE)
all_games(T, determined.only = TRUE, unplayed.only = TRUE)

# E <- (mosaic::do(25) * as.numeric(1:(2^R) %in% sample(1:(2^R), 2^R / 4))) |> as.matrix()
E <- madness::build_entry_matrix(Ent, "M")
EW <- madness::build_entry_matrix(Ent, "W")

rownames(E) |> abbreviate(4)

# rownames(E) <- LETTERS[1:nrow(E)]

W <- wins(T); W

T |> scores(E)
T |> scores(E, dust = FALSE)


round_by_round(T, E) |> apply(1, function(x) paste(paste(x, collapse = " + "), " = ", sum(x)))

possible_winners(T)
opponents(T)

# debugonce(max_possible_scores)
max_possible_scores(T, E)
scores(T, E, dust = FALSE)

TC <-
  T |> tournament_completions()

ScoresM <-
  TC |>
  apply(2, function(x, e = E) {scores(x, e)})

WinnersTable <-
  ScoresM |>
  apply(2, which.max) %>%
  tibble(winner = .) |>
  group_by(winner) |>
  summarise(scenarios = n()) |>
  mutate(
    winner = rownames(E)[winner],
    p = scenarios / sum(scenarios)
  ) |>
  mutate(
    winner = reorder(winner, scenarios)
  )



  WinnersTable |> gf_col(winner ~ p) |>
    gf_labs(x = "percent of scenarios that win") |>
    gf_refine(scale_x_continuous(labels = scales::label_percent()))

WinningScore <-
  ScoresM |>
  round() |>
  apply(2, max) %>%
  tibble(score = .)

WinningScore |>
  gf_histogram(~ score, binwidth = 1)

dim(TC)
dim(ScoresM)

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

H2H <-
  head2head(ScoresM = ScoresM)

H2H |>
  gf_tile(scenarios ~ loser_abbrv + winner_abbrv) |>
  gf_labs(fill = "wins - losses") |>
  gf_refine(
    scale_fill_gradient2(low = "red", high = "steelblue", mid = "gray90")
  )

H2H |>
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
