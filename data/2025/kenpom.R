winprob <- function(e1, t1, e2, t2) {

  pts <- (e1 - e2) * (t1 + t2) / 200
  pnorm(pts, mean = 0, sd = 11)

}

W <-
  outer(
  1:68, 1:68,
  function(a, b) {
    winprob(
      ken[a, "AdjEM"][[1]], ken[a,"AdjT"][[1]],
      ken[b, "AdjEM"][[1]], ken[b,"AdjT"][[1]])
  }
)

heatmap(W)
dim(W)

bracket <- read_csv("bracket-2025-M-kenpom.csv")
playoff <- read_csv("playoffstatus-2025-m.csv", col_types = "ccnnnnnnn")
athletic <- read_csv('athletic-2025.csv')
athletic |> summarise(across(W1:W6, ~ sum(.x, na.rm  = TRUE)/100))

wp <- bracket |> left_join(athletic) |>
  rowwise() |>
  mutate(ew = sum(c_across(W6:W1))/100) |>
  ungroup() |>
  mutate(
    slot = order(madness::seedOrder)[Seed],
    cost = madness::seedCost[Seed],
    roi = 200 * ew / cost)

sum(wp$ew, na.rm = TRUE)

