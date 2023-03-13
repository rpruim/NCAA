
convert538_to_bracket <- function(.data) {
  .data |>
    select(
      gender,
      team = team_name,
      seed_raw = team_seed,
      region = team_region
    ) |>
    mutate(
      seed = readr::parse_number(seed_raw)
    ) |>
    group_by(gender, region, seed ) |>
    summarise (team = paste(team, collapse = "/")) |>
    ungroup()
  }

setwd("~/projects/github/NCAA/data")

read.csv("2023/538forecasts.csv") |>
  convert538_to_bracket() |>
  filter(gender == "mens") |>
  readr::write_csv("2023/bracket-2023-M.csv")

read.csv("2023/538forecasts.csv") |>
  convert538_to_bracket() |>
  filter(gender == "womens") |>
  readr::write_csv("2023/bracket-2023-W.csv")