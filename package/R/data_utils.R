
#' Create Entry Matrix
#'
#' Convert a list of lists into a matrix representing each entrants selections.
#' Attributes are used to store some additional information.
#'
#' @param E a list of lists. The nested lists should have slots for
#'    "name", "email", "dept", "time", "time", "points", "teamsLogical", and "teamsLogicalW".
#'    The latter two are logical vectors with as many values as teams in the tournament
#' @param ext Either "M" or "W" indicating which tournament.
#'
#'
#' @export
#' @examples
#' EM <- build_entry_matrix(Ent, "M")
#' EW <- build_entry_matrix(Ent, "W")

build_entry_matrix <- function(E, ext = c("M", "W")){
  ext <- match.arg(ext)
  if (ext == "M") ext <- ""
  res <- sapply(E, function(x) x[[paste0('teamsLogical', ext)]] |> as.numeric()) |> t()
  for (a in c("name", "email", "dept", "time", "points")) {
    attr(res, a) <- sapply(E, function(x) x[[a]])
  }
  rownames(res) <- sapply(E, function(x) x[['name']]) |> as.vector()
  res
}

readRDS_from_dropbox <- function(file, dtoken = my_dropbox_token()) {
  localfile = tempfile()
  print(c(file = file, localfile = localfile))
  drop_download(file, local_path = localfile, overwrite = TRUE, dtoken = dtoken)
  readRDS(localfile)
}


#' Load Entries From Dropbox
#'
#' Load entries from files in a dropbox directory that match a specified pattern.
#'
#' @inheritParams contest_scores
#' @param path directory in which entry files are located.
#' @param pattern regex pattern.
#' @param year used to create the default value of pattern.
#' @export
load_entries_from_dropbox <-
  function(tournament, path, year = 2023, pattern =  paste0("Entry-", year, ".*rds"),
           dtoken = my_dropbox_token(),
           keep.all = FALSE) {
    efiles_info <- drop_dir(file.path('ncaa', path), dtoken = dtoken) # tibble of directory info
    efiles <- efiles_info$name |> stringr::str_subset(pattern)
    res <- list()
    # read files in order; newer entries with same email will clobber older ones
    for (f in sort(efiles)) {
      e <- readRDS_from_dropbox(file.path("ncaa", path, f), dtoken = dtoken)

      # updating names based on the play-in game winners not happening at the moment
      # e$teams <- update_teams(e$teams, Bracket$team)
      # names(e$teamsLogical) <- team_names(tournament) # update_teams(names(e$teamsLogical), Bracket$team) # gsub(names(playin[i]), playin[i], names(e$teamsLogical))

      # ensure the order is the same for all the logical vectors and matches the bracket order
      # this should now be happening at the time of creation
      # e$teamsLogical <- e$teamsLogical[Bracket$team]
      if (keep.all) {
        res[[paste0(sprintf("%03d", 1 + length(res)), "-", e$email)]] <- e
      } else {
        if (!is.null(res[[e$email]])) {
          if (purrr::pluck(e, 'points') < 190 && purrr::pluck(res, e$email, 'points') >= 190) {
            e[['points']] <- NULL
            e[['teams']] <- NULL
            e[['teamsLogical']] <- NULL
          }
          if (purrr::pluck(e, 'pointsW') < 190 && purrr::pluck(res, e$email, 'pointsW') >= 190) {
            e[['pointsW']] <- NULL
            e[['teamsW']] <- NULL
            e[['teamsLogicalW']] <- NULL
          }
          res[[e$email]] <- modifyList(res[[e$email]], e)
        } else {
          res[[e$email]] <- e
        }
      }
    }
    res
  }

#' Load Entries From Pins
#'
#' Load entries from pins that match a specified pattern.
#'
#' @inheritParams contest_scores
#' @param board a pins board
#' @param pattern regex pattern.
#' @param year used to create the default value of pattern.
#' @export

load_entries_from_pins <-
  function(board, tournament, year = 2023, pattern = paste0("NCAA-entry-", year), keep.all = FALSE) {
    epin_names <- board |> pins::pin_search(pattern) |> dplyr::pull(name)

    res <- list()
    # read files in order; newer entries with same email will clobber older ones
    for (n in epin_names) {
      e <- board |> pins::pin_read(n)
      res[[e$email]] <- e
    }
    return(res)
  }

pin_write_entry <- function(board, entry, year = entry$year, name = NULL, ...) {
  if (is.null(year)) { year <- 2023 }
  if (is.null(name)) {
    email_name <- entry$email |>
      stringr::str_replace_all("[@.]", "_")
    name <- paste0("rpruim/NCAA-", year, "-entry-", email_name)
  }
  pin_write(board, x = entry, name = name, ...)
}

# board |> pin_write_entry(ee)
# board |> load_entries_from_pins(year = 2022)


#' Load Entries From Local Files
#'
#' Load entries from files in a directory that match a specified pattern.
#'
#' @inheritParams contest_scores
#' @param path directory in which entry files are located.
#' @param pattern regex pattern.
#' @param year used to create the default value of pattern.
#' @export
load_entries_from_files <-
  function(tournament, path, year = 2022, pattern =  paste0("Entry-", year, ".*rds"), keep.all = FALSE)
  {
    efiles <- dir(path, pattern = pattern, full.names = TRUE)
    res <- list()
    # read files in order; newer entries with same email will clobber older ones
    for (f in sort(efiles)) {
      e <- readRDS(f)

      # updating names based on the play-in game winners not happening at the moment
      # e$teams <- update_teams(e$teams, Bracket$team)
      # names(e$teamsLogical) <- team_names(tournament) # update_teams(names(e$teamsLogical), Bracket$team) # gsub(names(playin[i]), playin[i], names(e$teamsLogical))

      # ensure the order is the same for all the logical vectors and matches the bracket order
      # this should now be happening at the time of creation
      # e$teamsLogical <- e$teamsLogical[Bracket$team]
      if (keep.all) {
        res[[paste0(sprintf("%03d", 1 + length(res)), "-", e$email)]] <- e
      } else {
        if (!is.null(res[[e$email]])) {
          if (purrr::pluck(e, 'points') < 190 && purrr::pluck(res, e$email, 'points') >= 190) {
            e[['points']] <- NULL
            e[['teams']] <- NULL
            e[['teamsLogical']] <- NULL
          }
          if (purrr::pluck(e, 'pointsW') < 190 && purrr::pluck(res, e$email, 'pointsW') >= 190) {
            e[['pointsW']] <- NULL
            e[['teamsW']] <- NULL
            e[['teamsLogicalW']] <- NULL
          }
          res[[e$email]] <- modifyList(res[[e$email]], e)
        } else {
          res[[e$email]] <- e
        }
      }
    }
    res
  }

#' Load Bracket
#'
#'
#' Load bracket from a CSV file
#'
#' @export
load_bracket <- function(file) {
  bracket <- readr::read_csv(file)

  bracket |>
    dplyr::mutate(
      cost = seedCost[seed],
      cost.old = seedCostOld[seed],
      # which regions play in final 4 is determined by order of appearance in csv
      slot = ( as.numeric(factor(region, levels = unique(region), # c('east','west','south','midwest'),
                                 ordered=TRUE)) * 100 + order(seedOrder)[seed] )
    ) |>
    dplyr::arrange(slot)
}

#'
#' Load Games Scores from Dropbox
#'
#' Load game scores from a CSV file or files on dropbox.
#'
#' @export
#' @param files A vector of csv files names. Each file should have coluns `game_number` (int),
#' `winner_01` (0 if "home" wins, 1 if "away" wins),
#'   `home` (chr), `away` (chr), `hscore` (int), and `ascore (int)`
#' @returns a data frame containing information about each game. `winner` and `loser` columns are computed from
#'   the scores and `home` and `away`.
#' @importFrom dplyr group_by slice_tail mutate
#' @importFrom purrr map_df
#' @importFrom tibble tibble

load_game_scores_from_dropbox <- function(files) {
  files <- file.path('ncaa', files)
  if (length(files) < 1)
    return(
      tibble::tibble(game_number = NA, winner_01 = NA,
             home = NA, away = NA, hscore = NA, ascore = NA) |>
        head(0))

  res <- list()
  # read files in order; newer entries with same teams will clobber older ones
  purrr::map_df(files, rdrop2::drop_read_csv) # , col_types = "iiccii") |>
    dplyr::group_by(game_number) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::mutate(
      winner = ifelse(hscore > ascore, home, away),
      loser = ifelse(hscore < ascore, home, away)
    )
}

#'
#' Load Games Scores
#'
#' Load game scores from a CSV file or files.
#'
#' @export
#' @param files A vector of csv files names. Each file should have coluns `game_number` (int),
#' `winner_01` (0 if "home" wins, 1 if "away" wins),
#'   `home` (chr), `away` (chr), `hscore` (int), and `ascore (int)`
#' @returns a data frame containing information about each game. `winner` and `loser` columns are computed from
#'   the scores and `home` and `away`.
#' @importFrom dplyr group_by slice_tail mutate
#' @importFrom purrr map_df
#' @importFrom tibble tibble

load_game_scores <- function(files) {
  if (length(files) < 1)
    return(
      tibble::tibble(game_number = NA, winner_01 = NA,
             home = NA, away = NA, hscore = NA, ascore = NA) |>
        head(0))

  res <- list()
  # read files in order; newer entries with same teams will clobber older ones
  purrr::map_df(files, readr::read_csv, col_types = "iiccii") |>
    dplyr::group_by(game_number) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::mutate(
      winner = ifelse(hscore > ascore, home, away),
      loser = ifelse(hscore < ascore, home, away)
    )
}

