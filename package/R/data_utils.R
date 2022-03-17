
#' @export
#' @examples
#' EM <- build_entry_matrix(Ent, "M")
#' EW <- build_entry_matrix(Ent, "W")

#' @param E a list of lists. The nested lists should have slots for
#'    "name", "email", "dept", "time", "time", "points", "teamsLogical", and "teamsLogicalW".
#'    The latter two are logical vectors with as many values as teams in the tournament
#' @param ext Either "M" or "W" indicating which tournament.
#'
#'
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

#' Load Entries From Files
#'
#' Load entries from files in a directory that match a specified pattern.
#'
#' @inheritParams scores
#' @param path directory in which entry files are located.
#' @param pattern regex pattern.
#' @param year used to create the default value of pattern.
#' @export
load_entries_from_files <-
  function(tournament, path, year = 2022, pattern =  paste0("Entry-", year, ".*rds"))
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

      res[[e$email]] <- e
    }
    res
  }

