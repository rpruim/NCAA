
# this is the server logic for a shiny web application.
# you can find out more about building applications with shiny here:
#
# http://www.rstudio.com/shiny/

library(shiny)
library(dplyr)
library(pins)

board <-
  board_connect(server = "https://connect.cs.calvin.edu",
                key = "S2RwoRs5VIkMPCyELvj0mixL6QC4Emx1")

# board |> pin_search()
# library(madness)
source('package/R/data_utils.r')
source('package/R/matrix_and_vector.r')
source('package/R/scenarios.r')

library(waiter)
library(ggformula)
library(plotly)
theme_set(theme_bw())
# library(reactlog)
# reactlog_enable()


clean_name <- function(name) {
  paste0('rpruim/NCAA-2023-', name) |>
    stringr::str_replace_all('[@.]', '_')
}

my_pin_write <- function(object, name, board) {
  clean_name <- clean_name(name)
  print(c(writing = clean_name))
  pin_write(board, object, clean_name)
}

my_pin_read <- function(name, board, default) {
  clean_name <- clean_name(name)
  print(c(reading = clean_name))
  if (pin_exists(board, clean_name)) {
    pin_read(board, name = clean_name)
  } else {
    default
  }
}

my_pin_reactive_read <- function(board, name, default, interval = 60000){
  clean_name <- clean_name(name)
  print(c(rective = clean_name))
  if (!pin_exists(board, clean_name)) {
    cat("pin didn't exist, creating with default.")
    pin_write(board, x = default, name = clean_name)
  }
  pin_reactive_read(board = board, name = clean_name, interval = interval)
}

# mysaverds <- function(object, file = "", dtoken = my_dropbox_token(), ...) {
#   saverds(object = object, file = file, ...)
#   board |> pin_upload(file)
#   # drop_upload(file, path = 'ncaa/data/2023/entries', dtoken = dtoken)
# }


# myreadrds <- function(file, refhook = null, ...) { # , dtoken = my_dropbox_token()) {
#   board |> pin_download(name = file)
#   # drop_download(file, overwrite = TRUE, dtoken = dtoken)
#   readrds(file = file, refhook = refhook)
# }

config <- yaml::read_yaml('ncaa-2023.yml')

maxPoints <- 200

human_time <- function() format(Sys.time(), "%y%m%d-%h%m%os")


### some utility functions

# safely_readrds <- function(file, ...) {
#   if (file.exists(file)) {
#     myreadrds(file, ...)
#   } else {
#     null
#   }
# }

sessionid <- function(session) {
  digest::digest(session$request)
}

the_year <- function() {
  return(config[['year']])

  return(2023)
  # the above is a temporary hack.
  qq <- Query()
  if ("year" %in% names(qq)) {
    as.numeric(qq["year"])
  } else {
    default_year
  }
}

regionChoices <- function(region, bracket) {
  region <- bracket %>%
    rename(regn = region) %>%
    filter(regn == region) %>%
    rename(region = regn)

  res <- as.character(region$team)
  names(res) <- paste0(
    region$seed, " ", region$team,
    " [", region$cost, " pt", ifelse(region$cost==1, "", "s"),"]")
  res
}

shinyServer(function(input, output, session) {

  Query <- reactive( parseQueryString(session$clientdata$url_search) )

  AdminMode <- reactive({
      tolower(Query()["admin"]) %in% c("yes","y") || file.exists('admin.txt')
  })

  ##############################
  # logging

  rValues <- reactiveValues(newlogentries = 0)

  # logentries <-
  #   reactiveFileReader(
  #     2000, session = session,
  #     filepath = "ncaa.log",
  #     function(path) {
  #       logdata <- readr::read_csv(path, as.is=TRUE)
  #       names(logdata) <- c("time", "event", "session")
  #       logdata[rev(seq_len(nrow(logdata))), ] |>
  #         mutate(time = lubridate::ymd_hms(time) - lubridate::hours(4))
  #     }
  #   )
  #
  # createlogentry <- function(text) {
  #   isolate( rValues$newlogentries <- rValues$newlogentries + 1 )
  #   write(paste0(Sys.time(), ',"', text, '",', sessionid(session)),
  #         append=TRUE, file="ncaa.log")
  #   output$statusmessage <- renderText({ paste(Sys.time(), text, sep=": ") })
  # }
  #
  # session$onsessionended(function() {
  #   createlogentry("sesssion ended.")
  # })
  #
  # createlogentry("new session started.")

  ##############################
  # watch files/pins and load data

  Entries <-
    reactivePoll(
      60000,
      session = session,
      function() pin_search(board, search = "NCAA-2023-entry") |> pull(created) |> max(),
      function() {
        board |>
        load_entries_from_pins(
          year = config[['year']]
          )
      }
    )

  BracketM <- reactiveFileReader(
      60000,
      session = session,
      config[["brackets"]][1],    # "data/bracket2022.csv",
      load_bracket
    )

  BracketW <- reactiveFileReader(
      60000,
      session = session,
      config[["brackets"]][2],    #  "data/bracket2022w.csv",
      load_bracket
    )

  # todo: change this to watching a pin with scores
  empty_scores_df <-
    tibble(
      game_number = integer(0),
      winner_01 = integer(0),
      home = character(0),
      away = character(0),
      hscore = integer(0),
      ascore = integer(0),
      timestamp = character(0)
    )

  GameScoresM <-
    my_pin_reactive_read(
      board, name = config[['scores']][1],
      default = empty_scores_df)

  GameScoresW <-
    my_pin_reactive_read(
      board, name = config[['scores']][2],
      default = empty_scores_df)

  ##############################
  # entry matrices

  EM <- reactive({
    build_entry_matrix(Entries(), ext = "m")
    })
  EW <- reactive({
    build_entry_matrix(Entries(), ext = "w")
  })

  ##############################
  # brackets and tournament

  TMinit <- reactive({tournament_init(names = BracketM()[['team']], seeds = BracketM()[['seed']], label = "M")})
  TWinit <- reactive({tournament_init(names = BracketW()[['team']], seeds = BracketW()[['seed']], label = "W")})

  # todo: deal with scores
  TM <- reactive({
    scores <- GameScoresM()
    if (nrow(scores) > 0) {
      TMinit() |> tournament_update(games = scores[['game_number']], results = scores[['winner_01']])
    } else {
      TMinit()
    }
  })

  TW <- reactive({
    scores <- GameScoresW()
    if (nrow(scores) > 0) {
      TWinit() |> tournament_update( games = scores[['game_number']], results = scores[['winner_01']])
    } else {
      TWinit()
    }
  })

  CompletedGamesM <- reactive({
    GameScoresM() |>
      mutate(score = paste(pmax(hscore, ascore), "-", pmin(hscore, ascore))) |>
      mutate(
        winner = ifelse(winner_01, away, home),
        loser = ifelse(winner_01, home, away)
      ) |>
      select(game_number, winner, loser, score)
  })
  CompletedGamesW <- reactive({
    GameScoresW() |>
      mutate(score = paste(pmax(hscore, ascore), "-", pmin(hscore, ascore))) |>
      mutate(
        winner = ifelse(winner_01, away, home),
        loser = ifelse(winner_01, home, away)
      ) |>
      select(game_number, winner, loser, score)
  })

  ContestStandingsM <- reactive({
    contest_standings(TM(), EM(), BracketM())
  })
  ContestStandingsW <- reactive({
    contest_standings(TW(), EW(), BracketW())
  })
  ContestStandingsAll <- reactive({
    csm <- ContestStandingsM()
    csw <- ContestStandingsW()
    cs <- csm |> dplyr::full_join(csw, by = c('email' = 'email'), suffix = c('_m', '_w')) |>
      filter(score_m > 0 & score_w > 0)
    cs |>
      mutate(
        total = score_m + score_w,
        name = name_m,
        `guaranteed wins` = `guaranteed wins_m` + `guaranteed wins_w`,
        `max possible` = `max possible_m` + `max possible_w`
      ) |>
      arrange(desc(total)) |>
      rename(`men's wins` = score_m, `women's wins` = score_w) |>
      select(name, `men's wins`, `women's wins`, total, `guaranteed wins`, `max possible`)
  })



  # todo: deal with crystal ball
  # cachecrystalballm <- function() {
  #   tc <- tournament_completions(tm(), max_games_remaining = 15)
  #   tc |>
  #   # mysaverds(file.path(config[['crystal_ball_path']], 'tcm.rds'))
  #     my_pin_write(name = 'tcm', board = board)
  #
  #   h2h <- head2head(tm(), EM(), tc, result = "data.frame")
  #   h2h |>
  #   # mysaverds(file.path(config[['crystal_ball_path']], 'h2hm.rds'))
  #     my_pin_write(name = "h2hm", board = board)
  #
  #   ps <- tc |>
  #     apply(2, function(x, e = EM()) { contest_scores(x, e)} )
  #   ps |> round(12) |>
  #     # mysaverds(file.path(config[['crystal_ball_path']], 'possiblescoresm.rds'))
  #     my_pin_write(name = 'possiblescoresm', board = board)
  #
  #   ps |>
  #     apply(2, which.max) %>%
  #     tibble(winner = .) |>
  #     group_by(winner) |>
  #     summarise(scenarios = n()) |>
  #     mutate(
  #       winner = rownames(EM())[winner],
  #       p = scenarios / sum(scenarios)
  #     ) |>
  #     mutate(
  #       winner = reorder(winner, scenarios)
  #     ) |>
  #     # mysaverds(file.path(config[['crystal_ball_path']], 'winnerstablem.rds'))
  #     my_pin_write(name = "winnerstablem", board = board)
  # }
  #
  #
  # tcm <-
  #   pin_reactive_read(board, name = clean_name('tcm'))
  #   # reactiveFileReader(
  #   #   1000, session,
  #   #   file.path(config[['crystal_ball_path']], 'tcm.rds'),
  #   #   safely_readrds
  #   # )
  #
  # h2hm <-
  #   my_pin_reactive_read(board, name = 'h2hm', default = tibble())
  #
  # #   reactiveFileReader(
  # #   1000, session,
  # #   file.path(config[['crystal_ball_path']], 'h2hm.rds'),
  # #   safely_readrds
  # # )
  #
  # possiblescoresm <-
  #   my_pin_reactive_read(board, name = 'possiblescoresm', default = matrix())
  #
  # #   reactiveFileReader(
  # #   1000, session,
  # #   file.path(config[['crystal_ball_path']], 'possiblescoresm.rds'),
  # #   safely_readrds
  # # )
  #
  # winnerstablem <-
  #   my_pin_reactive_read(board, name = "winnerstablem", default = tibble())
  #   # reactiveFileReader(
  #   # 1000, session,
  #   # file.path(config[['crystal_ball_path']], 'winnerstablem.rds'),
  #   # safely_readrds)
  #
  # possiblescorestablem <- reactive({
  #   possiblescoresm() |>
  #     as.table() |>
  #     as.data.frame() |>
  #     setnames(c('name', 'sceneario', 'score')) |>
  #     group_by(name, score) |>
  #     tally()
  # })
  #
  # cachecrystalballw <- function() {
  #   tc <- tournament_completions(tw(), max_games_remaining = 15)
  #   tc |> my_pin_write(board, name = 'tcw')
  #
  #   h2h <- head2head(tw(), EW(), tc, result = "data.frame")
  #   h2h |>
  #     my_pin_write(name = 'h2hw', board = board)
  #
  #   ps <- tc |>
  #     apply(2, function(x, e = EW()) { contest_scores(x, e)} )
  #   ps |> round(12) |>
  #     my_pin_write(name = 'possiblescoresw', board = board)
  #
  #   ps |>
  #     apply(2, which.max) %>%
  #     tibble(winner = .) |>
  #     group_by(winner) |>
  #     summarise(scenarios = n()) |>
  #     mutate(
  #       winner = rownames(EW())[winner],
  #       p = scenarios / sum(scenarios)
  #     ) |>
  #     mutate(
  #       winner = reorder(winner, scenarios)
  #     ) |>
  #     my_pin_write(name = 'winnerstablew', board = board)
  # }
  #
  #
  # tcw <-
  #   my_pin_reactive_read(board, name = 'tcw', default = matrix())
  #
  #   # reactiveFileReader(
  #   #   1000, session,
  #   #   file.path(config[['crystal_ball_path']], 'tcw.rds'),
  #   #   safely_readrds
  #   # )
  #
  # h2hw <-
  #   my_pin_reactive_read(board, name = 'h2hw', default = tibble())
  # #   reactiveFileReader(
  # #   1000, session,
  # #   file.path(config[['crystal_ball_path']], 'h2hw.rds'),
  # #   safely_readrds
  # # )
  #
  # possiblescoresw <-
  #   my_pin_reactive_read(board, name = 'possiblescoresw', default = matrix())
  # #   reactiveFileReader(
  # #   1000, session,
  # #   file.path(config[['crystal_ball_path']], 'possiblescoresw.rds'),
  # #   safely_readrds
  # # )
  #
  # winnerstablew <-
  #   my_pin_reactive_read(board, name = 'winnerstablew', default = tibble())
  #   # reactiveFileReader(
  #   # 1000, session,
  #   # file.path(config[['crystal_ball_path']], 'winnerstablew.rds'),
  #   # safely_readrds)
  #
  # possiblescorestablew <- reactive({
  #   possiblescoresw() |>
  #     as.table() |>
  #     as.data.frame() |>
  #     setnames(c('name', 'sceneario', 'score')) |>
  #     group_by(name, score) |>
  #     tally()
  # })

  # observeEvent(
  #   input$recachebutton,
  #   {
  #     if (as.numeric(input$recachebutton) > 0 && AdminMode()) {
  #       if (n_games_remaining(tw()) <= 15) {
  #         cachecrystalballw()
  #       }
  #       if (n_games_remaining(tm()) <= 15) {
  #         cachecrystalballm()
  #       }
  #       if (n_games_remaining(tm()) + n_games_remaining(tw()) <= 15) {
  #         cachecrystalballc()
  #       }
  #     }
  #   })
  #
  # possiblescoresc <-
  #   my_pin_reactive_read(board, name = 'possiblescoresc', default = matrix())
  # #   reactiveFileReader(
  # #   1000, session,
  # #   file.path(config[['crystal_ball_path']], 'possiblescoresc.rds'),
  # #   safely_readrds
  # # )
  #
  # cachecrystalballc <- function() {
  #   psm <- possiblescoresm()
  #   psw <- possiblescoresw()
  #   denom <- ncol(psm) * ncol(psw)
  #   n <- nrow(EM())
  #   ps <-
  #     sapply(1:n,
  #            function(x) {
  #              outer(psm[x, ], psw[x, ], "+")
  #            }
  #     ) |> t()
  #   if (nrow(ps) != n) {ps <- t(ps)}
  #   ps |> round(12) |>
  #     my_pin_write(name = 'possiblescoresc', board = board)
  #   ps |>
  #     apply(2, which.max) %>%
  #     tibble(winner = .) |>
  #     group_by(winner) |>
  #     summarise(scenarios = n()) |>
  #     mutate(
  #       winner = rownames(EM())[winner],
  #       p = scenarios / sum(scenarios)
  #     ) |>
  #     mutate(
  #       winner = reorder(winner, scenarios)
  #     ) |>
  #     my_pin_write(name = 'winnerstablec', board = board)
  # }
  #
  # possiblescorestablec <- reactive({
  #   possiblescoresc() |>
  #     as.table() |>
  #     as.data.frame() |>
  #     setnames(c('name', 'sceneario', 'score')) |>
  #     group_by(name, score) |>
  #     tally()
  # })
  #
  # winnerstablec <-
  #   my_pin_reactive_read(name = 'winnerstablec', board = board, default = tibble())
  #   # reactiveFileReader(
  #   # 1000, session,
  #   # file.path(config[['crystal_ball_path']], 'winnerstablec.rds'),
  #   # safely_readrds)
  #
  # output$whocanwinplotc <- renderPlot({
  #   winnerstablec() |>
  #     gf_col(winner ~ p, fill = "steelblue") |>
  #     gf_labs(x = "percent of scenarios that win") |>
  #     gf_refine(scale_x_continuous(labels = scales::label_percent()))
  # })
  #
  #
  # h2hc <- reactive({
  #   n <- nrow(EM())
  #   psc <- possiblescoresc()
  #   denom <- ncol(psc)
  #   res <-
  #     outer(1:n, 1:n, vectorize(function(r, c) {
  #       sum(psc[r,] > psc[c, ])
  #     })
  #     )
  #
  #   rownames(res) <- attr(EM(), "name")
  #   colnames(res) <- attr(EM(), "name")
  #
  #   res |>
  #     as.table() |> as.data.frame() |>
  #     setnames(c('key', 'other', 'scenarios')) |>
  #     mutate(
  #       prop = scenarios / denom,
  #       key_name = attr(EM(), 'name')[key],
  #       other_name = attr(EM(), 'name')[other],
  #       key_abbrv = abbreviate(key_name, 6),
  #       other_abbrv = abbreviate(other_name, 6)
  #     ) |>
  #     mutate(
  #       key_name = reorder(key_name, scenarios),
  #       key_abbrv = reorder(key_abbrv, scenarios),
  #       other_name = reorder(other_name, scenarios, function(x) - mean(x)),
  #       other_abbrv = reorder(other_abbrv, scenarios, function(x) - mean(x))
  #     )
  # })
  #
  # output$h2hplotc <- renderPlotly({
  #   h2hc() |>
  #     mutate(
  #       perc = round(100 * prop, 2),
  #       hovertext =
  #         glue::glue('{key_name}<br>defeats<br>{other_name}<br>in {scenarios} scenarios.<br>({perc} %)')
  #     ) |>
  #     mutate(scenarios = ifelse(max(scenarios) > 1 & scenarios <= 0, na, scenarios)) |>
  #     gf_raster(scenarios ~ other_abbrv + key_abbrv, alpha = 0.8, text = ~hovertext) |>
  #     gf_hline(yintercept = 0.5 + (0:nrow(EM())), color = "gray80", inherit = FALSE, size = 0.5) |>
  #     gf_vline(xintercept = 0.5 + (0:nrow(EM())), color = "gray80", inherit = FALSE, size = 0.5) |>
  #     gf_labs(title = "head to head winning scenarios",
  #             subtitle = "read across rows for wins against the other player",
  #             x = "", y = "", fill = "winning\nscenarios" ) |>
  #     gf_refine(
  #       scale_fill_steps(low = "white", high = "steelblue", n.breaks = 8),
  #       coord_cartesian(expand = FALSE)
  #     ) |>
  #     gf_theme(
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.major.y = element_blank(),
  #       axis.text.x = element_text(angle = 45, hjust = 1),
  #       panel.background = element_rect(fill = rgb(1,0,0, alpha = 0.2))
  #     ) |>
  #     plotly::ggplotly(tooltip = "text")
  # })

  ############ select teams ###########

  TeamsM <- reactive(c( input$regionm1, input$regionm2, input$regionm3, input$regionm4 ))
  TeamsW <- reactive(c( input$regionw1, input$regionw2, input$regionw3, input$regionw4 ))

  RegionsM <- reactive({unique(BracketM()$region)})
  RegionsW <- reactive({unique(BracketW()$region)})

  output$RegionNameM1 <- renderText({RegionsM()[1]})
  output$RegionNameM2 <- renderText({RegionsM()[2]})
  output$RegionNameM3 <- renderText({RegionsM()[3]})
  output$RegionNameM4 <- renderText({RegionsM()[4]})

  output$RegionNameW1 <- renderText({RegionsW()[1]})
  output$RegionNameW2 <- renderText({RegionsW()[2]})
  output$RegionNameW3 <- renderText({RegionsW()[3]})
  output$RegionNameW4 <- renderText({RegionsW()[4]})

  output$TeamsSelectorM1 <- renderUI({
    checkboxgroupinput("regionm1","",regionChoices(RegionsM()[1], BracketM()))
  })

  output$TeamsSelectorM2 <- renderUI({
    checkboxgroupinput("regionm2","",regionChoices(RegionsM()[2], BracketM()))
  })

  output$TeamsSelectorM3 <- renderUI({
    checkboxgroupinput("regionm3","",regionChoices(RegionsM()[3], BracketM()))
  })

  output$TeamsSelectorM4 <- renderUI({
    checkboxgroupinput("regionm4","",regionChoices(RegionsM()[4], BracketM()))
  })

  output$TeamsSelectorW1 <- renderUI({
    checkboxgroupinput("regionw1","",regionChoices(RegionsW()[1], BracketW()))
  })

  output$TeamsSelectorW2 <- renderUI({
    checkboxgroupinput("regionw2","",regionChoices(RegionsW()[2], BracketW()))
  })

  output$TeamsSelectorW3 <- renderUI({
    checkboxgroupinput("regionw3","",regionChoices(RegionsW()[3], BracketW()))
  })

  output$TeamsSelectorW4 <- renderUI({
    checkboxgroupinput("regionw4","",regionChoices(RegionsW()[4], BracketW()))
  })

  # timeoflastentry <- reactive( lasttimestamp )
  # entrystatus <- renderText( paste("most recent entry: ", timeoflastentry() ) )

  totalTeamsM <- reactive( length(TeamsM()) )
  totalTeamsW <- reactive( length(TeamsW()) )

  output$totalTeamsM <- renderText(paste("number of teams:",totalTeamsM()))
  output$totalTeamsW <- renderText(paste("number of teams:",totalTeamsW()))

  PointsSpentM <- reactive( sum( filter(BracketM(), team %in% TeamsM())$cost ) )
  PointsSpentW <- reactive( sum( filter(BracketW(), team %in% TeamsW())$cost ) )

  output$pointsSpentM <- reactive(PointsSpentM())
  output$pointsSpentW <- reactive(PointsSpentW())

  outputOptions(output, "pointsSpentM", suspendWhenHidden = FALSE)
  outputOptions(output, "pointsSpentW", suspendWhenHidden = FALSE)

  PointsRemainingM <- reactive( maxPoints - PointsSpentM() )
  PointsRemainingW <- reactive( maxPoints - PointsSpentW() )

  output$PointsRemainingM <- renderText(paste("points remaining:", PointsRemainingM()))
  output$PointsRemainingW <- renderText(paste("points remaining:", PointsRemainingW()))

  output$year <- renderText(paste(the_year()))

  output$manyTeamsM <- reactive(length(TeamsM()) > 3)
  output$manyTeamsW <- reactive(length(TeamsW()) > 3)

  output$spendmessagem<- renderText({
    if (PointsSpentM() < maxPoints) {
      "choose a team, you've got points to spend."
    } else if  (PointsSpentM() > maxPoints ) {
      "you have overspent.  please remove a team."
    } else {
      "you have spent all of your points."
    }
  })
  output$spendmessagew<- renderText({
    if (PointsSpentW() < maxPoints) {
      "choose a team, you've got points to spend."
    } else if  (PointsSpentW() > maxPoints ) {
      "you have overspent.  please remove a team."
    } else {
      "you have spent all of your points."
    }
  })

  output$confirmation <- renderUI( {
    if (input$submitbutton > 0) {
      lasttimestamp <<- Sys.time()
#      output$statusmessage <- renderText({paste("last entry submited at", lasttimestamp)})
      output$statusmessage <- renderText({paste("preparing to submit", lasttimestamp)})
      newentry <-
      isolate(
        list( name= input$name,
              email = input$email,
              dept = input$dept,
              points = PointsSpentM(),
              pointsw = PointsSpentW(),
              teams = TeamsM(),
              TeamsW = TeamsW(),
              teamslogical = sapply(BracketM()$team, function(x) x %in% TeamsM()),
              teamslogicalw = sapply(BracketW()$team, function(x) x %in% TeamsW()),
              time = lasttimestamp)
      )
      board |> pin_write_entry(newentry, year = config[['year']])

      # createlogentry(paste("entry submitted for", isolate(input$email)))
    }

    if (input$submitbutton < 1) {
      taglist(
        p("testing...")
      )
    } else {
      taglist(html(
        paste0("<h4>thank you, ", isolate(input$name), ", your selections have been submitted.</h4>",
               "<ul>",
               "<li>you spent ", isolate(PointsSpentM()), " points on the following ", isolate(length(TeamsM())),
               " teams: ", paste( isolate(TeamsM()), collapse=", " ),
               "</li><li>you spent ", isolate(PointsSpentW()), " points on the following ", isolate(length(TeamsW())),
               " teams: ", paste( isolate(TeamsW()), collapse=", " ),
               "</li></ul>"
        )
      ))
    }
  }
  )

  output$numEntries <- reactive( length(Entries()) )

  ############ download data ###########
  # todo: restore or ignore?
  # output$downloaddata <- downloadhandler(
  #   filename = function() { "entries.rds" },
  #   content = function(file) {
  #     mysaverds(Entries(), file)
  #   }
  # )

  # output$showdownloadbutton <- reactive({
  #   "download" %in% names(Query())
  # } )
  # outputOptions(output, "showdownloadbutton", suspendWhenHidden = FALSE)


  ########### turn controls on and off ############

  # todo: restore crystal ball

  # output$showcrystalballm <- reactive({
  #   n_teams_remaining(tm()) <= 16
  # })
  # output$showcrystalballw <- reactive({
  #   n_teams_remaining(tw()) <= 16
  # })

  output$showentryform <- reactive({
    ( as.numeric(input$submitbutton) + as.numeric(input$revisebutton) ) %% 2 == 0
  })
  outputOptions(output, "showentryform", suspendWhenHidden = FALSE)

  output$acceptingEntries <- reactive({
    AdminMode() || (Sys.time() < lubridate::ymd_hm(config[['deadline']]) + lubridate::hours(5))
      # (file.exists(bracketfile) &&
  })
  outputOptions(output, "acceptingEntries", suspendWhenHidden = FALSE)

  output$showadmintab <- reactive({
    AdminMode()
  })
  outputOptions(output, "showadmintab", suspendWhenHidden = FALSE)

  output$showgameentry <- reactive({
    tolower(as.character(input$passwd)) == "madly marching"
  })
  outputOptions(output, "showgameentry", suspendWhenHidden = FALSE)

  ## not working.. always yield FALSE?
  output$conteststandingsready <- reactive({
    nrow(ContestStandingsM()) >= 0 && nrow(ContestStandingsW()) >= 0
  })
  output$showstandingsm <- reactive({
    nrow(CompletedGamesM()) > 0 && nrow(EM()) > 0
  })
  output$showstandingsw <- reactive({
    nrow(CompletedGamesW()) > 0 && nrow(EM()) > 0
  })

  outputOptions(output, "showstandingsm", suspendWhenHidden = FALSE)
  outputOptions(output, "showstandingsw", suspendWhenHidden = FALSE)

  # todo: turn scoring back on
  output$showScoresM <- reactive({
    nrow(CompletedGamesM()) > 0
  } )
  output$showScoresW <- reactive({
    nrow(CompletedGamesW()) > 0
  } )
  outputOptions(output, "showScoresM", suspendWhenHidden = FALSE)
  outputOptions(output, "showScoresW", suspendWhenHidden = FALSE)

  ############ score updates #############

  # todo: turn scoring back on
  output$password <- renderPrint({input$password})

  output$gamescoreselectorm <- renderUI({
    gs <- all_games(tm(), GameScoresM())
    games <- gs[['game_number']] |> setnames(gs[['description']])
    print(games)

    selectinput("gametoscorem", "choose a game", choices = games, selectize=FALSE)
  })

  output$gamescoreselectorw <- renderUI({
    gs <- all_games(tw(), GameScoresW())
    games <- gs[['game_number']] |> setnames(gs[['description']])

    selectinput("gametoscorew", "choose a game", choices = games, selectize=FALSE)
  })

  # this block was already commented prior to 15 march 2023
  # output$gametoscoretextm <- renderText({
  #   paste0("about to give score for game ", input$gametoscorem, ": ",
  #          awayteam(as.numeric(input$gametoscorem), BracketM(), GameScoresM()), " vs. ",
  #          hometeam(as.numeric(input$gametoscorem), BracketM(), GameScoresM())
  #   )
  # })
  # output$gametoscoretextw <- renderText({
  #   paste0("about to give score for game ", input$gametoscorew, ": ",
  #          awayteam(as.numeric(input$gametoscorew), BracketW(), GameScoresW()), " vs. ",
  #          hometeam(as.numeric(input$gametoscorew), BracketW(), GameScoresW())
  #   )
  # })

  # todo: turn scoring back on
  output$hometeamscorem <- renderUI({
    tourn <- tm()
    numericinput("hscorem", step=0,
                 label = home_team_name(tourn, as.numeric(input$gametoscorem)),
                 value = ""
    #              value = GameScoresM() |>
    #                filter(game_number == as.numeric(input$gametoscoresm)) |>
    #                pull(hscore) |> c(na) |> getelement(1)
    )
  })
  output$hometeamscorew <- renderUI({
    tourn <- tw()
    numericinput("hscorew", step=0,
                 label = home_team_name(tourn, as.numeric(input$gametoscorew)),
                 value = ""
    )
  })

  output$awayteamscorem <- renderUI({
    numericinput("ascorem", step=0,
                 label = away_team_name(tm(), as.numeric(input$gametoscorem)),
                 value = ""
                 # value = GameScoresM() |>
                 #   filter(game_number == as.numeric(input$gametoscoresm)) |>
                 #   pull(ascore) |> c(na) |> getelement(1)
    )
  })
  output$awayteamscorew <- renderUI({
    numericinput("ascorew", step=0,
                 label = away_team_name(tw(), as.numeric(input$gametoscorew)),
                 value = ""
                 # value = GameScoresW() |>
                 #   filter(game_number == as.numeric(input$gametoscoresw)) |>
                 #   pull(ascore) |> c(na) |> getelement(1)
    )
  })

  output$scoresavedtextm <- renderText({
    # don't react to these until the button is pushed
    if(TRUE) {
      input$savescorebuttonm
      isolate(gts <- as.numeric(input$gametoscorem))
      isolate(hs <- as.numeric(input$hscorem))
      isolate(as <- as.numeric(input$ascorem))

      home <- home_team_name(tm(), gts)
      away <- away_team_name(tm(), gts)

      # this will react each time the save score button is pressed.
      if (as.numeric(input$savescorebuttonm) > 0)
        paste0("score saved for game ", gts, ": ", away, " ", as, " - ", home, " ", hs )
      else "select a game and enter scores above."
    } else {
      "sample text."
    }
  })
  output$scoresavedtextw <- renderText({
    # don't react to these until the button is pushed
    if(TRUE) {
      input$savescorebuttonw
      isolate(gts <- as.numeric(input$gametoscorew))
      isolate(hs <- as.numeric(input$hscorew))
      isolate(as <- as.numeric(input$ascorew))

      home <- home_team_name(tw(), gts)
      away <- away_team_name(tw(), gts)

      # this will react each time the save score button is pressed.
      if (as.numeric(input$savescorebuttonw) > 0)
        paste0("score saved for game ", gts, ": ", away, " ", as, " - ", home, " ", hs )
      else "select a game and enter scores above."
    } else {
      "sample text."
    }
  })

  # updates when a new score is saved (button)
  observeEvent( input$savescorebuttonm, {
    if (as.numeric(input$savescorebuttonm) > 0) {
      gts <- as.numeric(input$gametoscorem)
      hs <- as.numeric(input$hscorem)
      as <- as.numeric(input$ascorem)
      home <- home_team_name(tm(), gts)
      away <- away_team_name(tm(), gts)
      # createlogentry(paste("score enterred:", away, "vs.", home, as, "-", hs))
      # note for winner_01: 0 = home win; 1 = away win

      # # write score for one game
      # board |>
      #   my_pin_write(
      #     tibble(game_number = gts, winner_01 = as.numeric(as > hs),
      #            home = home, away = away, hscore = hs, ascore = as, timestamp = human_time()),
      #     name =  paste0("/m-", gsub("/"," or ", home), "-", gsub("/", " or ", away))
      # )

      # replacement for a write_csv() with apend = TRUE
      board |>
        my_pin_write(
          bind_rows(
            # grab pinned scores
            my_pin_read(board, name = config[['scores']][1]),
            # add new score
            tibble(game_number = gts, winner_01 = as.numeric(as > hs),
                 home = home, away = away, hscore = hs, ascore = as, timestamp = human_time())
          ),
          name = config[['scores']][1]
        )
    }

    if (as.numeric(input$savescorebuttonm) > 0 &&
        AdminMode() &&
        n_games_remaining(tm()) <= 16) {
      # cachecrystalballm() # todo: turn on crystal ball
    }
  })

  observeEvent( input$savescorebuttonw, {
    if (as.numeric(input$savescorebuttonw) > 0) {
      gts <- as.numeric(input$gametoscorew)
      hs <- as.numeric(input$hscorew)
      as <- as.numeric(input$ascorew)
      home <- home_team_name(tw(), gts)
      away <- away_team_name(tw(), gts)
      # createlogentry(paste("women's score enterred:", away, "vs.", home, as, "-", hs))
      # note for winner_01: 0 = home win; 1 = away win

      # # write score for one game
      # board |>
      #   my_pin_write(
      #     tibble(game_number = gts, winner_01 = as.numeric(as > hs),
      #            home = home, away = away, hscore = hs, ascore = as, timestamp = human_time()),
      #     name =  paste0("/m-", gsub("/"," or ", home), "-", gsub("/", " or ", away))
      # )

      # replacement for a write_csv() with apend = TRUE
      board |>
        my_pin_write(
          bind_rows(
            # grab pinned scores
            my_pin_read(board, name = config[['scores']][2]),
            # add new score
            tibble(game_number = gts, winner_01 = as.numeric(as > hs),
                 home = home, away = away, hscore = hs, ascore = as, timestamp = human_time())
          ),
          name = config[['scores']][2]
        )
    }
    if (as.numeric(input$savescorebuttonw) > 0 &&
        AdminMode() &&
        n_games_remaining(tw()) <= 16) {
      # cachecrystalballw()  # todo: turn crystal ball back on
    }
  })

  ########## bracket ###############

  # already commented out prior to 15 march 2023
  # # updates when games scores change
  # BracketWithteamstatusm <- reactive({
  #   addteamstatus(BracketM(), scheduledgames(bracket=BracketM(), results = GameScoresM()))
  # })
  # BracketWithteamstatusw <- reactive({
  #   addteamstatus(BracketW(), scheduledgames(bracket=BracketW(), results = GameScoresW()))
  # })


  ############ query stuff #############
  output$queryText <- renderText({
    # return a string with key-value pairs
    paste(names(Query()), Query(), sep = "=", collapse=", ")
  })

  ########################################
  # outputs
  ########################################

  ########## instructions/background info

  output$CostTable1 <- renderDataTable(
    options=list(lengthChange=0,                      # show/hide records per page dropdown
                 searching=0,                         # global search box on/off
                 info=0,                              # information on/off (how many records filtered, etc)
                 paging=FALSE,
                 ordering = FALSE,
                 autoWidth=TRUE                       # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),  {
    SeedTable <-
      BracketM() %>% group_by(seed) %>%
      summarise(cost = max(cost), `pre-2019 cost` = max(cost.old)) %>%
      arrange(seed)
    SeedTable %>% head(nrow(SeedTable) / 2)
  })

  output$CostTable2 <- renderDataTable(
    options=list(lengthChange=0,                      # show/hide records per page dropdown
                 searching=0,                         # global search box on/off
                 info=0,                              # information on/off (how many records filtered, etc)
                 paging=FALSE,
                 ordering = FALSE,
                 autoWidth=TRUE                       # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),  {
    SeedTable <-
      BracketM() %>% group_by(seed) %>%
      summarise(cost = max(cost), `pre-2019 cost` = max(cost.old)) %>%
      arrange(seed)
    SeedTable %>% tail(-nrow(SeedTable) / 2)
  })

  output$pastwinners <- renderDataTable(
    options=list(lengthChange=0,                      # show/hide records per page dropdown
                 searching=0,                         # global search box on/off
                 info=0,                              # information on/off (how many records filtered, etc)
                 ordering = FALSE,
                 paging=FALSE,
                 autoWidth=TRUE                       # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),  {
    History <- readr::read_csv("data/historical-winners.csv") # , header=TRUE)
    History %>% arrange(year, winner)
  })

  ############# game scores table #####################

  # todo: turn scoring back on
  output$scorestablem <- renderDataTable(
    options=list(pagelength = 63,                     # initial number of records
                 lengthmenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 ordering = FALSE,
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),  {
      CompletedGamesM()
    })
  output$scorestablew <- renderDataTable(
    options=list(pagelength = 63,                     # initial number of records
                 lengthmenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 ordering = FALSE,
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),  {
      CompletedGamesW()
    })

  ############# standings table #####################

  # todo: turn standing back on
  output$standingstablem <- renderDataTable(
    options=list(pagelength = 35,                     # initial number of records
                 lengthmenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ContestStandingsM()
    })

  output$standingstablew <- renderDataTable(
    options=list(pagelength = 35,                     # initial number of records
                 lengthmenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ContestStandingsW()
    })

  output$standingstableall <- renderDataTable(
    options=list(pagelength = 35,                     # initial number of records
                 lengthmenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ContestStandingsAll()
    })

  ######### reactive text messages #########
  output$tournystatusm <- renderText({
    games <- nrow(CompletedGamesM()) # sum(BracketWithteamstatus()$wins, na.rm=TRUE)
    paste0("data based on ", nrow(EM()), " contestants and ", games, " games.")
  })
  output$tournystatusw <- renderText({
    games <- nrow(CompletedGamesW()) # sum(BracketWithteamstatus()$wins, na.rm=TRUE)
    paste0("data based on ", nrow(EW()), " contestants and ", games, " games.")
  })

  ######### (basketball) team data table #########
  # commented out prior to 15 march 2023
  # teamdatam <- reactive({
  #   teamdata(Entries(), BracketWithteamstatusm())
  # })
  #
  # teamdataw <- reactive({
  #   teamdata(Entries(), BracketWithteamstatusw())
  # })

  # output$teamdata <- renderDataTable(
  #   options=list(pagelength = 64,             # initial number of records
  #                lengthmenu = c(5,10,25,50),  # records/page options
  #                lengthChange = 0,            # show/hide records per page dropdown
  #                searching = 1,               # global search box on/off
  #                info = 1,                    # information on/off (how many records filtered, etc)
  #                autoWidth = 1                # automatic column width calculation, disable if passing column width via aoColumnDefs
  #                #aoColumnDefs = list(list(swidth="300px", atargets=c(list(0),list(1))))    # custom column size
  #   ),
  #   teamdatam()
  # )
      # e <- Entries()
      # m <- do.call(rbind, lapply( e, function(x) x$teamslogical ) )
      # b <- BracketWithteamstatus()
      # if (length(e) > 0) {
      #   row.names(m) <- sapply( e, function(x) x$name)
      # }
      #
      # data.frame(check.names=FALSE,
      #            team=colnames(m),
      #            seed = b$seed, region=b$region,
      #            "number of players selecting"=apply(m,2,sum),
      #            wins = b$wins)


  # todo: more crystal ball stuff?
  # output$entrantselector <- renderUI({
  #   entrants <- sapply(Entries(), function(x) x$email)
  #   names(entrants) <-
  #     paste0(
  #       sapply(Entries(), function(x) x$name),
  #       " [",
  #       conteststandings()$score,
  #       "]"
  #     )
  #   selectinput("oneentrant", "select a player", choices = entrants[order(- conteststandings()$score)], selectize=FALSE)
  # })
  #
  #
  #
  # output$whocanwinplotm <- renderPlot({
  #   winnerstablem() |>
  #     gf_col(winner ~ p, fill = "steelblue") |>
  #     gf_labs(x = "percent of scenarios that win") |>
  #     gf_refine(scale_x_continuous(labels = scales::label_percent()))
  # })
  #
  #
  # output$scorehistogramsm <-
  #   renderPlot(height = 600,
  #     {
  #     possiblescorestablem() |>
  #       gf_col(n ~ round(score) | reorder(name, score, function(x) - mean (x)),
  #              binwidth = 1, fill = "steelblue") |>
  #         gf_labs(x = "score")
  #   })
  #
  # textmat <- function(m, denom) {
  #   rn <- rownames(m) |> matrix(nrow = nrow(m), ncol = ncol(m), byrow = FALSE)
  #   cn <- colnames(m) |> matrix(nrow = nrow(m), ncol = ncol(m), byrow = TRUE)
  #   paste0(
  #     rn , " defeats " , cn , "</br>in " , m , " scenarios</br>",
  #     "(" , round(100 * m / denom,2) , "%)") |>
  #     matrix(nrow = nrow(m))
  # }
  #
  # output$h2hplotm <- renderPlotly({
  #   h2hm() |>
  #     mutate(
  #       perc = round(100 * prop, 2),
  #       hovertext =
  #         glue::glue('{key_name}<br>defeats<br>{other_name}<br>in {scenarios} scenarios.<br>({perc} %)')
  #     ) |>
  #     mutate(scenarios = ifelse(max(scenarios) > 1 & scenarios <= 0, na, scenarios)) |>
  #     gf_raster(scenarios ~ other_abbrv + key_abbrv, alpha = 0.8,
  #             text = ~hovertext) |>
  #     gf_hline(yintercept = 0.5 + (0:nrow(EM())), color = "gray80", inherit = FALSE, size = 0.5) |>
  #     gf_vline(xintercept = 0.5 + (0:nrow(EM())), color = "gray80", inherit = FALSE, size = 0.5) |>
  #     gf_labs(title = "head to head winning scenarios",
  #             subtitle = "read across rows for wins against the other player",
  #             x = "", y = "", fill = "winning\nscenarios" ) |>
  #     gf_refine(
  #       scale_fill_steps(low = "white", high = "steelblue", n.breaks = 8),
  #       coord_cartesian(expand = FALSE)
  #     ) |>
  #     gf_theme(
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.major.y = element_blank(),
  #       axis.text.x = element_text(angle = 45, hjust = 1),
  #       panel.background = element_rect(fill = rgb(1,0,0, alpha = 0.2))
  #     ) |>
  #     plotly::ggplotly(tooltip = "text")
  # })
  #
  # output$whocanwinplotw <- renderPlot({
  #   winnerstablew() |>
  #     gf_col(winner ~ p, fill = "steelblue") |>
  #     gf_labs(x = "percent of scenarios that win") |>
  #     gf_refine(scale_x_continuous(labels = scales::label_percent()))
  # })
  #
  # output$scorehistogramsw <-
  #   renderPlot(height = 600,
  #              {
  #                possiblescorestablew() |>
  #                  gf_col(n ~ round(score) | reorder(name, score, function(x) - mean (x)),
  #                         binwidth = 1, fill = "steelblue") |>
  #                  gf_labs(x = "score")
  #              })
  #
  # output$h2hplotw <- renderPlotly({
  #   h2hw() |>
  #     mutate(
  #       perc = round(100 * prop, 2),
  #       hovertext =
  #         glue::glue('{key_name}<br>defeats<br>{other_name}<br>in {scenarios} scenarios.<br>({perc} %)')
  #     ) |>
  #     mutate(scenarios = ifelse(max(scenarios) > 1 & scenarios <= 0, na, scenarios)) |>
  #     gf_raster(scenarios ~ other_abbrv + key_abbrv, alpha = 0.8,
  #             text = ~hovertext) |>
  #     gf_hline(yintercept = 0.5 + (0:nrow(EW())), color = "gray80", inherit = FALSE, size = 0.5) |>
  #     gf_vline(xintercept = 0.5 + (0:nrow(EW())), color = "gray80", inherit = FALSE, size = 0.5) |>
  #     gf_labs(title = "head to head winning scenarios",
  #             subtitle = "read across rows for wins against the other player",
  #             x = "", y = "", fill = "winning\nscenarios" ) |>
  #     gf_refine(
  #       coord_cartesian(expand = FALSE),
  #       scale_fill_steps(low = "white", high = "steelblue", n.breaks = 8)
  #     ) |>
  #     gf_theme(
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.major.y = element_blank(),
  #       axis.text.x = element_text(angle = 45, hjust = 1),
  #       panel.background = element_rect(fill = rgb(1,0,0, alpha = 0.2))
  #     ) |>
  #     plotly::ggplotly(tooltip = "text")
  # })


  # commented out prior to 15 march 2023
  # output$dendroplot <- renderd3heatmap({
  #   e <- Entries()
  #   m <- do.call(rbind, lapply(e, function(x) x$teamslogical))
  #   rownames(m) <- sapply(e, function(x) x$name)
  #   d <- as.data.frame(m)
  #   d <- as.data.frame(lapply(d, function(x) as.numeric(x)))
  #   rownames(d) <- rownames(m)
  #   rowv  <- d %>% dist(method = "euclidean") %>% hclust %>% as.dendrogram %>%
  #     set("branches_k_color", k = 5) %>% set("branches_lwd", 2) %>%
  #     ladderize()
  #   colv  <- d %>% t %>% dist(method = "euclidean") %>% hclust %>% as.dendrogram %>%
  #     set("branches_k_color", k = 7) %>% set("branches_lwd", 2) %>%
  #     ladderize()
  #   d3heatmap(d, rowv = rowv, colv = colv, color="blues")
  # })

  # todo: restore options as reactives come back online
  outputOptions(output, "scorestablem", suspendWhenHidden = FALSE)
  outputOptions(output, "scorestablew", suspendWhenHidden = FALSE)
  # outputOptions(output, "standingstablem", suspendWhenHidden = FALSE, priority = 100)
  # outputOptions(output, "standingstablew", suspendWhenHidden = FALSE, priority = 101)
  # outputOptions(output, "standingstableall", suspendWhenHidden = FALSE, priority = 90)
  # outputOptions(output, "whocanwinplotm", suspendWhenHidden = FALSE, priority = 80)
  # outputOptions(output, "scorehistogramsm", suspendWhenHidden = FALSE, priority = 80)
  # outputOptions(output, "whocanwinplotm", suspendWhenHidden = FALSE, priority = 50)
  # outputOptions(output, "whocanwinplotw", suspendWhenHidden = FALSE, priority = 50)
  # outputOptions(output, "h2hplotm", suspendWhenHidden = FALSE, priority = 40)
  # outputOptions(output, "h2hplotw", suspendWhenHidden = FALSE, priority = 40)
  # outputOptions(output, "scorehistogramsm", suspendWhenHidden = FALSE, priority = 30)
  # outputOptions(output, "scorehistogramsw", suspendWhenHidden = FALSE, priority = 30)

  Sys.sleep(1)
  waiter::waiter_hide()

})
