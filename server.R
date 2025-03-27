
# this is the server logic for a shiny web application.
# you can find out more about building applications with shiny here:
#
# http://www.rstudio.com/shiny/

library(shiny)
library(dplyr)
library(pins)
library(vegabrite)

yaml_file <- yaml::read_yaml('ncaa.yml')$yaml
config <- yaml::read_yaml(yaml_file)

board <-
  board_connect(server = "https://connect.cs.calvin.edu",
                key = "S2RwoRs5VIkMPCyELvj0mixL6QC4Emx1")

# board |> pin_search()
# library(madness)
source('package/R/data_utils.R')
source('package/R/matrix_and_vector.R')
source('package/R/scenarios.R')

library(waiter)
library(ggformula)
library(plotly)
theme_set(theme_bw())
# library(reactlog)
# reactlog_enable()


clean_name <- function(name) {
  paste0('rpruim/NCAA-', config[['year']], '-', name) |>
    stringr::str_replace_all('[@.]', '_')
}

my_pin_write <- function(object, name, board, ...) {
  clean_name <- clean_name(name)
  print(clean_name)
  pin_write(board, object, clean_name, ...)
}

my_pin_read <- function(name, board, default) {
  clean_name <- clean_name(name)
  print(clean_name)
  if (pin_exists(board, clean_name)) {
    pin_read(board, name = clean_name)
  } else {
    default
  }
}

my_pin_reactive_read <- function(board, name, default, interval = 60000){
  clean_name <- clean_name(name)
  print(clean_name)
  if (!pin_exists(board, clean_name)) {
    cat(paste0("pin (", name, ") didn't exist, creating with default. "))
    pin_write(board, x = default, name = clean_name)
  }
  pin_reactive_read(board = board, name = clean_name, interval = interval)
}

maxPoints <- 200

human_time <- function() format(Sys.time(), "%y%m%d-%h%m%os")


### some utility functions

sessionid <- function(session) {
  digest::digest(session$request)
}

the_year <- function() {
  return(config[['year']])

  ## perhaps allow this to be read as part of URL instead
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

  Query <- reactive( parseQueryString(session$clientData$url_search) )

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
      function() pin_search(board, search = paste0("NCAA-", config[['year']], "-entry")) |> pull(created) |> max(),
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

  empty_scores_df <-
    tibble(
      game_number = integer(0),
      winner_01 = integer(0),
      home = character(0),
      away = character(0),
      hscore = integer(0),
      ascore = integer(0)
      # timestamp = character(0)
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
    build_entry_matrix(Entries(), ext = "M")
    })
  EW <- reactive({
    build_entry_matrix(Entries(), ext = "W")
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


  if (TRUE) {
  # todo: deal with crystal ball
  cacheCrystalBallM <- function() {
    tc <- tournament_completions(TM(), max_games_remaining = 15)
    tc |>
      my_pin_write(name = 'TCM', board = board)

    h2h <- head2head(TM(), EM() , tc, result = "data.frame")
    h2h |>
      my_pin_write(name = "H2HM", board = board)

    ps <- tc |>
      apply(2, function(x, e = EM()) { contest_scores(x, e)} )
    ps |> round(12) |>
      my_pin_write(name = 'PossibleScoresM', board = board)

    ps |>
      apply(2, which.max) %>%
      tibble(winner = .) |>
      group_by(winner) |>
      summarise(scenarios = n()) |>
      mutate(
        winner = rownames(EM())[winner],
        p = scenarios / sum(scenarios)
      ) |>
      mutate(
        winner = reorder(winner, scenarios)
      ) |>
      my_pin_write(name = "WinnersTableM", board = board)
  }


  TCM <-
    my_pin_reactive_read(board, name = 'TCM', default = matrix(numeric(0), nrow = 0))

  h2h_default <-
    tibble(
      key = factor(integer(0)),
      other = factor(integer(0)),
      scenarios = integer(0),
      key_name = factor(character(0)),
      other_name = factor(character(0)),
      prop = numeric(0),
      key_abbrev = factor(character(0)),
      other_abbrev = factor(character(0))
    )

  H2HM <-
    my_pin_reactive_read(board, name = 'H2HM', default = h2h_default)

  possible_scores_default <-
    isolate(matrix(numeric(0), nrow = nrow(EM())))

  PossibleScoresM <-
    my_pin_reactive_read(board, name = 'PossibleScoresM', default = possible_scores_default)

  winners_table_default <- tibble(
    winner = factor(character(0)),
    scenarios = integer(0),
    p = numeric(0)
  )

  WinnersTableM <-
    my_pin_reactive_read(board, name = "WinnersTableM", default = winners_table_default)

  PossibleScoresTableM <- reactive({
    PossibleScoresM() |>
      as.table() |>
      as.data.frame() |>
      setNames(c('name', 'sceneario', 'score')) |>
      group_by(name, score) |>
      tally()
  })

  cacheCrystalBallW <- function() {
    tc <- tournament_completions(TW(), max_games_remaining = 15)
    tc |> my_pin_write(board, name = 'TCW')

    h2h <- head2head(TW(), EW(), tc, result = "data.frame")
    h2h |>
      my_pin_write(name = 'H2HW', board = board)

    ps <- tc |>
      apply(2, function(x, e = EW()) { contest_scores(x, e)} )
    ps |> round(12) |>
      my_pin_write(name = 'PossibleScoresW', board = board)

    ps |>
      apply(2, which.max) %>%
      tibble(winner = .) |>
      group_by(winner) |>
      summarise(scenarios = n()) |>
      mutate(
        winner = rownames(EW())[winner],
        p = scenarios / sum(scenarios)
      ) |>
      mutate(
        winner = reorder(winner, scenarios)
      ) |>
      my_pin_write(name = 'WinnersTableW', board = board)
  }


  TCW <-
    my_pin_reactive_read(board, name = 'TCW', default = matrix())

  H2HW <-
    my_pin_reactive_read(board, name = 'H2HW', default = tibble())

  PossibleScoresW <-
    my_pin_reactive_read(board, name = 'PossibleScoresW', default = matrix())

  WinnersTableW <-
    my_pin_reactive_read(board, name = 'WinnersTableW', default = tibble())

  PossibleScoresTableW <- reactive({
    PossibleScoresW() |>
      as.table() |>
      as.data.frame() |>
      setNames(c('name', 'sceneario', 'score')) |>
      group_by(name, score) |>
      tally()
  })

  observeEvent(
    input$reCacheButton,
    {
      if (as.numeric(input$reCacheButton) > 0 && AdminMode()) {
        if (n_games_remaining(TW()) <= 15) {
          cacheCrystalBallW()
        }
        if (n_games_remaining(TM()) <= 15) {
          cacheCrystalBallM()
        }
        if (n_games_remaining(TM()) + n_games_remaining(TW()) <= 15) {
          cacheCrystalBallC()
        }
      }
    })

  PossibleScoresC <-
    my_pin_reactive_read(board, name = 'PossibleScoresC', default = matrix())

  cacheCrystalBallC <- function() {
    psm <- PossibleScoresM()
    psw <- PossibleScoresW()
    denom <- ncol(psm) * ncol(psw)
    n <- nrow(EM())
    ps <-
      sapply(1:n,
             function(x) {
               outer(psm[x, ], psw[x, ], "+")
             }
      ) |> t()
    if (nrow(ps) != n) {ps <- t(ps)}
    ps |> round(12) |>
      my_pin_write(name = 'PossibleScoresC', board = board)
    ps |>
      apply(2, which.max) %>%
      tibble(winner = .) |>
      group_by(winner) |>
      summarise(scenarios = n()) |>
      mutate(
        winner = rownames(EM())[winner],
        p = scenarios / sum(scenarios)
      ) |>
      mutate(
        winner = reorder(winner, scenarios)
      ) |>
      my_pin_write(name = 'WinnersTableC', board = board)
  }

  PossibleScoresTableC <- reactive({
    PossibleScoresC() |>
      as.table() |>
      as.data.frame() |>
      setNames(c('name', 'sceneario', 'score')) |>
      group_by(name, score) |>
      tally()
  })

  WinnersTableC <-
    my_pin_reactive_read(name = 'WinnersTableC', board = board, default = tibble())

output$WhoCanWinPlotC <- renderPlot({
  WinnersTableC() |>
    gf_col(winner ~ p, fill = "steelblue") |>
    gf_labs(x = "percent of scenarios that win") |>
    gf_refine(scale_x_continuous(labels = scales::label_percent()))
})


H2HC <- reactive({
  n <- nrow(EM())
  psc <- PossibleScoresC()
  denom <- ncol(psc)
  res <-
    outer(1:n, 1:n, Vectorize(function(r, c) {
      sum(psc[r,] > psc[c, ])
    })
    )

  rownames(res) <- attr(EM(), "name")
  colnames(res) <- attr(EM(), "name")

  res |>
    as.table() |> as.data.frame() |>
    setNames(c('key', 'other', 'scenarios')) |>
    mutate(
      prop = scenarios / denom,
      key_name = attr(EM(), 'name')[key],
      other_name = attr(EM(), 'name')[other],
      key_abbrv = abbreviate(key_name, 6),
      other_abbrv = abbreviate(other_name, 6)
    ) |>
    mutate(
      key_name = reorder(key_name, scenarios),
      key_abbrv = reorder(key_abbrv, scenarios),
      other_name = reorder(other_name, scenarios, function(x) - mean(x)),
      other_abbrv = reorder(other_abbrv, scenarios, function(x) - mean(x))
    )
})

output$H2HPlotC <- renderPlotly({
  H2HC() |>
    mutate(
      perc = round(100 * prop, 2),
      hovertext =
        glue::glue('{key_name}<br>defeats<br>{other_name}<br>in {scenarios} scenarios.<br>({perc} %)')
    ) |>
    mutate(scenarios = ifelse(max(scenarios) > 1 & scenarios <= 0, NA, scenarios)) |>
    gf_raster(scenarios ~ other_abbrv + key_abbrv, alpha = 0.8, text = ~hovertext) |>
    gf_hline(yintercept = 0.5 + (0:nrow(EM())), color = "gray80", inherit = FALSE, linewidth = 0.5) |>
    gf_vline(xintercept = 0.5 + (0:nrow(EM())), color = "gray80", inherit = FALSE, linewidth = 0.5) |>
    gf_labs(title = "head to head winning scenarios",
            subtitle = "read across rows for wins against the other player",
            x = "", y = "", fill = "winning\nscenarios" ) |>
    gf_refine(
      scale_fill_steps(low = "white", high = "steelblue", n.breaks = 8),
      coord_cartesian(expand = FALSE)
    ) |>
    gf_theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.background = element_rect(fill = rgb(1,0,0, alpha = 0.2))
    ) |>
    plotly::ggplotly(tooltip = "text")
})

} # turn off stuff that doesn't work until second week.

  ############ select teams ###########

  TeamsM <- reactive(c( input$regionM1, input$regionM2, input$regionM3, input$regionM4 ))
  TeamsW <- reactive(c( input$regionW1, input$regionW2, input$regionW3, input$regionW4 ))

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
    checkboxGroupInput("regionM1","",regionChoices(RegionsM()[1], BracketM()))
  })

  output$TeamsSelectorM2 <- renderUI({
    checkboxGroupInput("regionM2","",regionChoices(RegionsM()[2], BracketM()))
  })

  output$TeamsSelectorM3 <- renderUI({
    checkboxGroupInput("regionM3","",regionChoices(RegionsM()[3], BracketM()))
  })

  output$TeamsSelectorM4 <- renderUI({
    checkboxGroupInput("regionM4","",regionChoices(RegionsM()[4], BracketM()))
  })

  output$TeamsSelectorW1 <- renderUI({
    checkboxGroupInput("regionW1","",regionChoices(RegionsW()[1], BracketW()))
  })

  output$TeamsSelectorW2 <- renderUI({
    checkboxGroupInput("regionW2","",regionChoices(RegionsW()[2], BracketW()))
  })

  output$TeamsSelectorW3 <- renderUI({
    checkboxGroupInput("regionW3","",regionChoices(RegionsW()[3], BracketW()))
  })

  output$TeamsSelectorW4 <- renderUI({
    checkboxGroupInput("regionW4","",regionChoices(RegionsW()[4], BracketW()))
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

  output$pointsRemainingM <- renderText(paste("points remaining:", PointsRemainingM()))
  output$pointsRemainingW <- renderText(paste("points remaining:", PointsRemainingW()))

  output$year <- renderText(paste(the_year()))

  output$manyTeamsM <- reactive(length(TeamsM()) > 3)
  output$manyTeamsW <- reactive(length(TeamsW()) > 3)

  output$spendMessageM <- renderText({
    if (PointsSpentM() < maxPoints) {
      "choose a team, you've got points to spend."
    } else if  (PointsSpentM() > maxPoints ) {
      "you have overspent.  please remove a team."
    } else {
      "you have spent all of your points."
    }
  })
  output$spendMessageW <- renderText({
    if (PointsSpentW() < maxPoints) {
      "choose a team, you've got points to spend."
    } else if  (PointsSpentW() > maxPoints ) {
      "you have overspent.  please remove a team."
    } else {
      "you have spent all of your points."
    }
  })

  output$confirmation <- renderUI( {
    if (input$submitButton > 0) {
      lasttimestamp <<- Sys.time()
#      output$statusMessage <- renderText({paste("last entry submited at", lasttimestamp)})
      output$statusMessage <- renderText({paste("preparing to submit", lasttimestamp)})
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

    if (input$submitButton < 1) {
      taglist(
        p("testing...")
      )
    } else {
      tagList(HTML(
        paste0("<h4>Thank you, ", isolate(input$name), ", your selections have been submitted.</h4>",
               "<ul>",
               "<li>You spent ", isolate(PointsSpentM()), " points on the following ", isolate(length(TeamsM())),
               " teams: ", paste( isolate(TeamsM()), collapse=", " ),
               "</li><li>You spent ", isolate(PointsSpentW()), " points on the following ", isolate(length(TeamsW())),
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
  # output$downloadData <- downloadhandler(
  #   filename = function() { "entries.rds" },
  #   content = function(file) {
  #     mysaveRDS(Entries(), file)
  #   }
  # )

  # output$showDownloadButton <- reactive({
  #   "download" %in% names(Query())
  # } )
  # outputOptions(output, "showDownloadButton", suspendWhenHidden = FALSE)


  ########### turn controls on and off ############

  # todo: restore crystal ball

  output$showCrystalBallM <- reactive({
    print(n_teams_remaining(TM()))
    n_teams_remaining(TM()) <= 16
    TRUE
  })
  output$showCrystalBallW <- reactive({
    print(n_teams_remaining(TW()))
    n_teams_remaining(TW()) <= 16
    TRUE
  })
  output$showCrystalBallC <- reactive({
    ( n_teams_remaining(TW()) + n_teams_remaining(TM()) )<= 16
    FALSE
  })

  output$showEntryForm <- reactive({
    ( as.numeric(input$submitButton) + as.numeric(input$reviseButton) ) %% 2 == 0
  })
  outputOptions(output, "showEntryForm", suspendWhenHidden = FALSE)

  output$acceptingEntries <- reactive({
    # TODO: fix this time-based check
    (Sys.time() < lubridate::ymd_hm(config[['deadline']]) + lubridate::hours(5))
    # TRUE
    # FALSE
    # AdminMode() || (Sys.time() < lubridate::ymd_hm(config[['deadline']]) + lubridate::hours(5))
  })
  outputOptions(output, "acceptingEntries", suspendWhenHidden = FALSE)

  output$showAdminTab <- reactive({
    AdminMode()
  })
  outputOptions(output, "showAdminTab", suspendWhenHidden = FALSE)

  output$showGameEntry <- reactive({
    tolower(as.character(input$passwd)) == "madly marching"
  })
  outputOptions(output, "showGameEntry", suspendWhenHidden = FALSE)

  ## not working.. always yield FALSE?
  output$contestStandingsReady <- reactive({
    nrow(ContestStandingsM()) >= 0 && nrow(ContestStandingsW()) >= 0
  })
  output$showStandingsM <- reactive({
    nrow(CompletedGamesM()) > 0 && nrow(EM()) > 0
  })
  output$showStandingsW <- reactive({
    nrow(CompletedGamesW()) > 0 && nrow(EM()) > 0
  })

  outputOptions(output, "showStandingsM", suspendWhenHidden = FALSE)
  outputOptions(output, "showStandingsW", suspendWhenHidden = FALSE)

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

  output$gameScoreSelectorM <- renderUI({
    gs <- all_games(TM(), GameScoresM())
    games <- gs[['game_number']] |> setNames(gs[['description']])
    # print(games)

    selectInput("gameToScoreM", "choose a game", choices = games, selectize=FALSE)
  })

  output$gameScoreSelectorW <- renderUI({
    gs <- all_games(TW(), GameScoresW())
    games <- gs[['game_number']] |> setNames(gs[['description']])

    selectInput("gameToScoreW", "choose a game", choices = games, selectize=FALSE)
  })

  # this block was already commented prior to 15 march 2023
  # output$gameToScoreTextM <- renderText({
  #   paste0("about to give score for game ", input$gameToScoreM, ": ",
  #          awayTeam(as.numeric(input$gameToScoreM), BracketM(), GameScoresM()), " vs. ",
  #          homeTeam(as.numeric(input$gameToScoreM), BracketM(), GameScoresM())
  #   )
  # })
  # output$gameToScoreTextW <- renderText({
  #   paste0("about to give score for game ", input$gameToScoreW, ": ",
  #          awayTeam(as.numeric(input$gameToScoreW), BracketW(), GameScoresW()), " vs. ",
  #          homeTeam(as.numeric(input$gameToScoreW), BracketW(), GameScoresW())
  #   )
  # })

  # todo: turn scoring back on
  output$homeTeamScoreM <- renderUI({
    tourn <- TM()
    numericInput("hscoreM", step=0,
                 label = home_team_name(tourn, as.numeric(input$gameToScoreM)),
                 value = ""
    #              value = GameScoresM() |>
    #                filter(game_number == as.numeric(input$gameToScoresM)) |>
    #                pull(hscore) |> c(na) |> getelement(1)
    )
  })
  output$homeTeamScoreW <- renderUI({
    tourn <- TW()
    numericInput("hscoreW", step=0,
                 label = home_team_name(tourn, as.numeric(input$gameToScoreW)),
                 value = ""
    )
  })

  output$awayTeamScoreM <- renderUI({
    numericInput("ascoreM", step=0,
                 label = away_team_name(TM(), as.numeric(input$gameToScoreM)),
                 value = ""
                 # value = GameScoresM() |>
                 #   filter(game_number == as.numeric(input$gameToScoresM)) |>
                 #   pull(ascore) |> c(na) |> getelement(1)
    )
  })
  output$awayTeamScoreW <- renderUI({
    numericInput("ascoreW", step=0,
                 label = away_team_name(TW(), as.numeric(input$gameToScoreW)),
                 value = ""
                 # value = GameScoresW() |>
                 #   filter(game_number == as.numeric(input$gameToScoresW)) |>
                 #   pull(ascore) |> c(na) |> getelement(1)
    )
  })

  output$scoreSavedTextM <- renderText({
    # don't react to these until the button is pushed
    if(TRUE) {
      input$saveScoreButtonm
      isolate(gts <- as.numeric(input$gameToScoreM))
      isolate(hs <- as.numeric(input$hscoreM))
      isolate(as <- as.numeric(input$ascoreM))

      home <- home_team_name(TM(), gts)
      away <- away_team_name(TM(), gts)

      # this will react each time the save score button is pressed.
      if (as.numeric(input$saveScoreButtonM) > 0)
        paste0("score saved for game ", gts, ": ", away, " ", as, " - ", home, " ", hs )
      else "select a game and enter scores above."
    } else {
      "sample text."
    }
  })
  output$scoreSavedTextW <- renderText({
    # don't react to these until the button is pushed
    if(TRUE) {
      input$saveScoreButtonW
      isolate(gts <- as.numeric(input$gameToScoreW))
      isolate(hs <- as.numeric(input$hscoreW))
      isolate(as <- as.numeric(input$ascoreW))

      home <- home_team_name(TW(), gts)
      away <- away_team_name(TW(), gts)

      # this will react each time the save score button is pressed.
      if (as.numeric(input$saveScoreButtonW) > 0)
        paste0("score saved for game ", gts, ": ", away, " ", as, " - ", home, " ", hs )
      else "select a game and enter scores above."
    } else {
      "sample text."
    }
  })

  # updates when a new score is saved (button)
  observeEvent( input$saveScoreButtonM, {
    if (as.numeric(input$saveScoreButtonM) > 0) {
      gts <- as.numeric(input$gameToScoreM)
      hs <- as.numeric(input$hscoreM)
      as <- as.numeric(input$ascoreM)
      home <- home_team_name(TM(), gts)
      away <- away_team_name(TM(), gts)
      # createlogentry(paste("score enterred:", away, "vs.", home, as, "-", hs))
      # note for winner_01: 0 = home win; 1 = away win

      # # write score for one game
      # board |>
      #   my_pin_write(
      #     tibble(game_number = gts, winner_01 = as.numeric(as > hs),
      #            home = home, away = away, hscore = hs, ascore = as, timestamp = human_time()),
      #     name =  paste0("/m-", gsub("/"," or ", home), "-", gsub("/", " or ", away))
      # )


      previous_scores <-
        board |>
        my_pin_read(name = config[['scores']][1], default = empty_scores_df)

      new_score <-
        tibble(
          game_number = gts, winner_01 = as.numeric(as > hs),
          home = home, away = away, hscore = hs, ascore = as, timestamp = human_time()
        )
      # replacement for a write_csv() with apend = TRUE
      bind_rows(previous_scores, new_score) |>
        group_by(game_number) |>
        arrange(timestamp, .by_group = TRUE) |>
        slice_tail(n = 1) |>
        my_pin_write(board = board, name = config[['scores']][1] )
    }

    if (as.numeric(input$saveScoreButtonM) > 0 &&
        AdminMode() &&
        n_games_remaining(TM()) <= 16) {
        cacheCrystalBallM() # TODO: turn on crystal ball
    }
  })

  observeEvent( input$saveScoreButtonW, {
    if (as.numeric(input$saveScoreButtonW) > 0) {
      gts <- as.numeric(input$gameToScoreW)
      hs <- as.numeric(input$hscoreW)
      as <- as.numeric(input$ascoreW)
      home <- home_team_name(TW(), gts)
      away <- away_team_name(TW(), gts)
      # createlogentry(paste("women's score enterred:", away, "vs.", home, as, "-", hs))
      # note for winner_01: 0 = home win; 1 = away win

      # # write score for one game
      # board |>
      #   my_pin_write(
      #     tibble(game_number = gts, winner_01 = as.numeric(as > hs),
      #            home = home, away = away, hscore = hs, ascore = as, timestamp = human_time()),
      #     name =  paste0("/m-", gsub("/"," or ", home), "-", gsub("/", " or ", away))
      # )

      previous_scores <-
        my_pin_read(board = board, name = config[['scores']][2], default = empty_scores_df)

      new_score <-
        tibble(
          game_number = gts, winner_01 = as.numeric(as > hs),
          home = home, away = away, hscore = hs, ascore = as, timestamp = human_time()
        )
      # replacement for a write_csv() with apend = TRUE

      # print(new_score)

      bind_rows(previous_scores, new_score) |>
        group_by(game_number) |>
        arrange(timestamp, .by_group = TRUE) |>
        slice_tail(n = 1) |>
        my_pin_write( board = board, name = config[['scores']][2] )
    }
    if (as.numeric(input$saveScoreButtonW) > 0 &&
        AdminMode() &&
        n_games_remaining(TW()) <= 16) {
        cacheCrystalballW()  # TODO: turn crystal ball back on
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

  output$CostTable1 <- DT::renderDT(
    rownames = FALSE,
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

  output$CostTable2 <- DT::renderDT(
    rownames = FALSE,
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

  output$PastWinners <- DT::renderDT(
    rownames = FALSE,
    options=list(lengthChange=0,                      # show/hide records per page dropdown
                 searching=0,                         # global search box on/off
                 info=0,                              # information on/off (how many records filtered, etc)
                 ordering = FALSE,
                 paging=FALSE,
                 autoWidth=TRUE                       # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),  {
    History <- readr::read_csv("data/historical-winners.csv") # , header=TRUE)
    History %>% arrange(desc(year))
  })

  ############# game scores table #####################

  # todo: turn scoring back on
  output$ScoresTableM <- DT::renderDT(
    rownames = FALSE,
    options=list(pageLength = 63,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 ordering = FALSE,
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),  {
      CompletedGamesM()
    })
  output$ScoresTableW <- DT::renderDT(
    rownames = FALSE,
    options=list(pageLength = 63,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
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
  output$standingsTableM <- DT::renderDT(
    rownames = FALSE,
    options=list(pageLength = 35,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ContestStandingsM()
    })

  output$standingsTableW <- DT::renderDT(
    rownames = FALSE,
    options=list(pageLength = 35,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ContestStandingsW()
    })

  output$standingsTableAll <- DT::renderDT(
    rownames = FALSE,
    options=list(pageLength = 35,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ContestStandingsAll()
    })

  ######### reactive text messages #########
  output$tournyStatusM <- renderText({
    games <- nrow(CompletedGamesM())
    paste0("data based on ", nrow(EM()), " contestants and ", games, " games.")
  })
  output$tournyStatusW <- renderText({
    games <- nrow(CompletedGamesW())
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

  # output$teamData <- DT::renderDT(
  #   rownames = FALSE,
  #   options=list(pageLength = 64,             # initial number of records
  #                lengthMenu = c(5,10,25,50),  # records/page options
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
  output$entrantSelector <- renderUI({
    entrants <- sapply(Entries(), function(x) x$email)
    names(entrants) <-
      paste0(
        sapply(Entries(), function(x) x$name),
        " [",
        ContestStandings()$score,
        "]"
      )
    selectInput("oneEntrant", "select a player", choices = entrants[order(- ContestStandings()$score)], selectize=FALSE)
  })



  output$WhoCanWinPlotM <- renderVegawidget({
    WinnersTableM() |>
      mutate(winner2 = abbreviate(winner, minlength = 20)) |>
      vl_chart() |>
      vl_mark_bar(fill = "steelblue") |>
      vl_encode_x("p:Q") |>
      vl_encode_y("winner2:O", sort = "-x", title = "Winner") |>
      vl_axis_x(format = "%", title = "") |>
      vl_add_properties(width = 600)
      # gf_col(winner ~ p, fill = "steelblue") |>
      # gf_labs(x = "percent of scenarios that win") |>
      # gf_refine(scale_x_continuous(labels = scales::label_percent()))
  })


  output$ScoreHistogramsM <-
    renderPlot(height = 600,
      {
      PossibleScoresTableM() |>
        gf_col(n ~ round(score) | reorder(name, score, function(x) - mean (x)),
               binwidth = 1, fill = "steelblue") |>
          gf_labs(x = "score")
    })

  textmat <- function(m, denom) {
    rn <- rownames(m) |> matrix(nrow = nrow(m), ncol = ncol(m), byrow = FALSE)
    cn <- colnames(m) |> matrix(nrow = nrow(m), ncol = ncol(m), byrow = TRUE)
    paste0(
      rn , " defeats " , cn , "</br>in " , m , " scenarios</br>",
      "(" , round(100 * m / denom,2) , "%)") |>
      matrix(nrow = nrow(m))
  }
  oldh2hplot <- function(data) {
    data |>
      mutate(
        perc = round(100 * prop, 2),
        hovertext1 = glue::glue('{key_name}'),
        hovertext2 = glue::glue('defeats'),
        hovertext3 = glue::glue('{other_name}'),
        hovertext4 = glue::glue('in {scenarios} scenarios.'),
        hovertext5 = glue::glue('({perc} %)'),
        scenarios = ifelse(max(scenarios) > 1 & scenarios <= 0, NA, scenarios)
      ) |>
    gf_raster(scenarios ~ other_abbrv + key_abbrv, alpha = 0.8,
              text = ~hovertext) |>
      gf_hline(yintercept = 0.5 + (0:nrow(EM())), color = "gray80", inherit = FALSE, linewidth = 0.5) |>
      gf_vline(xintercept = 0.5 + (0:nrow(EM())), color = "gray80", inherit = FALSE, linewidth = 0.5) |>
      gf_labs(title = "head to head winning scenarios",
              subtitle = "read across rows for wins against the other player",
              x = "", y = "", fill = "winning\nscenarios" ) |>
      gf_refine(
        scale_fill_steps(low = "white", high = "steelblue", n.breaks = 8),
        coord_cartesian(expand = FALSE)
      ) |>
      gf_theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = rgb(1,0,0, alpha = 0.2))
      ) |>
      plotly::ggplotly(tooltip = "text")  # failing with error "undefined columns selected" in 2025
  }

  h2hplot <- function(data) {
    sorted_data <-
      data |>
      group_by(key_name, key_abbrv) |>
      summarise(avg_win_p = mean(prop, na.rm = TRUE)) |>
      arrange(desc(avg_win_p)) |>
      mutate(
        key_name = as.character(key_name),
        key_abbrv = as.character(key_abbrv)
      )

    vl_chart() |>
      vl_add_data_frame(
        data |>
          # filter(prop > 0) |>
          mutate(
            perc = round(100 * prop, 2),
            hovertext1 = glue::glue('{key_name}'),
            hovertext2 = glue::glue('defeats'),
            hovertext3 = glue::glue('{other_name}'),
            hovertext4 = glue::glue('in {scenarios} scenarios.'),
            hovertext5 = glue::glue('({perc} %)'),
            scenarios = ifelse(max(scenarios) > 1 & scenarios <= 0, NA, scenarios)
          )
      ) |>
      vl_mark_rect(opacity = 0.7) |>
      vl_encode_y("key_name:N", title = FALSE, sort = sorted_data |> pull(key_name)) |>
      vl_encode_x("other_abbrv:N", title = FALSE, sort = sorted_data |> pull(key_abbrv)) |>
      vl_encode_fill("prop:Q", legend = FALSE) |>
      vl_condition_fill(test = "datum['scenarios'] === null", value = "salmon") |>
      vl_scale_fill(scheme = "blues") |>
      vl_encode_tooltip("hovertext1", title = " ") |>
      vl_encode_tooltip("hovertext2", title = "  ") |>
      vl_encode_tooltip("hovertext3", title = "    ") |>
      vl_encode_tooltip("hovertext4", title = "     ") |>
      vl_encode_tooltip("hovertext5", title = "      ") |>
      vl_add_properties(
        title =
          list(text = "head to head winning scenarios",
               subtitle = "read across rows for wins against the other player"
          )
      )
  }

  output$H2HPlotM <- vegabrite::renderVegawidget({
    comps <- competitors(Entries(), division = "M", by = "name")
    # print(comps)
    H2HM() |>
      filter(key_name %in% comps) |>
      filter(other_name %in% comps) |>
      h2hplot()
  })

  output$WhoCanWinPlotW <- renderVegawidget({
    WinnersTableW() |>
      mutate(winner2 = abbreviate(winner, minlength = 20)) |>
      vl_chart() |>
      vl_mark_bar(fill = "steelblue") |>
      vl_encode_x("p:Q") |>
      vl_encode_y("winner2:O", sort = "-x", title = "Winner") |>
      vl_axis_x(format = "%", title = "") |>
      vl_add_properties(width = 600)
      # gf_col(winner ~ p, fill = "steelblue") |>
      # gf_labs(x = "percent of scenarios that win") |>
      # gf_refine(scale_x_continuous(labels = scales::label_percent()))
  })

  output$ScoreHistogramsW <-
    renderPlot(height = 600,
               {
                 PossibleScoresTableW() |>
                   gf_col(n ~ round(score) | reorder(name, score, function(x) - mean (x)),
                          binwidth = 1, fill = "steelblue") |>
                   gf_labs(x = "score")
               })

  output$H2HPlotW <- vegabrite::renderVegawidget({
    comps <- competitors(Entries(), division = "W", by = "name")
    H2HW() |>
      filter(key_name %in% comps) |>
      filter(other_name %in% comps) |>
      h2hplot()
  })


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
  outputOptions(output, "ScoresTableM", suspendWhenHidden = FALSE)
  outputOptions(output, "ScoresTableW", suspendWhenHidden = FALSE)
  outputOptions(output, "standingsTableM", suspendWhenHidden = FALSE, priority = 100)
  outputOptions(output, "standingsTableW", suspendWhenHidden = FALSE, priority = 101)
  outputOptions(output, "standingsTableAll", suspendWhenHidden = FALSE, priority = 90)
  outputOptions(output, "WhoCanWinPlotM", suspendWhenHidden = FALSE, priority = 80)
  outputOptions(output, "WhoCanWinPlotM", suspendWhenHidden = FALSE, priority = 50)
  outputOptions(output, "WhoCanWinPlotW", suspendWhenHidden = FALSE, priority = 50)
  outputOptions(output, "H2HPlotM", suspendWhenHidden = FALSE, priority = 40)
  outputOptions(output, "H2HPlotW", suspendWhenHidden = FALSE, priority = 40)
  outputOptions(output, "ScoreHistogramsM", suspendWhenHidden = FALSE, priority = 80)
  outputOptions(output, "ScoreHistogramsM", suspendWhenHidden = FALSE, priority = 30)
  outputOptions(output, "ScoreHistogramsW", suspendWhenHidden = FALSE, priority = 30)

  Sys.sleep(1)
  waiter::waiter_hide()

})

