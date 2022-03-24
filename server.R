
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/

library(shiny)
library(dplyr)
library(madness)
library(waiter)
library(ggformula)
library(plotly)
theme_set(theme_bw())
# library(reactlog)
# reactlog_enable()

source("Tourny.R")
source("Loaders.R")

maxPoints <- 200

deadline <- "2017-03-16 12:30"
deadline <- "2018-03-15 12:30"
deadline <- "2019-03-21 11:30"   # standard time!
defaultYear <- 2019

deadline <- "2021-03-19 11:30"   # standard time!
defaultYear <- 2021

deadline <- "2022-03-17 11:30"   # standard time!
defaultYear <- 2022

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")


### Some utility functions

sessionID <- function(session) {
  digest::digest(session$request)
}

the_year <- function() {
  return(2022)
  # the above is a temporary hack.
  qq <- query()
  if ("year" %in% names(qq)) {
    as.numeric(qq["year"])
  } else {
    default_year
  }
}

regionChoices <- function(region, bracket) {
  Region <- bracket %>%
    rename(regn = region) %>%
    filter(regn == region) %>%
    rename(region = regn)

  res <- as.character(Region$team)
  names(res) <- paste0(
    Region$seed, " ", Region$team,
    " [", Region$cost, " pt", ifelse(Region$cost==1, "", "s"),"]")
  res
}

shinyServer(function(input, output, session) {



  query <- reactive( parseQueryString(session$clientData$url_search) )

  adminMode <- reactive({
      tolower(query()["admin"]) %in% c("yes","y") || file.exists('admin.txt')
  })

  ##############################
  # logging

  rValues <- reactiveValues(newLogEntries = 0)

  # LogEntries <-
  #   reactiveFileReader(
  #     2000, session = session,
  #     filePath = "NCAA.log",
  #     function(path) {
  #       logData <- readr::read_csv(path, as.is=TRUE)
  #       names(logData) <- c("time", "event", "session")
  #       logData[rev(seq_len(nrow(logData))), ] |>
  #         mutate(time = lubridate::ymd_hms(time) - lubridate::hours(4))
  #     }
  #   )
  #
  # createLogEntry <- function(text) {
  #   isolate( rValues$newLogEntries <- rValues$newLogEntries + 1 )
  #   write(paste0(Sys.time(), ',"', text, '",', sessionID(session)),
  #         append=TRUE, file="NCAA.log")
  #   output$statusMessage <- renderText({ paste(Sys.time(), text, sep=": ") })
  # }
  #
  # session$onSessionEnded(function() {
  #   createLogEntry("Sesssion ended.")
  # })
  #
  # createLogEntry("New session started.")

  ##############################
  # watch files and load data

  Entries <-
    reactivePoll(
      500,
      session = session,
      function() dir("data/Entries/", full.names = TRUE) |> file.mtime() |> max(),
      function() {
        load_entries_from_files(
          TMinit(),
          path = "data/Entries/2022"
          )
      }
    )
  # BracketM <- reactiveVal(LoadBracket('data/bracket2022.csv'))
  # BracketW <- reactiveVal(LoadBracket('data/bracket2022w.csv'))

  BracketM <- reactiveFileReader(
      500,
      session = session,
      "data/bracket2022.csv",
      LoadBracket
    )
  BracketW <- reactiveFileReader(
      500,
      session = session,
      "data/bracket2022w.csv",
      LoadBracket
    )

  GameScoresM <- reactiveFileReader(
      500,
      session = session,
      'data/Scores/2022/Mens/scores-2022-M.csv',
      madness::load_scores_file
    )

  GameScoresW <- reactiveFileReader(
      500,
      session = session,
      'data/Scores/2022/Womens/scores-2022-W.csv',
      madness::load_scores_file
    )

  GameScoresM <-
    reactivePoll(
      500,
      session = session,
      function() max(file.mtime(dir("data/Scores/2022/Mens", full.names = TRUE))),
      function() {LoadGameScores("data/Scores/2022/Mens/", pattern = "M-.*2022.*\\.csv")}
    )

  GameScoresW <-
    reactivePoll(
      500,
      session = session,
      function() max(file.mtime(dir("data/Scores/2022/Womens", full.names = TRUE))),
      function() {LoadGameScores("data/Scores/2022/Womens/", pattern = "W-.*2022.*\\.csv")}
    )

  ##############################
  # entry matrices

  # 2022 hack to change Chem dept name
  EM <- reactive({
    res <- build_entry_matrix(Entries(), ext = "M")
    d <- attr(res, "dept")
    d[d == "Chem"] <- "Chem/BioCh"
    attr(res, "dept") <- d
    res
    })
  EW <- reactive({
    res <- build_entry_matrix(Entries(), ext = "W")
    d <- attr(res, "dept")
    d[d == "Chem"] <- "Chem/BioCh"
    attr(res, "dept") <- d
    res
  })

  ##############################
  # Brackets and tournament

  TMinit <- reactive({tournament_init(names = BracketM()[['team']], seeds = BracketM()[['seed']], label = "M")})
  TWinit <- reactive({tournament_init(names = BracketW()[['team']], seeds = BracketW()[['seed']], label = "W")})

  TM <- reactive({
    Scores <- GameScoresM()
    if (nrow(Scores) > 0) {
      TMinit() |> tournament_update(games = Scores[['game_number']], results = Scores[['winner_01']])
    } else {
      TMinit()
    }
  })

  TW <- reactive({
    Scores <- GameScoresW()
    if (nrow(Scores) > 0) {
      TWinit() |> tournament_update( games = Scores[['game_number']], results = Scores[['winner_01']])
    } else {
      TWinit()
    }
  })

  ### ****** -> madness?

  CompletedGamesM <- reactive({
    compGames <- completedGames(GameScoresM(), BracketM())
    if (nrow(compGames) < 1) return(compGames)
    compGames %>%
      arrange(game) %>%
      select(game, winner, loser, score)
  })
  CompletedGamesW <- reactive({
    compGames <- completedGames(GameScoresW(), BracketW())
    if (nrow(compGames) < 1) return(compGames)
    compGames %>%
      arrange(game) %>%
      select(game, winner, loser, score)
  })

  ContestStandingsM <- reactive({
    madness::contest_standings(TM(), EM(), BracketM())
  })
  ContestStandingsW <- reactive({
    madness::contest_standings(TW(), EW(), BracketW())
  })
  ContestStandingsAll <- reactive({
    CSM <- ContestStandingsM()
    CSW <- ContestStandingsW()
    CS <- CSM |> dplyr::full_join(CSW, by = c('email' = 'email'), suffix = c('_M', '_W')) |>
      filter(score_M > 0 & score_W > 0)
    CS |>
      mutate(
        total = score_M + score_W,
        name = name_M,
        `guaranteed wins` = `guaranteed wins_M` + `guaranteed wins_W`,
        `max possible` = `max possible_M` + `max possible_W`
      ) |>
      arrange(desc(total)) |>
      rename(`men's wins` = score_M, `women's wins` = score_W) |>
      select(name, `men's wins`, `women's wins`, total, `guaranteed wins`, `max possible`)
  })

  observeEvent(
    input$saveScoreButtonM,
    {
      if (adminMode()) {
        madness::tournament_completions(TM(), max_games_remaining = 15) |>
          saveRDS('data/2022/TCM.Rds')
      }
    })

  TCM <-
    reactiveFileReader(
      1000, session,
      'data/2022/TCM.Rds',
      readRDS
    )

  observeEvent(
    TCM(),
    {
      if (adminMode()) {
        madness::head2head(TM(), EM(), TCM()) |>
          saveRDS('data/2022/H2HM.Rds')
      }
    })

  H2HM <- reactiveFileReader(
    1000, session,
    'data/2022/H2HM.Rds',
    readRDS
  )

  PossibleScoresM <- reactiveFileReader(
    1000, session,
    'data/2022/PossibleScoresM.Rds',
    readRDS
  )

  observeEvent(
    TCM(),
    {
      if (adminMode()) {
        TCM() |>
          apply(2, function(x, e = EM()) { contest_scores(x, e)} ) |>
          saveRDS('data/2022/PossibleScoresM.Rds')
      }
    })

  WinnersTableM <- reactiveFileReader(
    1000, session,
    'data/2022/WinnersTableM.Rds',
    readRDS)

  observeEvent(
    PossibleScoresM(),
    {
      if (adminMode()) }
        PossibleScoresM() |>
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
          saveRDS('data/2022/WinnersTableM.Rds')
      }
    })

  PossibleScoresTableM <- reactive({
    PossibleScoresM() |>
      as.table() |>
      as.data.frame() |>
      setNames(c('name', 'sceneario', 'score')) |>
      group_by(name, score) |>
      tally()
  })

  observeEvent(
    input$saveScoreButtonW,
    {
      if (adminMode()) {
        madness::tournament_completions(TW(), max_games_remaining = 15) |>
          saveRDS('data/2022/TCW.Rds')
      }
    })

  TCW <-
    reactiveFileReader(
      1000, session,
      'data/2022/TCW.Rds',
      readRDS
    )

  observeEvent(
    TCW(),
    {
      if (adminMode()) {
        madness::head2head(TW(), EW(), TCW()) |>
          saveRDS('data/2022/H2HW.Rds')
      }
    })

  H2HW <- reactiveFileReader(
    1000, session,
    'data/2022/H2HW.Rds',
    readRDS
  )

  PossibleScoresW <- reactiveFileReader(
    1000, session,
    'data/2022/PossibleScoresW.Rds',
    readRDS
  )

  observeEvent(
    TCW(),
    {
      if (adminMode()) {
        TCW() |>
          apply(2, function(x, e = EW()) { contest_scores(x, e)} ) |>
          saveRDS('data/2022/PossibleScoresW.Rds')
      }
    })

  WinnersTableW <- reactiveFileReader(
    1000, session,
    'data/2022/WinnersTableW.Rds',
    readRDS)

  observeEvent(
    PossibleScoresW(),
    {
      if (adminMode()) {
        PossibleScoresW() |>
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
          saveRDS('data/2022/WinnersTableW.Rds')
      }
    })

  PossibleScoresTableW <- reactive({
    PossibleScoresW() |>
      as.table() |>
      as.data.frame() |>
      setNames(c('name', 'sceneario', 'score')) |>
      group_by(name, score) |>
      tally()
  })



  ############ Select Teams ###########

  TeamsM <- reactive(c( input$regionM1, input$regionM2, input$regionM3, input$regionM4 ))
  TeamsW <- reactive(c( input$regionW1, input$regionW2, input$regionW3, input$regionW4 ))

  regionsM <- reactive({unique(BracketM()$region)})
  regionsW <- reactive({unique(BracketW()$region)})

  output$RegionNameM1 <- renderText({regionsM()[1]})
  output$RegionNameM2 <- renderText({regionsM()[2]})
  output$RegionNameM3 <- renderText({regionsM()[3]})
  output$RegionNameM4 <- renderText({regionsM()[4]})

  output$RegionNameW1 <- renderText({regionsW()[1]})
  output$RegionNameW2 <- renderText({regionsW()[2]})
  output$RegionNameW3 <- renderText({regionsW()[3]})
  output$RegionNameW4 <- renderText({regionsW()[4]})

  output$TeamsSelectorM1 <- renderUI({
    checkboxGroupInput("regionM1","",regionChoices(regionsM()[1], BracketM()))
  })

  output$TeamsSelectorM2 <- renderUI({
    checkboxGroupInput("regionM2","",regionChoices(regionsM()[2], BracketM()))
  })

  output$TeamsSelectorM3 <- renderUI({
    checkboxGroupInput("regionM3","",regionChoices(regionsM()[3], BracketM()))
  })

  output$TeamsSelectorM4 <- renderUI({
    checkboxGroupInput("regionM4","",regionChoices(regionsM()[4], BracketM()))
  })

  output$TeamsSelectorW1 <- renderUI({
    checkboxGroupInput("regionW1","",regionChoices(regionsW()[1], BracketW()))
  })

  output$TeamsSelectorW2 <- renderUI({
    checkboxGroupInput("regionW2","",regionChoices(regionsW()[2], BracketW()))
  })

  output$TeamsSelectorW3 <- renderUI({
    checkboxGroupInput("regionW3","",regionChoices(regionsW()[3], BracketW()))
  })

  output$TeamsSelectorW4 <- renderUI({
    checkboxGroupInput("regionW4","",regionChoices(regionsW()[4], BracketW()))
  })

  # timeOfLastEntry <- reactive( lastTimeStamp )
  # entryStatus <- renderText( paste("Most Recent Entry: ", timeOfLastEntry() ) )

  totalTeamsM <- reactive( length(TeamsM()) )
  totalTeamsW <- reactive( length(TeamsW()) )

  output$totalTeamsM <- renderText(paste("Number of Teams:",totalTeamsM()))
  output$totalTeamsW <- renderText(paste("Number of Teams:",totalTeamsW()))

  pointsSpentM <- reactive( sum( filter(BracketM(), team %in% TeamsM())$cost ) )
  pointsSpentW <- reactive( sum( filter(BracketW(), team %in% TeamsW())$cost ) )

  output$pointsSpentM <- reactive(pointsSpentM())
  output$pointsSpentW <- reactive(pointsSpentW())

  outputOptions(output, "pointsSpentM", suspendWhenHidden = FALSE)
  outputOptions(output, "pointsSpentW", suspendWhenHidden = FALSE)

  pointsRemainingM <- reactive( maxPoints - pointsSpentM() )
  pointsRemainingW <- reactive( maxPoints - pointsSpentW() )

  output$pointsRemainingM <- renderText(paste("Points Remaining:", pointsRemainingM()))
  output$pointsRemainingW <- renderText(paste("Points Remaining:", pointsRemainingW()))

  output$year <- renderText(paste(the_year()))

  output$manyTeamsM <- reactive(length(TeamsM()) > 3)
  output$manyTeamsW <- reactive(length(TeamsW()) > 3)

  output$spendMessageM<- renderText({
    if (pointsSpentM() < maxPoints) {
      "Choose a team, you've got points to spend."
    } else if  (pointsSpentM() > maxPoints ) {
      "You have overspent.  Please remove a team."
    } else {
      "You have spent all of your points."
    }
  })
  output$spendMessageW<- renderText({
    if (pointsSpentW() < maxPoints) {
      "Choose a team, you've got points to spend."
    } else if  (pointsSpentW() > maxPoints ) {
      "You have overspent.  Please remove a team."
    } else {
      "You have spent all of your points."
    }
  })

  output$confirmation <- renderUI( {
    if (input$submitButton > 0) {
      lastTimeStamp <<- Sys.time()
#      output$statusMessage <- renderText({paste("Last entry submited at", lastTimeStamp)})
      output$statusMessage <- renderText({paste("Preparing to submit", lastTimeStamp)})
      NewEntry <-
      isolate(
        list( name= input$name,
              email = input$email,
              dept = input$dept,
              points = pointsSpentM(),
              pointsW = pointsSpentW(),
              teams = TeamsM(),
              teamsW = TeamsW(),
              teamsLogical = sapply(BracketM()$team, function(x) x %in% TeamsM()),
              teamsLogicalW = sapply(BracketW()$team, function(x) x %in% TeamsW()),
              time = lastTimeStamp)
      )
      saveRDS(NewEntry, file=paste0("data/Entries/2022/Entry-",
                                    humanTime(),
                                    "-",    # was missing when 2016 entries were posted.
                                    digest::digest(NewEntry),
                                    ".rds"))
      # createLogEntry(paste("Entry submitted for", isolate(input$email)))
#      Entries <<- LoadEntries()
    }

    if (input$submitButton < 1) {
      tagList(
        p("Testing...")
      )
    } else {
      tagList(HTML(
        paste0("<h4>Thank you, ", isolate(input$name), ", your selections have been submitted.</h4>",
               "<ul>",
               "<li>You spent ", isolate(pointsSpentM()), " points on the following ", isolate(length(TeamsM())),
               " teams: ", paste( isolate(TeamsM()), collapse=", " ),
               "</li><li>You spent ", isolate(pointsSpentW()), " points on the following ", isolate(length(TeamsW())),
               " teams: ", paste( isolate(TeamsW()), collapse=", " ),
               "</li></ul>"
        )
      ))
    }
  }
  )

  output$numEntries <- reactive( length(Entries()) )

  ############ Download Data ###########
  output$downloadData <- downloadHandler(
    filename = function() { "Entries.rds" },
    content = function(file) {
      saveRDS(Entries(), file)
    }
  )

  ########### Turn Controls on and off ############

  output$showCrystalBallM <- reactive({
    n_teams_remaining(TM()) <= 16
  })
  output$showCrystalBallW <- reactive({
    n_teams_remaining(TW()) <= 16
  })

  output$showDownloadButton <- reactive({
    "download" %in% names(query())
  } )
  outputOptions(output, "showDownloadButton", suspendWhenHidden = FALSE)

  output$showEntryForm <- reactive({
    ( as.numeric(input$submitButton) + as.numeric(input$reviseButton) ) %% 2 == 0
  })
  outputOptions(output, "showEntryForm", suspendWhenHidden = FALSE)

  output$acceptingEntries <- reactive({
    adminMode() ||
      (file.exists(bracketFile) &&
         Sys.time() < lubridate::ymd_hm(deadline) + lubridate::hours(5) )
  })
  outputOptions(output, "acceptingEntries", suspendWhenHidden = FALSE)

  output$showAdminTab <- reactive({
    adminMode()
  })
  outputOptions(output, "showAdminTab", suspendWhenHidden = FALSE)

  output$showGameEntry <- reactive({
    tolower(as.character(input$passwd)) == "madly marching"
  })
  outputOptions(output, "showGameEntry", suspendWhenHidden = FALSE)

  ## Not working.. always yield FALSE?
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

  output$showScoresM <- reactive({
    nrow(CompletedGamesM()) > 0
  } )
  output$showScoresW <- reactive({
    nrow(CompletedGamesW()) > 0
  } )
  outputOptions(output, "showScoresM", suspendWhenHidden = FALSE)
  outputOptions(output, "showScoresW", suspendWhenHidden = FALSE)


  ############ Score Updates #############

  output$password <- renderPrint({input$password})

  output$gameScoreSelectorM <- renderUI({
    games <- allGames(BracketM(), GameScoresM())
    selectInput("gameToScoreM", "Choose a Game", choices = games, selectize=FALSE)
  })

  output$gameScoreSelectorW <- renderUI({
    games <- allGames(BracketW(), GameScoresW())
    selectInput("gameToScoreW", "Choose a Game", choices = games, selectize=FALSE)
  })

  output$gameToScoreTextM <- renderText({
    paste0("About to give score for game ", input$gameToScoreM, ": ",
           awayTeam(as.numeric(input$gameToScoreM), BracketM(), GameScoresM()), " vs. ",
           homeTeam(as.numeric(input$gameToScoreM), BracketM(), GameScoresM())
    )
  })
  output$gameToScoreTextW <- renderText({
    paste0("About to give score for game ", input$gameToScoreW, ": ",
           awayTeam(as.numeric(input$gameToScoreW), BracketW(), GameScoresW()), " vs. ",
           homeTeam(as.numeric(input$gameToScoreW), BracketW(), GameScoresW())
    )
  })

  output$homeTeamScoreM <- renderUI({
    numericInput("hscoreM", step=0,
                 label = homeTeam(as.numeric(input$gameToScoreM), BracketM(), GameScoresM()),
                 value = homeScore(as.numeric(input$gameToScoreM), BracketM(), GameScoresM()))
  })
  output$homeTeamScoreW <- renderUI({
    numericInput("hscoreW", step=0,
                 label = homeTeam(as.numeric(input$gameToScoreW), BracketW(), GameScoresW()),
                 value = homeScore(as.numeric(input$gameToScoreW), BracketW(), GameScoresW()))
  })

  output$awayTeamScoreM <- renderUI({
    numericInput("ascoreM", step=0,
                 label = awayTeam(as.numeric(input$gameToScoreM), BracketM(), GameScoresM()),
                 value = awayScore(as.numeric(input$gameToScoreM), BracketM(), GameScoresM()))
  })
  output$awayTeamScoreW <- renderUI({
    numericInput("ascoreW", step=0,
                 label = awayTeam(as.numeric(input$gameToScoreW), BracketW(), GameScoresW()),
                 value = awayScore(as.numeric(input$gameToScoreW), BracketW(), GameScoresW()))
  })

  output$scoreSavedTextM <- renderText({
    # don't react to these until the button is pushed
    if(TRUE) {
      input$saveScoreButtonM
      isolate(gts <- as.numeric(input$gameToScoreM))
      isolate(hs <- as.numeric(input$hscoreM))
      isolate(as <- as.numeric(input$ascoreM))

      home <- homeTeam(gts, BracketM(), GameScoresM())
      away <- awayTeam(gts, BracketM(), GameScoresM())

      # this will react each time the save score button is pressed.
      if (as.numeric(input$saveScoreButtonM) > 0)
        paste0("Score saved for game ", gts, ": ", away, " ", as, " - ", home, " ", hs )
      else "Select a game and enter scores above."
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

      home <- homeTeam(gts, BracketW(), GameScoresW())
      away <- awayTeam(gts, BracketW(), GameScoresW())

      # this will react each time the save score button is pressed.
      if (as.numeric(input$saveScoreButtonW) > 0)
        paste0("Score saved for game ", gts, ": ", away, " ", as, " - ", home, " ", hs )
      else "Select a game and enter scores above."
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
      home <- homeTeam(gts, BracketM(), GameScoresM())
      away <- awayTeam(gts, BracketM(), GameScoresM())
      # createLogEntry(paste("Score enterred:", away, "vs.", home, as, "-", hs))
      # Note for winner_01: 0 = home win; 1 = away win
      readr::write_csv(
        tibble(game_number = gts, winner_01 = as.numeric(as > hs),
               home = home, away = away, hscore = hs, ascore = as),
        file = paste0("data/Scores/2022/Mens/M-",gsub("/"," or ", home),"-", gsub("/", " or ", away),
                      "-", humanTime(), ".csv")
      )
      readr::write_csv(
        tibble(game_number = gts, winner_01 = as.numeric(as > hs),
               home = home, away = away, hscore = hs, ascore = as),
        file = "data/Scores/2022/Mens/scores-2022-M.csv", append = TRUE)
    }
  })
  observeEvent( input$saveScoreButtonW, {
    if (as.numeric(input$saveScoreButtonW) > 0) {
      gts <- as.numeric(input$gameToScoreW)
      hs <- as.numeric(input$hscoreW)
      as <- as.numeric(input$ascoreW)
      home <- homeTeam(gts, BracketW(), GameScoresW())
      away <- awayTeam(gts, BracketW(), GameScoresW())
      # createLogEntry(paste("Women's Score enterred:", away, "vs.", home, as, "-", hs))
      # Note for winner_01: 0 = home win; 1 = away win
      readr::write_csv(
        tibble(game_number = gts, winner_01 = as.numeric(as > hs),
               home = home, away = away, hscore = hs, ascore = as),
        # row.names = FALSE,
        file = paste0("data/Scores/2022/Womens/W-",gsub("/"," or ", home),"-", gsub("/", " or ", away),
                      "-", humanTime(), ".csv")
      )
      readr::write_csv(
        tibble(game_number = gts, winner_01 = as.numeric(as > hs),
               home = home, away = away, hscore = hs, ascore = as),
        file = "data/Scores/2022/Womens/scores-2022-W.csv", append = TRUE)
    }
  })


  ########## Bracket ###############

  # updates when games scores change
  BracketWithTeamStatusM <- reactive({
    addTeamStatus(BracketM(), scheduledGames(bracket=BracketM(), results = GameScoresM()))
  })
  BracketWithTeamStatusW <- reactive({
    addTeamStatus(BracketW(), scheduledGames(bracket=BracketW(), results = GameScoresW()))
  })


  ############ Query Stuff #############
  output$queryText <- renderText({
    # Return a string with key-value pairs
    paste(names(query()), query(), sep = "=", collapse=", ")
  })

  ########################################
  # Outputs
  ########################################

  ########## Instructions/Background Info

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

  output$PastWinners <- renderDataTable(
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

  ############# Game Scores Table #####################

  output$ScoresTableM <- renderDataTable(
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
  output$ScoresTableW <- renderDataTable(
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

  ############# Standings Table #####################

  output$standingsTableM <- renderDataTable(
    options=list(pageLength = 35,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ContestStandingsM()
    })

  output$standingsTableW <- renderDataTable(
    options=list(pageLength = 35,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ContestStandingsW()
    })

  output$standingsTableAll <- renderDataTable(
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
    games <- nrow(CompletedGamesM()) # sum(BracketWithTeamStatus()$wins, na.rm=TRUE)
    paste0("Data based on ", nrow(EM()), " contestants and ", games, " games.")
  })
  output$tournyStatusW <- renderText({
    games <- nrow(CompletedGamesW()) # sum(BracketWithTeamStatus()$wins, na.rm=TRUE)
    paste0("Data based on ", nrow(EW()), " contestants and ", games, " games.")
  })

  ######### (basketball) team data table #########

  TeamDataM <- reactive({
    teamData(Entries(), BracketWithTeamStatusM())
  })

  TeamDataW <- reactive({
    teamData(Entries(), BracketWithTeamStatusW())
  })

  output$teamData <- renderDataTable(
    options=list(pageLength = 64,             # initial number of records
                 lengthMenu = c(5,10,25,50),  # records/page options
                 lengthChange = 0,            # show/hide records per page dropdown
                 searching = 1,               # global search box on/off
                 info = 1,                    # information on/off (how many records filtered, etc)
                 autoWidth = 1                # automatic column width calculation, disable if passing column width via aoColumnDefs
                 #aoColumnDefs = list(list(sWidth="300px", aTargets=c(list(0),list(1))))    # custom column size
    ),
    TeamDataM()
  )
      # E <- Entries()
      # M <- do.call(rbind, lapply( E, function(x) x$teamsLogical ) )
      # B <- BracketWithTeamStatus()
      # if (length(E) > 0) {
      #   row.names(M) <- sapply( E, function(x) x$name)
      # }
      #
      # data.frame(check.names=FALSE,
      #            Team=colnames(M),
      #            Seed = B$seed, Region=B$region,
      #            "Number of players selecting"=apply(M,2,sum),
      #            Wins = B$wins)

  output$entrantSelector <- renderUI({
    entrants <- sapply(Entries(), function(x) x$email)
    names(entrants) <-
      paste0(
        sapply(Entries(), function(x) x$name),
        " [",
        ContestStandings()$score,
        "]"
      )
    selectInput("oneEntrant", "Select a player", choices = entrants[order(- ContestStandings()$score)], selectize=FALSE)
  })


  # H2H <- reactive({
  #   E <- Entries()
  #   G <- GameScoresM()
  #   B <- BracketWithTeamStatusM()
  #   head2head_byindex <-
  #     Vectorize( function(i, j) head2head(E[[i]]$teams, E[[j]]$teams, BracketM(), G) )
  #   n <- length(E)
  #   best <- t(outer(input$oneEntrant, 1:n, head2head_byindex))
  #   bestText <- ifelse(best > 0, paste("win by", best), ifelse (best == 0, "tie", paste("lose by", -best)))
  #   worst <- outer(1:n, input$oneEntrant, head2head_byindex)
  #   worstText <- ifelse(worst > 0, paste("lose by", worst), ifelse (worst == 0, "tie", paste("win by" , -worst)))
  #   tibble(
  #     `other player` = sapply(E, function(x) x$name),
  #     `best head-to-head result` = as.vector(bestText),
  #     `worst head-to-head result` = as.vector(worstText),
  #     `plus teams`  = sapply(E, function(x)
  #         intersect( B[B$alive, "team"], setdiff(E[[input$oneEntrant]]$teams, x$teams))
  #       ),
  #       `minus teams` = sapply(E, function(x)
  #           intersect( B[B$alive, "team"], setdiff(x$teams, E[[input$oneEntrant]]$teams))
  #       ),
  #       `common teams` = sapply(E, function(x)
  #           intersect( B[B$alive, "team"], intersect(x$teams, E[[input$oneEntrant]]$teams))
  #       )
  #   ) %>%
  #     cbind( ContestStandings() ) %>%
  #     arrange(- score) %>%
  #     select(1:6, `current score of other player` = score)
  # })
  #
  # output$H2HTable <- renderDataTable(
  #   options=list(pageLength = 100,                    # initial number of records
  #                lengthMenu=c(25,50,100),             # records/page options
  #                lengthChange=0,                      # show/hide records per page dropdown
  #                searching=1,                         # global search box on/off
  #                info=1,                              # information on/off (how many records filtered, etc)
  #                ordering = TRUE,
  #                autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
  #   ),
  #   {
  #   H2H()
  # })

  # Sweet16M <-
  #   reactiveFileReader(
  #     2000, session,
  #     "data/Sweet16winMatrix.rds",
  #     readRDS
  #   )
  #
  # Sweet16Standings <-
  #   reactiveFileReader(
  #     2000, session,
  #     "data/Sweet16Standings.rds",
  #     readRDS
  #   )

  output$WhoCanWinPlotM <- renderPlot({
    WinnersTableM() |>
      gf_col(winner ~ p, fill = "steelblue") |>
      gf_labs(x = "percent of scenarios that win") |>
      gf_refine(scale_x_continuous(labels = scales::label_percent()))
  })

  output$ScoreHistogramsM <-
    renderPlot(height = 600,
      {
      PossibleScoresTableM() |>
        gf_col(n ~ round(score) | reorder(name, score, function(x) - mean (x)),
               binwidth = 1, fill = "steelblue") |>
          gf_labs(x = "score")
    })

  output$H2HPlotM <- renderPlotly({
    H2HM() |>
      mutate(
        perc = round(100 * scenarios / 2^n_games_remaining(TW()), 2),
        hovertext =
          glue::glue('{key_name}<br>defeats<br>{other_name}<br>in {scenarios} scenarios.<br>({perc} %)')
      ) |>
      # mutate(scenarios = ifelse(scenarios <= 0, NA, scenarios)) |>
      gf_tile(scenarios ~ other_abbrv + key_abbrv, alpha = 0.8,
              text = ~hovertext) |>
      gf_labs(title = "Head to head winning scenarios",
              subtitle = "Read across rows for wins against the other player",
              x = "", y = "", fill = "winning\nscenarios" ) |>
      gf_refine(
        scale_fill_steps(low = "white", high = "steelblue", n.breaks = 12)
      ) |>
      gf_theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) |>
      plotly::ggplotly(tooltip = "text")
  })

  output$WhoCanWinPlotW <- renderPlot({
    WinnersTableW() |>
      gf_col(winner ~ p, fill = "steelblue") |>
      gf_labs(x = "percent of scenarios that win") |>
      gf_refine(scale_x_continuous(labels = scales::label_percent()))
  })

  output$ScoreHistogramsW <-
    renderPlot(height = 600,
               {
                 PossibleScoresTableW() |>
                   gf_col(n ~ round(score) | reorder(name, score, function(x) - mean (x)),
                          binwidth = 1, fill = "steelblue") |>
                   gf_labs(x = "score")
               })

  output$H2HPlotW <- renderPlotly({
    H2HW() |>
      mutate(
        perc = round(100 * scenarios / 2^n_games_remaining(TW()), 2),
        hovertext =
          glue::glue('{key_name}<br>defeats<br>{other_name}<br>in {scenarios} scenarios.<br>({perc} %)')
      ) |>
      # mutate(scenarios = ifelse(scenarios <= 0, NA, scenarios)) |>
      gf_tile(scenarios ~ other_abbrv + key_abbrv, alpha = 0.8,
              text = ~hovertext) |>
      gf_labs(title = "Head to head winning scenarios",
              subtitle = "Read across rows for wins against the other player",
              x = "", y = "", fill = "winning\nscenarios" ) |>
      gf_refine(
        scale_fill_steps(low = "white", high = "steelblue", n.breaks = 12)
      ) |>
      gf_theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) |>
      plotly::ggplotly(tooltip = "text")
  })


  # output$WhoCanWinTable <- renderDataTable(
  #   options=list(pageLength = 100,                    # initial number of records
  #                lengthMenu=c(25,50,100),             # records/page options
  #                lengthChange=0,                      # show/hide records per page dropdown
  #                searching=1,                         # global search box on/off
  #                info=1,                              # information on/off (how many records filtered, etc)
  #                ordering = TRUE,
  #                autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
  #   ),
  #   WhoCanWin(
  #     Entries(),
  #     M = Sweet16M(), Sweet16Standings(),
  #     results = (TeamData() %>% filter(Team %in% rownames(Sweet16M())))$Wins - 2
  #     ) %>%
  #     merge(ContestStandingsM(), by = "name") %>%
  #     select(name, `winning scenarios`, `win percent`,
  #            score, `guaranteed wins`, `max possible`,
  #            `losing scenarios`, `lose percent`)
  # )

  # output$dendroPlot <- renderD3heatmap({
  #   E <- Entries()
  #   M <- do.call(rbind, lapply(E, function(x) x$teamsLogical))
  #   rownames(M) <- sapply(E, function(x) x$name)
  #   D <- as.data.frame(M)
  #   D <- as.data.frame(lapply(D, function(x) as.numeric(x)))
  #   rownames(D) <- rownames(M)
  #   Rowv  <- D %>% dist(method = "euclidean") %>% hclust %>% as.dendrogram %>%
  #     set("branches_k_color", k = 5) %>% set("branches_lwd", 2) %>%
  #     ladderize()
  #   Colv  <- D %>% t %>% dist(method = "euclidean") %>% hclust %>% as.dendrogram %>%
  #     set("branches_k_color", k = 7) %>% set("branches_lwd", 2) %>%
  #     ladderize()
  #   d3heatmap(D, Rowv = Rowv, Colv = Colv, color="Blues")
  # })

  outputOptions(output, "ScoresTableM", suspendWhenHidden = FALSE)
  outputOptions(output, "ScoresTableW", suspendWhenHidden = FALSE)
  outputOptions(output, "standingsTableM", suspendWhenHidden = FALSE, priority = 100)
  outputOptions(output, "standingsTableW", suspendWhenHidden = FALSE, priority = 101)
  outputOptions(output, "standingsTableAll", suspendWhenHidden = FALSE, priority = 90)
  outputOptions(output, "WhoCanWinPlotM", suspendWhenHidden = FALSE, priority = 80)
  outputOptions(output, "ScoreHistogramsM", suspendWhenHidden = FALSE, priority = 80)
  outputOptions(output, "WhoCanWinPlotM", suspendWhenHidden = FALSE, priority = 50)
  outputOptions(output, "WhoCanWinPlotW", suspendWhenHidden = FALSE, priority = 50)
  outputOptions(output, "H2HPlotM", suspendWhenHidden = FALSE, priority = 40)
  outputOptions(output, "H2HPlotW", suspendWhenHidden = FALSE, priority = 40)
  outputOptions(output, "ScoreHistogramsM", suspendWhenHidden = FALSE, priority = 30)
  outputOptions(output, "ScoreHistogramsW", suspendWhenHidden = FALSE, priority = 30)

  Sys.sleep(1)
  waiter::waiter_hide()

})
