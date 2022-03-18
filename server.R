
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/

library(shiny)
library(dplyr)
library(madness)

source("../Tourny.R")
source("../Loaders.R")

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

### Read in some data

BracketM <<- LoadBracket('data/bracket2022.csv')
BracketW <<- LoadBracket('data/bracket2022w.csv')

TMinit <<- tournament_init(names = BracketM[['team']], seeds = BracketM[['seed']], label = "M")
TWinit <<- tournament_init(names = BracketW[['team']], seeds = BracketW[['seed']], label = "W")

Entries <<- load_entries_from_files(TMinit, path = "data/Entries/2022/", year = 2022)

EM <<- build_entry_matrix(Entries, ext = "M")
EW <<- build_entry_matrix(Entries, ext = "W")


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

regionChoices <- function(region, bracket=BracketM) {
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

# head2head_byindex <- Vectorize( function(i, j) head2head(F[[i]]$teams, F[[j]]$teams, BracketM, Games) )


shinyServer(function(input, output, session) {

  rValues <- reactiveValues(newLogEntries = 0)

  GetEntries <-
    reactivePoll(
      3000,
      session = session,
      function() file.mtime("data/Entries/"),
      function() {
        load_entries_from_files(   # was LoadEntries
          path = "data/Entries/",
          year = the_year()
          )
      }
    )


  GetGameScoresM <-
    reactivePoll(
      1000,
      session = session,
      function() file.mtime("data/Scores/Mens"),
      function() {LoadGameScores("data/Scores/2022/Mens/", pattern = "M-.*2022.*\\.csv")}
    )

  GetGameScoresW <-
    reactivePoll(
      1000,
      session = session,
      function() file.mtime("data/Scores/Womens"),
      function() {LoadGameScores("data/Scores/2022/Womens/", pattern = "W-.*2022.*\\.csv")}
    )


  createLogEntry <- function(text) {
    isolate( rValues$newLogEntries <- rValues$newLogEntries + 1 )
    write(paste0(Sys.time(), ',"', text, '",', sessionID(session)),
          append=TRUE, file="NCAA.log")
    output$statusMessage <- renderText({ paste(Sys.time(), text, sep=": ") })
  }

  session$onSessionEnded(function() {
    createLogEntry("Sesssion ended.")
  })

  createLogEntry("New session started.")


  # LogEntries <-
  #   reactiveFileReader(
  #     2000, session = session,
  #     filePath = "NCAA.log",
  #     function(path) {
  #       logData <- read.csv(path, as.is=TRUE)
  #       names(logData) <- c("time", "event", "session")
  #       logData[rev(seq_len(nrow(logData))), ]
  #     }
  #   )

  # output$logTable <-
  # renderDataTable(
  #   options=list(pageLength = 20,                     # initial number of records
  #                lengthMenu=c(10,20,50,100),          # records/page options
  #                lengthChange=1,                      # show/hide records per page dropdown
  #                searching=1,                         # global search box on/off
  #                info=1,                              # information on/off (how many records filtered, etc)
  #                ordering = TRUE,
  #                autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
  #   ),   {
  #     # rValues$newLogEntries
  #     # logData <- read.csv("NCAA.log", as.is=TRUE)
  #     # names(logData) <- c("time", "event")
  #     # logData[rev(seq_len(nrow(logData))), ]
  #     LogEntries() %>%
  #       mutate(time = lubridate::ymd_hms(time) - lubridate::hours(4))
  #   })

  query <- reactive( parseQueryString(session$clientData$url_search) )

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
      BracketM %>% group_by(seed) %>%
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
      BracketM %>% group_by(seed) %>%
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
    History <- read.csv("data/historical-winners.csv", header=TRUE)
    History %>% arrange(year, winner)
  })

  ############ Select Teams and Store Entry ###########
  TeamsM <- reactive(c( input$regionM1, input$regionM2, input$regionM3, input$regionM4 ))
  TeamsW <- reactive(c( input$regionW1, input$regionW2, input$regionW3, input$regionW4 ))

  regionsM <- unique(BracketM$region)
  regionsW <- unique(BracketW$region)

  output$RegionNameM1 <- renderText({regionsM[1]})
  output$RegionNameM2 <- renderText({regionsM[2]})
  output$RegionNameM3 <- renderText({regionsM[3]})
  output$RegionNameM4 <- renderText({regionsM[4]})

  output$RegionNameW1 <- renderText({regionsW[1]})
  output$RegionNameW2 <- renderText({regionsW[2]})
  output$RegionNameW3 <- renderText({regionsW[3]})
  output$RegionNameW4 <- renderText({regionsW[4]})

  output$TeamsSelectorM1 <- renderUI({
    checkboxGroupInput("regionM1","",regionChoices(regionsM[1], BracketM))
  })

  output$TeamsSelectorM2 <- renderUI({
    checkboxGroupInput("regionM2","",regionChoices(regionsM[2], BracketM))
  })

  output$TeamsSelectorM3 <- renderUI({
    checkboxGroupInput("regionM3","",regionChoices(regionsM[3], BracketM))
  })

  output$TeamsSelectorM4 <- renderUI({
    checkboxGroupInput("regionM4","",regionChoices(regionsM[4], BracketM))
  })

  output$TeamsSelectorW1 <- renderUI({
    checkboxGroupInput("regionW1","",regionChoices(regionsW[1], BracketW))
  })

  output$TeamsSelectorW2 <- renderUI({
    checkboxGroupInput("regionW2","",regionChoices(regionsW[2], BracketW))
  })

  output$TeamsSelectorW3 <- renderUI({
    checkboxGroupInput("regionW3","",regionChoices(regionsW[3], BracketW))
  })

  output$TeamsSelectorW4 <- renderUI({
    checkboxGroupInput("regionW4","",regionChoices(regionsW[4], BracketW))
  })

  # timeOfLastEntry <- reactive( lastTimeStamp )
  # entryStatus <- renderText( paste("Most Recent Entry: ", timeOfLastEntry() ) )

  totalTeamsM <- reactive( length(TeamsM()) )
  totalTeamsW <- reactive( length(TeamsW()) )

  output$totalTeamsM <- renderText(paste("Number of Teams:",totalTeamsM()))
  output$totalTeamsW <- renderText(paste("Number of Teams:",totalTeamsW()))

  pointsSpentM <- reactive( sum( filter(BracketM, team %in% TeamsM())$cost ) )
  pointsSpentW <- reactive( sum( filter(BracketW, team %in% TeamsW())$cost ) )

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
              teamsLogical = sapply(BracketM$team, function(x) x %in% TeamsM()),
              teamsLogicalW = sapply(BracketW$team, function(x) x %in% TeamsW()),
              time = lastTimeStamp)
      )
      saveRDS(NewEntry, file=paste0("data/Entries/2022/Entry-",
                                    humanTime(),
                                    "-",    # was missing when 2016 entries were posted.
                                    digest::digest(NewEntry),
                                    ".rds"))
      createLogEntry(paste("Entry submitted for", isolate(input$email)))
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

  output$numEntries <- reactive( length(GetEntries()) )

  ############ Download Data ###########
  output$downloadData <- downloadHandler(
    filename = function() { "Entries.rds" },
    content = function(file) {
      saveRDS(GetEntries(), file)
    }
  )

  ########### Turn Controls on and off ############
  output$showDownloadButton <- reactive({
    "download" %in% names(query())
  } )
  outputOptions(output, "showDownloadButton", suspendWhenHidden = FALSE)

  output$showEntryForm <- reactive({
    ( as.numeric(input$submitButton) + as.numeric(input$reviseButton) ) %% 2 == 0
  })
  outputOptions(output, "showEntryForm", suspendWhenHidden = FALSE)

  output$acceptingEntries <- reactive({
    tolower(query()["admin"]) %in% c("yes","y") ||
      (file.exists(bracketFile) &&
         Sys.time() < lubridate::ymd_hm(deadline) + lubridate::hours(5) )
  })
  outputOptions(output, "acceptingEntries", suspendWhenHidden = FALSE)

  output$showAdminTab <- reactive({
    tolower(query()["admin"]) %in% c("yes", "y")
  })
  outputOptions(output, "showAdminTab", suspendWhenHidden = FALSE)

  output$showGameEntry <- reactive({
    tolower(as.character(input$passwd)) == "madly marching"
  })
  outputOptions(output, "showGameEntry", suspendWhenHidden = FALSE)

  output$showStandingsM <- reactive({
    nrow(GetCompletedGamesM()) > 0 && nrow(EM) > 0
  } )
  output$showStandingsW <- reactive({
    nrow(GetCompletedGamesW()) > 0 && nrow(EM) > 0
  } )
  outputOptions(output, "showStandingsM", suspendWhenHidden = FALSE)
  outputOptions(output, "showStandingsW", suspendWhenHidden = FALSE)

  output$showScoresM <- reactive({
    nrow(GetCompletedGamesM()) > 0
  } )
  output$showScoresW <- reactive({
    nrow(GetCompletedGamesW()) > 0
  } )
  outputOptions(output, "showScoresM", suspendWhenHidden = FALSE)
  outputOptions(output, "showScoresW", suspendWhenHidden = FALSE)


  ############ Score Updates ###########
  output$password <- renderPrint({input$password})

  output$gameScoreSelectorM <- renderUI({
    games <- allGames(BracketM, GetGameScoresM())
    selectInput("gameToScoreM", "Choose a Game", choices = games, selectize=FALSE)
  })

  output$gameScoreSelectorW <- renderUI({
    games <- allGames(BracketW, GetGameScoresW())
    selectInput("gameToScoreW", "Choose a Game", choices = games, selectize=FALSE)
  })

  output$gameToScoreTextM <- renderText({
    paste0("About to give score for game ", input$gameToScoreM, ": ",
           awayTeam(as.numeric(input$gameToScoreM), BracketM, GetGameScoresM()), " vs. ",
           homeTeam(as.numeric(input$gameToScoreM), BracketM, GetGameScoresM())
    )
  })
  output$gameToScoreTextW <- renderText({
    paste0("About to give score for game ", input$gameToScoreW, ": ",
           awayTeam(as.numeric(input$gameToScoreW), BracketW, GetGameScoresW()), " vs. ",
           homeTeam(as.numeric(input$gameToScoreW), BracketW, GetGameScoresW())
    )
  })

  output$homeTeamScoreM <- renderUI({
    numericInput("hscoreM", step=0,
                 label = homeTeam(as.numeric(input$gameToScoreM), BracketM, GetGameScoresM()),
                 value = homeScore(as.numeric(input$gameToScoreM), BracketM, GetGameScoresM()))
  })
  output$homeTeamScoreW <- renderUI({
    numericInput("hscoreW", step=0,
                 label = homeTeam(as.numeric(input$gameToScoreW), BracketW, GetGameScoresW()),
                 value = homeScore(as.numeric(input$gameToScoreW), BracketW, GetGameScoresW()))
  })

  output$awayTeamScoreM <- renderUI({
    numericInput("ascoreM", step=0,
                 label = awayTeam(as.numeric(input$gameToScoreM), BracketM, GetGameScoresM()),
                 value = awayScore(as.numeric(input$gameToScoreM), BracketM, GetGameScoresM()))
  })
  output$awayTeamScoreW <- renderUI({
    numericInput("ascoreW", step=0,
                 label = awayTeam(as.numeric(input$gameToScoreW), BracketW, GetGameScoresW()),
                 value = awayScore(as.numeric(input$gameToScoreW), BracketW, GetGameScoresW()))
  })

  output$scoreSavedTextM <- renderText({
    # don't react to these until the button is pushed
    if(TRUE) {
      input$saveScoreButtonM
      isolate(gts <- as.numeric(input$gameToScoreM))
      isolate(hs <- as.numeric(input$hscoreM))
      isolate(as <- as.numeric(input$ascoreM))

      home <- homeTeam(gts, BracketM, GetGameScoresM())
      away <- awayTeam(gts, BracketM, GetGameScoresM())

      # this will react each time the save score button is pressed.
      if (input$saveScoreButtonM > 0)
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

      home <- homeTeam(gts, BracketW, GetGameScoresW())
      away <- awayTeam(gts, BracketW, GetGameScoresW())

      # this will react each time the save score button is pressed.
      if (input$saveScoreButtonW > 0)
        paste0("Score saved for game ", gts, ": ", away, " ", as, " - ", home, " ", hs )
      else "Select a game and enter scores above."
    } else {
      "sample text."
    }
  })


  ########## Game Results ##########
  # updates when a new score is saved (button)
  observeEvent( input$saveScoreButtonM, {
    if (as.numeric(input$saveScoreButtonM) > 0) {
      gts <- as.numeric(input$gameToScoreM)
      hs <- as.numeric(input$hscoreM)
      as <- as.numeric(input$ascoreM)
      home <- homeTeam(gts, BracketM, GetGameScoresM())
      away <- awayTeam(gts, BracketM, GetGameScoresM())
      createLogEntry(paste("Score enterred:", away, "vs.", home, as, "-", hs))
      # Note for winner_01: 0 = home win; 1 = away win
      write.csv(
        tibble(game_number = gts, winner_01 = as.numeric(as > hs),
               home = home, away = away, hscore = hs, ascore = as),
        row.names = FALSE,
        file = paste0("data/Scores/2022/Mens/M-",gsub("/"," or ", home),"-", gsub("/", " or ", away),
                      "-", humanTime(), ".csv")
      )
    }
  })
  observeEvent( input$saveScoreButtonW, {
    if (as.numeric(input$saveScoreButtonW) > 0) {
      gts <- as.numeric(input$gameToScoreW)
      hs <- as.numeric(input$hscoreW)
      as <- as.numeric(input$ascoreW)
      home <- homeTeam(gts, BracketW, GetGameScoresW())
      away <- awayTeam(gts, BracketW, GetGameScoresW())
      createLogEntry(paste("Women's Score enterred:", away, "vs.", home, as, "-", hs))
      # Note for winner_01: 0 = home win; 1 = away win
      write.csv(
        tibble(game_number = gts, winner_01 = as.numeric(as > hs),
               home = home, away = away, hscore = hs, ascore = as),
        row.names = FALSE,
        file = paste0("data/Scores/2022/Womens/W-",gsub("/"," or ", home),"-", gsub("/", " or ", away),
                      "-", humanTime(), ".csv")
      )
    }
  })

  # observeEvent(input$MorW, {
  #   updateTabsetPanel(session, "gameScores", selected = paste0("gameScores", input$MorW))
  # })


  GetCompletedGamesM <- reactive({
    compGames <- completedGames(GetGameScoresM(), BracketM)
    if (nrow(compGames) < 1) return(compGames)
    compGames %>%
      arrange(game) %>%
      select(game, winner, loser, score)
  })
  GetCompletedGamesW <- reactive({
    compGames <- completedGames(GetGameScoresW(), BracketW)
    if (nrow(compGames) < 1) return(compGames)
    compGames %>%
      arrange(game) %>%
      select(game, winner, loser, score)
  })

  GetTM <- reactive({
    Scores <- GetGameScoresM()
    if (nrow(Scores) > 0) {
      TMinit |> tournament_update(games = Scores[['game_number']], results = Scores[['winner_01']])
    }
  })

  GetTW <- reactive({
    Scores <- GetGameScoresW()
    if (nrow(Scores) > 0) {
      TMinit |> tournament_update( games = Scores[['game_number']], results = Scores[['winner_01']])
    }
  })

  ########## Bracket ###############

  # updates when games scores change
  GetBracketWithTeamStatusM <- reactive({
    addTeamStatus(BracketM, scheduledGames(bracket=BracketM, results = GetGameScoresM()))
  })
  GetBracketWithTeamStatusW <- reactive({
    addTeamStatus(BracketW, scheduledGames(bracket=BracketW, results = GetGameScoresW()))
  })


  ############ Query Stuff #############
  output$queryText <- renderText({
    # Return a string with key-value pairs
    paste(names(query()), query(), sep = "=", collapse=", ")
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
      GetCompletedGamesM()
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
      GetCompletedGamesW()
    })

  ############# Results Table #####################
  ResultsDFM <- reactive({
    # resultsTable(GetEntries(), GetBracketWithTeamStatusM(), possibleMatchups(BracketM))
    contest_status(GetTM(), EM, BracketM)
  })
  ResultsDFW <- reactive({
    # resultsTable(GetEntries(), GetBracketWithTeamStatusM(), possibleMatchups(BracketM))
    contest_status(GetTW(), EW, BracketW)
  })


  output$ResultsTableM <- renderDataTable(
    options=list(pageLength = 35,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ResultsDFM()
    })

  output$ResultsTableW <- renderDataTable(
    options=list(pageLength = 35,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
      ResultsDFW()
    })

  ######### reactive text messages #########
  output$tournyStatusM <- renderText({
    games <- nrow(GetCompletedGamesM()) # sum(GetBracketWithTeamStatus()$wins, na.rm=TRUE)
    paste0("Data based on ", nrow(EM), " contestants and ", games, " games.")
  })
  output$tournyStatusW <- renderText({
    games <- nrow(GetCompletedGamesW()) # sum(GetBracketWithTeamStatus()$wins, na.rm=TRUE)
    paste0("Data based on ", nrow(EW), " contestants and ", games, " games.")
  })

  ######### (basketball) team data table #########

  GetTeamDataM <- reactive({
    teamData(GetEntries(), GetBracketWithTeamStatusM())
  })

  GetTeamDataW <- reactive({
    teamData(GetEntries(), GetBracketWithTeamStatusW())
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
    GetTeamDataM()
  )
      # E <- GetEntries()
      # M <- do.call(rbind, lapply( E, function(x) x$teamsLogical ) )
      # B <- GetBracketWithTeamStatus()
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
    entrants <- sapply(GetEntries(), function(x) x$email)
    names(entrants) <-
      paste0(
        sapply(GetEntries(), function(x) x$name),
        " [",
        ResultsDF()$score,
        "]"
      )
    selectInput("oneEntrant", "Select a player", choices = entrants[order(- ResultsDF()$score)], selectize=FALSE)
  })


  # H2H <- reactive({
  #   E <- GetEntries()
  #   G <- GetGameScoresM()
  #   B <- GetBracketWithTeamStatusM()
  #   head2head_byindex <-
  #     Vectorize( function(i, j) head2head(E[[i]]$teams, E[[j]]$teams, BracketM, G) )
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
  #     cbind( ResultsDF() ) %>%
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
  #     GetEntries(),
  #     M = Sweet16M(), Sweet16Standings(),
  #     results = (GetTeamData() %>% filter(Team %in% rownames(Sweet16M())))$Wins - 2
  #     ) %>%
  #     merge(ResultsDF(), by = "name") %>%
  #     select(name, `winning scenarios`, `win percent`,
  #            score, `guaranteed wins`, `max possible`,
  #            `losing scenarios`, `lose percent`)
  # )

  # output$dendroPlot <- renderD3heatmap({
  #   E <- GetEntries()
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

})
