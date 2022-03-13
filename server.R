
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://www.rstudio.com/shiny/

library(shiny)
library(ggplot2)
library(dplyr)
library(dendextend)
# library(dendextendRcpp)
library(d3heatmap)

source("Tourny.R")
source("Loaders.R")
source("WhoCanWin.R")

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

regionChoices <- function(region, bracket=Bracket) {
  Region <- Bracket %>%
    rename(regn = region) %>%
    filter(regn == region) %>%
    rename(region = regn)

  res <- as.character(Region$team)
  names(res) <- paste0(
    Region$seed, " ", Region$team,
    " [", Region$cost, " pt", ifelse(Region$cost==1, "", "s"),"]")
  res
}

head2head_byindex <- Vectorize( function(i, j) head2head(F[[i]]$teams, F[[j]]$teams, Bracket, Games) )

### Read in some data

Bracket <<- LoadBracket()


shinyServer(function(input, output, session) {

  rValues <- reactiveValues(newLogEntries = 0)

  GetEntries <-
    reactivePoll(
      3000,
      session = session,
      function() file.mtime("data/Entries/"),
      function() {
        LoadEntries(
          year = the_year()
          )
      }
    )


  GetGameScores <-
    reactivePoll(
      1000,
      session = session,
      function() file.mtime("data/Scores"),
      LoadGameScores
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


  LogEntries <-
    reactiveFileReader(
      2000, session = session,
      filePath = "NCAA.log",
      function(path) {
        logData <- read.csv(path, as.is=TRUE)
        names(logData) <- c("time", "event", "session")
        logData[rev(seq_len(nrow(logData))), ]
      }
    )

  output$logTable <-
  renderDataTable(
    options=list(pageLength = 20,                     # initial number of records
                 lengthMenu=c(10,20,50,100),          # records/page options
                 lengthChange=1,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 ordering = TRUE,
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),   {
      # rValues$newLogEntries
      # logData <- read.csv("NCAA.log", as.is=TRUE)
      # names(logData) <- c("time", "event")
      # logData[rev(seq_len(nrow(logData))), ]
      LogEntries() %>%
        mutate(time = lubridate::ymd_hms(time) - lubridate::hours(4))
    })

  query <- reactive( parseQueryString(session$clientData$url_search) )

  ########## Bracket ###############

  # updates when games scores change
  GetBracketWithTeamStatus <- reactive({
    addTeamStatus(Bracket, scheduledGames(bracket=Bracket, results = GetGameScores()))
  })

  output$CostTable1 <- renderDataTable(
    options=list(lengthChange=0,                      # show/hide records per page dropdown
                 searching=0,                         # global search box on/off
                 info=0,                              # information on/off (how many records filtered, etc)
                 paging=FALSE,
                 ordering = FALSE,
                 autoWidth=TRUE                       # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),  {
    SeedTable <-
      Bracket %>% group_by(seed) %>%
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
      Bracket %>% group_by(seed) %>%
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

  ########## Game Results ##########
  # updates when a new score is saved (button)
  observeEvent( input$saveScoreButton, {
    if (as.numeric(input$saveScoreButton) > 0) {
      gts <- as.numeric(input$gameToScore)
      hs <- as.numeric(input$hscore)
      as <- as.numeric(input$ascore)
      home <- homeTeam(gts, Bracket, GetGameScores())
      away <- awayTeam(gts, Bracket, GetGameScores())
      createLogEntry(paste("Score enterred:", away, "vs.", home, as, "-", hs))
      write.csv(
        tibble(home = home, away = away, hscore = hs, ascore = as),
        row.names = FALSE,
        file = paste0("data/Scores/Game-",gsub("/"," or ", home),"-", gsub("/", " or ", away),
                      "-", humanTime(), ".csv")
      )
    }
  })

  GetCompletedGames <- reactive({
    compGames <- completedGames(GetGameScores(), Bracket)
    if (nrow(compGames) < 1) return(compGames)
    compGames %>%
      arrange(game) %>%
      select(game, winner, loser, score)
  })


  ############ Select Teams and Store Entry ###########
  Teams <- reactive(c( input$region1, input$region2, input$region3, input$region4 ))

  regions <- unique(Bracket$region)

  output$RegionName1 <- renderText({regions[1]})
  output$RegionName2 <- renderText({regions[2]})
  output$RegionName3 <- renderText({regions[3]})
  output$RegionName4 <- renderText({regions[4]})

  output$TeamsSelector1 <- renderUI({
    checkboxGroupInput("region1","",regionChoices(regions[1], Bracket))
  })

  output$TeamsSelector2 <- renderUI({
    checkboxGroupInput("region2","",regionChoices(regions[2], Bracket))
  })

  output$TeamsSelector3 <- renderUI({
    checkboxGroupInput("region3","",regionChoices(regions[3], Bracket))
  })

  output$TeamsSelector4 <- renderUI({
    checkboxGroupInput("region4","",regionChoices(regions[4], Bracket))
  })

  # timeOfLastEntry <- reactive( lastTimeStamp )
  # entryStatus <- renderText( paste("Most Recent Entry: ", timeOfLastEntry() ) )

  totalTeams <- reactive( length(Teams()) )
  output$totalTeams <- renderText(paste("Number of Teams:",totalTeams()))

  pointsSpent <- reactive( sum( filter(Bracket, team %in% Teams())$cost ) )
  output$pointsSpent <- reactive(pointsSpent())
  outputOptions(output, "pointsSpent", suspendWhenHidden = FALSE)

  pointsRemaining <- reactive( maxPoints - pointsSpent() )
  output$pointsRemaining <- renderText(paste("Points Remaining:", pointsRemaining()))

  output$year <- renderText(paste(the_year()))

  output$manyTeams <- reactive(length(Teams()) > 3)
  output$spendMessage<- renderText({
    if (pointsSpent() < maxPoints) {
      "Choose a team, you've got points to spend."
    } else if  (pointsSpent() > maxPoints ) {
      "You have overspent.  Please remove a team."
    } else {
      "You have spent all of your points."
    }
  })

  output$confirmation <- renderText( {
    if (input$submitButton > 0) {
      lastTimeStamp <<- Sys.time()
#      output$statusMessage <- renderText({paste("Last entry submited at", lastTimeStamp)})
      NewEntry <-
      isolate(
        list( name= input$name,
              email=input$email,
              dept=input$dept,
              points = pointsSpent(),
              teams = Teams(),
              teamsLogical=sapply(GetBracketWithTeamStatus()$team, function(x) x %in% Teams()),
              time=lastTimeStamp)
      )
      saveRDS(NewEntry, file=paste0("data/Entries/Entry-",
                                    humanTime(),
                                    "-",    # was missing when 2016 entries were posted.
                                    digest::digest(NewEntry),
                                    ".rds"))
      createLogEntry(paste("Entry submitted for", isolate(input$email)))
#      Entries <<- LoadEntries()
    }

    if (input$submitButton < 1) {
      ""
    } else {
      paste0("Thank you, ", isolate(input$name), ", your selections have been submitted.  ",
             "You spent ", isolate(pointsSpent()), " points on the following ", isolate(length(Teams())),
             " teams: ", paste( isolate(Teams()), collapse=", " )
      )
    }
  }
  )

  output$numEntries <- reactive( length(GetEntries()) )

  ############ Dowload Data ###########
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

  output$showStandings <- reactive({
    nrow(GetCompletedGames()) > 0 && length(GetEntries()) > 0
  } )
  outputOptions(output, "showStandings", suspendWhenHidden = FALSE)

  output$showScores <- reactive({
    nrow(GetCompletedGames()) > 0
  } )
  outputOptions(output, "showScores", suspendWhenHidden = FALSE)


  ############ Score Updates ###########
  output$password <- renderPrint({input$password})

  output$gameScoreSelector <- renderUI({
    games <- allGames(Bracket, GetGameScores())
    selectInput("gameToScore", "Choose a Game", choices = games, selectize=FALSE)
  })

  output$gameToScoreText <- renderText({
    paste0("About to give score for game ", input$gameToScore, ": ",
           awayTeam(as.numeric(input$gameToScore), Bracket, GetGameScores()), " vs. ",
           homeTeam(as.numeric(input$gameToScore), Bracket, GetGameScores())
    )
  })

  output$homeTeamScore <- renderUI({
    numericInput("hscore", step=0,
                 label = homeTeam(as.numeric(input$gameToScore), Bracket, GetGameScores()),
                 value = homeScore(as.numeric(input$gameToScore), Bracket, GetGameScores()))
  })

  output$awayTeamScore <- renderUI({
    numericInput("ascore", step=0,
                 label = awayTeam(as.numeric(input$gameToScore), Bracket, GetGameScores()),
                 value = awayScore(as.numeric(input$gameToScore), Bracket, GetGameScores()))
  })

  output$scoreSavedText <- renderText({
    # don't react to these until the button is pushed
    if(TRUE) {
      input$saveScoreButton
      isolate(gts <- as.numeric(input$gameToScore))
      isolate(hs <- as.numeric(input$hscore))
      isolate(as <- as.numeric(input$ascore))

      home <- homeTeam(gts, Bracket, GetGameScores())
      away <- awayTeam(gts, Bracket, GetGameScores())

      # this will react each time the save score button is pressed.
      if (input$saveScoreButton > 0)
        paste0("Score saved for game ", gts, ": ", away, " ", as, " - ", home, " ", hs )
      else "Select a game and enter scores above."
    } else {
      "sample text."
    }
  })

  ############ Query Stuff #############
  output$queryText <- renderText({
    # Return a string with key-value pairs
    paste(names(query()), query(), sep = "=", collapse=", ")
  })



  ############# Game Scores Table #####################

  output$ScoresTable <- renderDataTable(
    options=list(pageLength = 63,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 ordering = FALSE,
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),  {
      GetCompletedGames()
    })

  ############# Results Table #####################
  ResultsDF <- reactive({
    resultsTable(GetEntries(), GetBracketWithTeamStatus(), possibleMatchups(Bracket))
  })

  output$ResultsTable <- renderDataTable(
    options=list(pageLength = 35,                     # initial number of records
                 lengthMenu=c(5,10,25,50),            # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ), {
#      GetGameScores()
#      resultsTable(GetEntries(), GetBracketWithTeamStatus(), possibleMatchups(Bracket))
      ResultsDF()
    })

  ######### reactive text messages #########
  output$tournyStatus <- renderText({
    games <- nrow(GetCompletedGames()) # sum(GetBracketWithTeamStatus()$wins, na.rm=TRUE)
    paste0("Data based on ", length(GetEntries()), " contestants and ", games, " games.")
  })

  ######### (basketball) team data table #########

  GetTeamData <- reactive({
    teamData(GetEntries(), GetBracketWithTeamStatus())
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
    GetTeamData()
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


  H2H <- reactive({
    E <- GetEntries()
    G <- GetGameScores()
    B <- GetBracketWithTeamStatus()
    head2head_byindex <-
      Vectorize( function(i, j) head2head(E[[i]]$teams, E[[j]]$teams, Bracket, G) )
    n <- length(E)
    best <- t(outer(input$oneEntrant, 1:n, head2head_byindex))
    bestText <- ifelse(best > 0, paste("win by", best), ifelse (best == 0, "tie", paste("lose by", -best)))
    worst <- outer(1:n, input$oneEntrant, head2head_byindex)
    worstText <- ifelse(worst > 0, paste("lose by", worst), ifelse (worst == 0, "tie", paste("win by" , -worst)))
    tibble(
      `other player` = sapply(E, function(x) x$name),
      `best head-to-head result` = as.vector(bestText),
      `worst head-to-head result` = as.vector(worstText),
      `plus teams`  = sapply(E, function(x)
          intersect( B[B$alive, "team"], setdiff(E[[input$oneEntrant]]$teams, x$teams))
        ),
        `minus teams` = sapply(E, function(x)
            intersect( B[B$alive, "team"], setdiff(x$teams, E[[input$oneEntrant]]$teams))
        ),
        `common teams` = sapply(E, function(x)
            intersect( B[B$alive, "team"], intersect(x$teams, E[[input$oneEntrant]]$teams))
        )
    ) %>%
      cbind( ResultsDF() ) %>%
      arrange(- score) %>%
      select(1:6, `current score of other player` = score)
  })

  output$H2HTable <- renderDataTable(
    options=list(pageLength = 100,                    # initial number of records
                 lengthMenu=c(25,50,100),             # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 ordering = TRUE,
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),
    {
    H2H()
  })

  Sweet16M <-
    reactiveFileReader(
      2000, session,
      "data/Sweet16winMatrix.rds",
      readRDS
    )

  Sweet16Standings <-
    reactiveFileReader(
      2000, session,
      "data/Sweet16Standings.rds",
      readRDS
    )

  output$WhoCanWinTable <- renderDataTable(
    options=list(pageLength = 100,                    # initial number of records
                 lengthMenu=c(25,50,100),             # records/page options
                 lengthChange=0,                      # show/hide records per page dropdown
                 searching=1,                         # global search box on/off
                 info=1,                              # information on/off (how many records filtered, etc)
                 ordering = TRUE,
                 autoWidth=1                          # automatic column width calculation, disable if passing column width via aoColumnDefs
    ),
    WhoCanWin(
      GetEntries(),
      M = Sweet16M(), Sweet16Standings(),
      results = (GetTeamData() %>% filter(Team %in% rownames(Sweet16M())))$Wins - 2
      ) %>%
      merge(ResultsDF(), by = "name") %>%
      select(name, `winning scenarios`, `win percent`,
             score, `guaranteed wins`, `max possible`,
             `losing scenarios`, `lose percent`)
  )

  output$dendroPlot <- renderD3heatmap({
    E <- GetEntries()
    M <- do.call(rbind, lapply(E, function(x) x$teamsLogical))
    rownames(M) <- sapply(E, function(x) x$name)
    D <- as.data.frame(M)
    D <- as.data.frame(lapply(D, function(x) as.numeric(x)))
    rownames(D) <- rownames(M)
    Rowv  <- D %>% dist(method = "euclidean") %>% hclust %>% as.dendrogram %>%
      set("branches_k_color", k = 5) %>% set("branches_lwd", 2) %>%
      ladderize()
    Colv  <- D %>% t %>% dist(method = "euclidean") %>% hclust %>% as.dendrogram %>%
      set("branches_k_color", k = 7) %>% set("branches_lwd", 2) %>%
      ladderize()
    d3heatmap(D, Rowv = Rowv, Colv = Colv, color="Blues")
  })

})
