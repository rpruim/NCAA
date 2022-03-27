
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
# library(d3heatmap)
library(shinycssloaders)
library(waiter)
library(plotly)

options(spinner.type = 5, spinner.color = "steelblue", spinner.size = 1.5)

shinyUI(
  fluidPage(
    useWaiter(),
    waiterShowOnLoad(),
    titlePanel("NCAA Modeling Contest"),
    tabsetPanel(
      id = "Tabset",
      selected = "Rules & History",
      tabPanel(
        "Rules & History",
        h2("History"),
        p(paste(
          "Calvin faculty and staff have been participating in this NCAA modeling contest since 1995",
          "when Mike Stob introduced the contest.  In his honor, each year's winner is presented the",
          "traveling Mike Stob Trophy which they display proudly until presenting it to the next winner.",
          "Past winners are listed at the bottom of this page.")),
        p(paste(
          "The Clarence Menninga Trophy was named after the first person to win it twice and is",
          "presented each year to the contestant with the lowest score.",
          "The recipients of this trophy are not listed at the bottom of this page."
        )),
        h2("Official Rules"),
        tags$ol(
          tags$li("Each team has an integer cost determined by its seed in its region (see table below).  Periodically costs are revised.  The most recent revision was in 2019."),
          tags$li("Each contestant selects as many teams as he/she wants with costs totaling at most 200."),
          tags$li("Each contestant receives one point for each win by a team that he/she selects."),
          tags$li("The four play-in games do not count -- you buy the spot in the bracket and get the team that wins the play-in game but only score points starting with the round of 64."),
          tags$li("The contestant with the most points wins."),
          tags$li("In case of a tie, the winner is the contestant who scores the \"most points in
                  the latest round\". That is, the first tie-breaker is choosing the champion.
                  The second tie-breaker is choosing the second place team.
                  The third tie-breaker is number of semifinalists chosen. And so forth."),
          tags$li("Entries are due by the noon Eastern Time on the first day of the round of 64.")
        ),
        fluidRow(
          column(1),
          column(4,  dataTableOutput("CostTable1")) |> withSpinner(),
          column(2),
          column(4,  dataTableOutput("CostTable2")) |> withSpinner(),
          column(1)
        ),
        h2("Past Winners"),
        fluidRow(
          column(3),
          column(6,  dataTableOutput("PastWinners")) |> withSpinner(),
          column(3)
        )
      ),
      tabPanel(
        "Choose Wisely",
        br(),
        br(),
        conditionalPanel(
          condition = '! output.acceptingEntries',
          p(paste(
            "We are not accepting entries at this time either because the tournament brackets have",
            "not yet been posted or because the tournament has begun."))
        ),
        conditionalPanel(
          condition = 'output.showEntryForm && output.acceptingEntries',
          span(paste("Welcome to the NCAA modeling competition.",
                     "Please enter your name, email address, and department, and then",
                     "select at most 200 points worth of teams in each bracket.")),
          br(),
          br(),
          fluidRow(
            column(3, textInput("name", "Name:", "")),
            column(3,
                   textInput("email", "e-mail:", ""),
                   p("One entry per email address.")
            ),
            column(3, selectInput("dept", "Department:",
                                  c("Select a department","Admin","Bio","Chem/BioCh","CS","Engr","GEO","Math/Stat","Nursing","Phys/Ast","Psych","Other"))),
            column(3,
                   h3(""),
                   conditionalPanel(
                     condition = 'output.pointsSpentM <= 200 && output.pointsSpentW <= 200 && input.name.length > 2 && input.email.length > 2',
                     column(3, actionButton("submitButton", "Submit Teams"))
                   )
            )
          ),
          fluidRow(
            column(9,
                   conditionalPanel(
                     condition = "input.name.length < 3 || input.email.length < 3",
                     helpText("The submit button won't appear until you have entered a name and email address.  Only one submission is allowed per email address. Each new submission will replace any previous submissions.")
                   )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Women's Tournament",
              fluidRow(
                column(6,
                       h4(textOutput("pointsRemainingW")),
                       helpText(textOutput("spendMessageW"))
                )
              ),  # fluid row
              br(),
              hr(),
              fluidRow(
                column(3, h4(textOutput("RegionNameW1")), uiOutput("TeamsSelectorW1")),
                column(3, h4(textOutput("RegionNameW2")), uiOutput("TeamsSelectorW2")),
                column(3, h4(textOutput("RegionNameW3")), uiOutput("TeamsSelectorW3")),
                column(3, h4(textOutput("RegionNameW4")), uiOutput("TeamsSelectorW4"))
              )
            ),  # end women's tab panel
            tabPanel(
              "Men's Tournament",
              fluidRow(
                column(6,
                       h4(textOutput("pointsRemainingM")),
                       helpText(textOutput("spendMessageM"))
                )),  # fluid row
              br(),
              hr(),
              fluidRow(
                column(3, h4(textOutput("RegionNameM1")), uiOutput("TeamsSelectorM1")),
                column(3, h4(textOutput("RegionNameM2")), uiOutput("TeamsSelectorM2")),
                column(3, h4(textOutput("RegionNameM3")), uiOutput("TeamsSelectorM3")),
                column(3, h4(textOutput("RegionNameM4")), uiOutput("TeamsSelectorM4"))
              )
            )  # end men's tab panel
          )  # end men/women tabset
        ), # conditional panel
        conditionalPanel(
          condition = '! output.showEntryForm && output.acceptingEntries',
          br(),
          uiOutput("confirmation"),
          br(),
          actionButton("reviseButton", "Revise my entry")
        ) # conditional panel
      ),  # tabPanel choose wisely

      tabPanel(
        "Scores",
        br(),
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Women's Bracket",
            br(),br(),
            conditionalPanel(
              condition = '! output.showStandingsW',
              helpText("Information will display here after the first tournament results are in and loaded.")
            ),
            conditionalPanel(
              condition = 'true || output.showStandingsW',
              dataTableOutput("ScoresTableW") |> withSpinner()
            )
          ),
          tabPanel(
            "Men's Bracket",
            br(),br(),
            conditionalPanel(
              condition = '! output.showStandingsM',
              helpText("Information will display here after the first tournament results are in and loaded.")
            ),
            conditionalPanel(
              condition = 'true || output.showStandingsM',
              dataTableOutput("ScoresTableM") |> withSpinner()
            )
          ) # tabPanel
        ) # tabsetPanel
      ), # tabPanel Scores

      tabPanel(
        "Standings",
        br(),
        tabsetPanel(
          type = "pills",
          tabPanel(
            "Women's Bracket",
            br(),br(),
            conditionalPanel(
              condition = '! output.showStandingsW',
              helpText("Standings will be displayed after the first tournament results are in and loaded.")
            ),
            conditionalPanel(
              condition = 'output.showStandingsW',
              strong(textOutput("tournyStatusW")),
              dataTableOutput("standingsTableW") |> withSpinner()
            )
          ),
          tabPanel(
            "Men's Bracket",
            br(),br(),
            conditionalPanel(
              condition = 'true', # 'output.contestStandingsReady',
              conditionalPanel(
                condition = '! output.showStandingsM',
                helpText("Standings will be displayed after the first tournament results are in and loaded.")
              ),
              conditionalPanel(
                condition = 'output.showStandingsM',
                strong(textOutput("tournyStatusM")),
                dataTableOutput("standingsTableM") |> withSpinner()
              )
            )
          ),
          tabPanel(
            "Combined",
            br(),br(),
            conditionalPanel(
              condition = '! output.showStandingsW || ! output.showStandingsM',
              helpText("Standings will be displayed after the first tournament results are in and loaded.")
            ),
            conditionalPanel(
              condition = 'output.showStandingsW && output.showStandingsM',
              # strong(textOutput("tournyStatusW")),
              dataTableOutput("standingsTableAll") |> withSpinner()
            )
          )
        )
      ),

      tabPanel(
        "Crystal Ball",
        conditionalPanel(
          condition = 'true',
          tabsetPanel(
            type = "pills",
            tabPanel(
              "Women's Bracket",
              h3("Who can win?"),
              plotOutput('WhoCanWinPlotW') |> withSpinner(),
              br(),
              h3('Head to Head'),
              p('Read across rows for wins. Read up columns for losses. A red column indicates that some has clinced victory. A red row, that someone has clinced defeat.'),
              plotlyOutput('H2HPlotW', height = "600px") |> withSpinner(),
              br(),
              h3('Score Histograms'),
              plotOutput('ScoreHistogramsW') |> withSpinner(),
              br()
            ),
            tabPanel(
              "Men's Bracket",
              h3("Who can win?"),
              plotOutput('WhoCanWinPlotM') |> withSpinner(),
              br(),
              h3('Head to Head'),
              p('Read across rows for wins. Read up columns for losses. A red column indicates that some has clinced victory. A red row, that someone has clinced defeat.'),
              plotlyOutput('H2HPlotM', height = "600px") |> withSpinner(),
              br(),
              h3('Score Histograms'),
              plotOutput('ScoreHistogramsM') |> withSpinner(),
              br()
            )
          )
        )
      ), # crystal ball panel,

      tabPanel(
        "Admin",
        fluidRow(
          column(6,
                 conditionalPanel(
                   condition = '!output.showAdminTab',
                   helpText("If you have administrative access, you should know how to unlock the door."),
                   helpText("If not, this tab will be pretty boring."),
                   br(), br(),
                   strong("Commissioner:"), span("R Pruim"), br(),
                   strong("Scoremaster:"), span("R Bebej"), br(),
                   strong("Honorary Commissioner & Historian:"), span("M Stob"), br()
                 ),
                 conditionalPanel(
                   condition = 'output.showAdminTab',
                   textInput("passwd", label = h3("Access Code"), value = ""),
                   # h3("System Log"),
                   # dataTableOutput("logTable") |> withSpinner()
                 ) # conditionalPanel
          ),  # column
          column(
            6,
            conditionalPanel(
              condition = 'output.showGameEntry',
              h3("Enter Game Results"),
              tabsetPanel(
                id = "gameScores",
                type = "tabs",
                tabPanel(
                  "Women's",
                  id = "gameScoresW",
                  uiOutput("gameScoreSelectorW") |> withSpinner(),
                  uiOutput("awayTeamScoreW"),
                  uiOutput("homeTeamScoreW"),
                   actionButton("saveScoreButtonW", "Submit Score"),
                   textOutput("scoreSavedTextW")
                ),
                tabPanel(
                  "Men's",
                  id = "gameScoresM",
                  uiOutput("gameScoreSelectorM") |> withSpinner(),
                  uiOutput("awayTeamScoreM"),
                  uiOutput("homeTeamScoreM"),
                  actionButton("saveScoreButtonM", "Submit Score"),
                ), # tabPanel
                tabPanel(
                  "Other stuff",
                  id = 'otherStuff',
                  br(), br(),
                  actionButton("reCacheButton", "Refresh Crystal Ball Cache"),
                ),
              ) # tabsetPanel
            ) # conditionalPanel
          ) # column
        ) # fluidRow
      ) # tabPanel
    ) # tabsetPanel
  ) # fluidPage
) # shinyUI
