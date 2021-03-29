
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(d3heatmap)

shinyUI(
  fluidPage(
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
          tags$li("The four play-in games do not count -- you buy the spot in the bracket and get the team that wins the play-in game but only score points starting with round 2."),
          tags$li("The contestant with the most points wins."),
          tags$li("In case of a tie, the winner is the contestant who scores the \"most points in
                  the latest round\". That is, the first tie-breaker is choosing the champion.
                  The second tie-breaker is choosing the second place team.
                  The third tie-breaker is number of semifinalists chosen. And so forth."),
          tags$li("Entries are due by the noon Eastern Time on the first day of the round of 64.")
        ),
        fluidRow(
          column(1),
          column(4,  dataTableOutput("CostTable1")),
          column(2),
          column(4,  dataTableOutput("CostTable2")),
          column(1)
        ),
        h2("Past Winners"),
        fluidRow(
          column(3),
          column(6,  dataTableOutput("PastWinners")),
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
          condition = "output.showEntryForm && output.acceptingEntries",
          span(paste("Welcome to the NCAA modeling competition.",
                     "Please enter your name, email address, and department, and then",
                     "select at most 200 points worth of teams.")),
          br(),
          br(),
          fluidRow(
            column(3, textInput("name", "Name:", "")),
            column(3,
                   textInput("email", "e-mail:", ""),
                   p("One entry per email address.")
                   ),
            column(3, selectInput("dept", "Department:",
                                  c("Select a department","Admin","Bio","Chem","CS","Engr","GEO","Math/Stat","Nursing","Phys/Ast","Psych","Other")))
          ),
          fluidRow(
            column(3,
                   conditionalPanel(
                     condition = "output.pointsSpent <= 200 && input.name.length > 2 && input.email.length > 2",
                     column(3, actionButton("submitButton", "Submit Teams"))
                   )),
            column(6,
                   h4(textOutput("pointsRemaining")),
                   helpText(textOutput("spendMessage")),
                   conditionalPanel(
                     condition = "input.name.length < 3 || input.email.length < 3",
                     helpText("The submit button won't appear until you have entered a name and email address.  Only one submission is allowed per email address. Each new submission will replace any previous submissions.")
                   )
            )
          ),  # fluid row
          br(),
          hr(),
          fluidRow(
            column(3, h4(textOutput("RegionName1")), uiOutput("TeamsSelector1")),
            column(3, h4(textOutput("RegionName2")), uiOutput("TeamsSelector2")),
            column(3, h4(textOutput("RegionName3")), uiOutput("TeamsSelector3")),
            column(3, h4(textOutput("RegionName4")), uiOutput("TeamsSelector4"))
          )
        ),
        conditionalPanel(
          condition = '! output.showEntryForm && output.acceptingEntries',
          br(),
          textOutput("confirmation"),
          br(),
          actionButton("reviseButton", "Revise my entry")
        )
      ),  # tabPanel choose wisely

      # tabPanel(
      #   "Scores",
      #   br(),br(),
      #   conditionalPanel(
      #     condition = '! output.showStandings',
      #     helpText("Information will display here after the first tournament results are in.")
      #   ),
      #   conditionalPanel(
      #     condition = 'true', # 'output.showStandings',
      #     dataTableOutput("ScoresTable")
      #   )
      # ), # tabPanel Scores

      tabPanel(
        "Team Data",
        br(),br(),
        conditionalPanel(
          condition = '! output.showStandings',
          helpText("Information will display here after the first tournament results are in.")
        ),
        conditionalPanel(
          condition = 'output.showStandings',
          dataTableOutput("teamData")
        )
      ), # tabPanel  Team Data

      tabPanel(
        "Standings",
        br(),br(),
        conditionalPanel(
          condition = '! output.showStandings',
          helpText("Standings will be displayed after the first tournament results are in.")
        ),
        conditionalPanel(
          condition = 'output.showStandings',
          strong(textOutput("tournyStatus")),
          dataTableOutput("ResultsTable")
        )
      ),

      # tabPanel(
      #   "Whom Can I Beat?",
      #    br(),br(),
      #   # strong(textOutput("tournyStatus")),
      #   # br(),br(),
      #   # conditionalPanel(
      #   #   condition = '! output.showStandings',
      #   #   helpText("Information will be displayed after the first tournament results are in.")
      #   # ),
      #   conditionalPanel(
      #     condition = 'output.showStandings',
      #     p("Select a player to see which other players that player could defeat.",
      #       "Note that even if a player can defeat all players head to head, it may not ",
      #       "be possible to win the modeling competition."),
      #   br(), br(),
      #   uiOutput("entrantSelector"),
      #   dataTableOutput("H2HTable"),
      #   br()
      #   )
      # ),


      tabPanel(
         "Who Can Win?",
         br(),br(),
         # conditionalPanel(
         #   condition = '! output.showStandings',
         #   helpText("Information will be displayed after the first tournament results are in.")
         # ),
         conditionalPanel(
           p("Note: The results below take into account tie breakers that can be resolved in the ",
             "final four rounds of the tournament, but do not take into account tie breakers that ",
             "go back into the first two rounds. ",
             "For tie breaker rules, see the official rules of the contest."),
           br(), br(),
           dataTableOutput("WhoCanWinTable"),
           br()
         )
       ),

      # tabPanel(
      #   "Group Photo",
      #   br(), br(),
      #   p("Below is a graphical representation of all of the entries in this year's contest.",
      #     "The plot below uses a Euclidean metric to cluster teams and entries."
      #     ),
      #   br(), br(),
      #   d3heatmapOutput("dendroPlot")
      # ),

      tabPanel(
        "Admin",
        fluidRow(
          column(6,
                 conditionalPanel(
                   condition = "!output.showAdminTab",
                   helpText("If you have administrative access, you should know how to unlock the door."),
                   helpText("If not, this tab will be pretty boring."),
                   br(), br(),
                   strong("Commissioner:"), span("R Pruim"), br(),
                   strong("Scoremaster:"), span("R Bebej"), br(),
                   strong("Honorary Commissioner & Historian:"), span("M Stob"), br()
                 ),
                 conditionalPanel(
                   condition = "output.showAdminTab",
                   textInput("passwd", label = h3("Access Code"), value = ""),
                   h3("System Log"),
                   dataTableOutput("logTable")
                 ) # conditionalPanel
          ),  # column
          conditionalPanel(
            condition = "output.showGameEntry",
            column(6,
                   h3("Enter Game Results"),
                   uiOutput("gameScoreSelector"),
                   uiOutput("awayTeamScore"),
                   uiOutput("homeTeamScore"),
                   actionButton("saveScoreButton", "Submit Score"),
                   textOutput("scoreSavedText")
            )
          ) # conditionalPanel
        ) # fluidRow
      ) # tabPanel Admin
    ) # tabsetPanel
  ) # fluidPage
) # shinyUI