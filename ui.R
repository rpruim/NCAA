
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
                                  c("Select a department","Admin","Bio","Chem","CS","Engr","GEO","Math/Stat","Nursing","Phys/Ast","Psych","Other"))),
            column(3,
                   h3(""),
                   conditionalPanel(
                     condition = "output.pointsSpentM <= 200 && output.pointsSpentW <= 200 && input.name.length > 2 && input.email.length > 2",
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
                       helpText(textOutput("spendMessageM")),
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
      )  # tabPanel choose wisely
    ) # tabsetPanel
  ) # fluidPage
) # shinyUI