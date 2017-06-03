## ui.R ##
library(shinydashboard)
library(DT)

#http://www.palettable.io/FA9C1E-0AA9E1-FF008C-BABABA

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    includeCSS("www/app.css"),
    singleton(
      tags$head(tags$script(src = "main.js"))
    ),
    uiOutput("textbox_ui"),
    uiOutput("newInputs"),
    radioButtons('type', 'Game Mode:', choices=c('comp', 'qp'), selected = 'comp'),
   # htmlOutput("players"),
    numericInput(inputId='damage', label="# DPS", value=2, min=0, max=6, step=1),
    numericInput(inputId='tank', label="# Tank", value=2, min=0, max=6, step=1),
    numericInput(inputId='support', label="# Support", value=2, step=1, max=6),
    numericInput(inputId='defense', label="# Defense", value=0, step=1, max=6),
    numericInput(inputId='min_gp', label='Min Games Played:',value=5),
    #selectInput(inputId = 'objective', label='Optimize by: ', choices=c('Medals Per Game', 'Objective Time', 'On Fire Per Game', 'Gold Medals Per Game', 'Healing + Damage', 'Healing + Elims')),
    selectInput(inputId = 'hero', label='Select Hero to Compare Users: ', choices=sort(c(heroes)), selected = 'dva'),
    textInput(inputId='newPlayer', 'BattleTag:', value='tcash21#1211'),
    actionButton("add_player", "Add Player"),
    actionButton('reset2', 'Reset'),
   uiOutput('about')
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel('Team Optimizer',
               DT::dataTableOutput('table1'),
               DT::dataTableOutput('table2')
      ),
      tabPanel('Hero Ranker',
               DT::dataTableOutput('table3')
      )
    )
  )
)