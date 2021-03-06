library(DT)
library(plotly)
library(shinyFiles)
library(shinyjs)


.readYamlConfig <- function() {
  config <- example_strategy_config()
  config$from <- NULL
  config$to <- NULL
  
  yaml::as.yaml(config)
}

ui <- fluidPage(
  
  fluidRow(
    column(12,
      titlePanel("strand"),
      hr()
    )
  ),
  
  tabsetPanel(
    id = "top",
    type = "tabs",
    tabPanel("Configuration",
             br(),
             fluidRow(
               column(2,
                      dateInput("startDate", label = "Start date", value = "2020-06-01",
                                min = "2020-06-01", max = "2020-08-31",
                                daysofweekdisabled = c(0,6)),
                      dateInput("endDate", label = "End date", value = "2020-08-31",
                                min = "2020-06-01", max = "2020-08-31",
                                daysofweekdisabled = c(0,6)),
                      actionButton("runSim", "Run simulation")
               ),
               column(10,
                      textAreaInput("config", "Configuration",
                                    width = "600px",
                                    height = "400px",
                                    value = .readYamlConfig()
                      )
               )),
             # added useshinyjs() call
             useShinyjs(),
             fluidRow(
               column(
                 12,
                 p(strong("Select your simulation directory and press Load Simulation"))
               )
             ),
             fluidRow(
               column(2,
                      shinyDirButton("simDir", "Directory select", "Please select your simulation directory")
               ),
               column(10,
                      textOutput("directory")
               )),
             fluidRow(
               column(2,
                      br(),
                      actionButton("loadSim", "Load simulation")
             ))),
    tabPanel("Results",
      br(),
      tabsetPanel(
        id = "results",
        type = "tabs",
        tabPanel(
          "Overall Stats",
          fluidRow(
            column(
              4,
              align = "left",
              br(),
              tableOutput("overallStatsTable")
            ),
            column(
              8,
              br(),
              plotlyOutput('plot_1')
            )
          ),
          fluidRow(
            column(
              12,
              br(),
              DT::dataTableOutput('perfTable')
            )
          )
          
        ),
        tabPanel(
          "Market Values",
          fluidRow(
            column(
              12,
              br(),
              plotlyOutput('plot_2'),
              DT::dataTableOutput('marketValueTable')
            )
          )
        ),
        tabPanel(
          "Exposures",
          fluidRow(
            column(
              12,
              br(),
              uiOutput('plot_3s'),
              uiOutput('factor_exposure')
            )
          )
        ),
        tabPanel(
          "Holdings",
          fluidRow(
            column(
              12,
              DT::dataTableOutput('positionSummaryTable'),
              br()
            ),
          ),
         # Contains the plot and data table of the selected position
         uiOutput("selectedPlotAndTable")
        ),
        tabPanel(
          "Holdings by Date",
          fluidRow(
            column(
              12,
              dateInput("holdingsDate", label = "Date", value = "2020-06-01",
                        daysofweekdisabled = c(0,6)),
              DT::dataTableOutput('holdingsTable')
            ) 
          )
        )
      )
    )
  )
)