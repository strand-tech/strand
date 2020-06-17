library(DT)

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
             column(2,
                    dateInput("startDate", label = "Start date", value = "2019-01-02",
                              min = "2019-01-02", max = "2019-03-29",
                              daysofweekdisabled = c(0,6)),
                    dateInput("endDate", label = "End date", value = "2019-03-29",
                              min = "2019-01-02", max = "2019-03-29",
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
    tabPanel(
      "Results",
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
              plotOutput('plot_1')
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
              plotOutput('plot_2'),
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
              plotOutput('plot_3'),
              plotOutput('plot_4')
            )
          )
        ),
        tabPanel(
          "Holdings",
          fluidRow(
            column(
              12,
              DT::dataTableOutput('positionSummaryTable'),
              br(),
              dateInput("holdingsDate", label = "Date", value = "2019-01-02",
                        daysofweekdisabled = c(0,6)),
              DT::dataTableOutput('holdingsTable'),
              br(),
              #column(6,
               
            )
          )
        ),
        tabPanel(
          "Individual Positions",
          fluidRow(
            column(
              4,
              DT::dataTableOutput('selectedrow')
            ),
            column(
              8,
              plotOutput('holdingsPlot')
            )
          ),
          fluidRow(
            column(
              12,
              #textInput("secID", label = "Enter a holding to track", value = "N/A"),
              # br(),        
              DT::dataTableOutput('holdings')
             )
          )
        )
      )
    )
  )
)