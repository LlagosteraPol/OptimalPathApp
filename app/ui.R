# Define UI for data upload app ----
ui <- fluidPage(
  navbarPage("Optimal Path",
    tabPanel("Data Source",
      fluidRow(
        # Inputs ----
        column(width = 4,
            # Input: Select a file ----
            div(style = 'height: 75px;', 
            fileInput(inputId = "file_nodes",
                      label = "Upload nodes CSV file",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
            )
        ),
        column(width = 4,
          div(style = 'height: 75px;', 
            fileInput(inputId = "file_edges",
                      label = "Upload edges CSV file",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
          )
        ),
        column(width = 4,
          div(style = 'height: 75px;', 
            fileInput(inputId = "file_events",
                      label = "Upload event CSV file",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
          )
        )
      ),#fluidRow
      fluidRow(
        column( width = 2, 
          # Input: Checkbox if file has header ----
          checkboxInput("header", "Header", TRUE),
        ),
        column( width = 2, 
          # Input: Select separator ----
          radioButtons("sep", "Separator",
                       choices = c(Semicolon = ";",
                                   Comma = ",",
                                   Tab = "\t"),
                       selected = ";"),
        ),
        column( width = 2, 
          # Input: Select number of rows to display ----
          radioButtons("disp", "Display",
                       choices = c(Head = "head",
                                   All = "all"),
                       selected = "head")
        ),
      ),
      # Display loaded data tables
      fluidRow(
        column(width = 4, class = "source_tables",
          h4(style = 'text-align: center;', "Node data table"),
          DT::dataTableOutput("node_table")
        ),
        column(width = 4, class = "source_tables",
          h4(style = 'text-align: center;', "Edge data table"),
          DT::dataTableOutput("edge_table")
        ),
        column(width = 4, class = "source_tables",
          h4(style = 'text-align: center;', "Event data table"),
          DT::dataTableOutput("event_table")
        ),
      )#fluidRow: Data table display
    ),#Panel DataSource
    tabPanel("Plot",
      h4("This is another panel"),
      actionButton("load_net", "Load Network"),
      plotOutput('net_plot')
    )# Panel Plot
  )#NavBar 
)#ui fluidpage