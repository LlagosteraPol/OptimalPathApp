library(shinycssloaders)
# Define UI for data upload app ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
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
          checkboxInput("header", "Header", TRUE)
        ),
        column( width = 2, 
          # Input: Select separator ----
          radioButtons("sep", "Separator",
                       choices = c(Semicolon = ";",
                                   Comma = ",",
                                   Tab = "\t"),
                       selected = ";")
        ),
        column( width = 2, 
          # Input: Select number of rows to display ----
          radioButtons("disp", "Display",
                       choices = c(Head = "head",
                                   All = "all"),
                       selected = "head")
        )
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
        )
      )#fluidRow: Data table display
    ),#Panel DataSource
    tabPanel("Interactive Plot",
      fluidRow(
        h4("Interactive plot")
      ),
      div(style = 'margin: 0px 0px 10px 0px;',
        fluidRow(
          actionButton("load_net", "Load Network")
        )
      ),
      fluidRow(
        column(3,
               selectInput(inputId = "node_display", 
                           label = "Node display:",
                           choices = c("id" = 1, 
                                       "intensity" = 2, 
                                       "none" = 3),
                           selected = 1),
               selectInput(inputId = "edge_display", 
                           label = "Edge display", 
                           choices = c("id" = 1, 
                                       "intensity" = 2, 
                                       "imd" = 3,
                                       "none" = 4),
                           selected = 3),
               h3("Node Info:"),
               htmlOutput("node_info"),
               h3("Edge Info:"),
               htmlOutput("edge_info")
               #dataTableOutput("nodes_data_from_shiny"),
               #verbatimTextOutput("node_id"),
               #verbatimTextOutput("node_int"),
        ),
        column(style='border: 1px solid black', 9,
               withSpinner(
                 id = 'loader',
                 type = 1,
                 visNetwork::visNetworkOutput("network",  height = "100vh")
               )
        )
      )
    ),# Panel Plot
    tabPanel("Heatmap Plots",
      h4("Heatmap plot"),
      actionButton("show_plot", "Show selected plot"),
      plotOutput('net_plot')
    )# Panel Plot
  )#NavBar 
)#ui fluidpage