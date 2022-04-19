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
        tabBox(width = 12, height =  "15vh",
          tabPanel("Network data",
            div(style = "margin: 7px; 0px; 0px;",
              fluidRow(
                column(3,
                       strong("Covariant 1"),
                       uiOutput('cov1'),
                       checkboxInput("invert_cov1", "Invert", FALSE)
                ),
                column(3,
                       strong("Covariant 2"),
                       uiOutput('cov2'),
                       checkboxInput("invert_cov2", "Invert", FALSE)
                ),
                column(3,
                       sliderInput(inputId = "weight_prop", 
                                   label = "Weight proportion",
                                   min = 1, 
                                   max = 100, 
                                   value = 50)
                ),
                column(3, align = "center",
                       div( style = "margin: 18px; 0px; 0px;",
                           actionButton("load_net", "Load Network")
                       )
                )
              )
            )
          ),
          tabPanel("Optimal Path",
            div(style = "margin: 7px; 0px; 0px;",
              fluidRow(
                column(3,
                       strong("Origin"),
                       textInput(inputId = 'start_path', label = NULL)
                       
                ),
                column(3,
                       strong("Destination"),
                       textInput(inputId = 'end_path', label = NULL)
                ),
                column(3,
                       strong("Weight"),
                       uiOutput('weight_type')
                ),
                column(3,  align = "center",
                       div( style = "margin: 19px; 0px; 0px;",
                           actionButton("get_path", "calculate")
                       ),
                       
                ),
              )
            )
          )
        )
      ),
      hr(),
      fluidRow(
        column(3, 
              
        strong("Node display"),
        uiOutput("node_select_info"),
        
        strong("Edge display"),
        uiOutput("edge_select_info"),
       
        h3("Node Info:"),
        fluidRow(style = "overflow-y:scroll; max-height: 170px",
          htmlOutput("node_info"),
        ),
        h3("Edge Info:"),
        fluidRow(style = "overflow-y:scroll; max-height: 260px",
          htmlOutput("edge_info")
        )
        ),
        column(9,
           fluidRow(style='border: 1px solid black',
             withSpinner(
               id = 'loader',
               type = 1,
               visNetwork::visNetworkOutput("network",  height = "75vh"),
               
             ),
             tags$script("
                Shiny.addCustomMessageHandler('current_node_id', function(value) {
                Shiny.setInputValue('current_node_id', value);
                });
                Shiny.addCustomMessageHandler('current_edge_id', function(value) {
                Shiny.setInputValue('current_edge_id', value);
                });
              ")
           )
        )
      ),
    ),# Panel Plot
    tabPanel("Heatmap Plots",
    fluidRow(
      column(width = 3,
        strong("Heatmap Type"),
        uiOutput("heatmap_select")
      ),
      column(width = 3,  align = "center",
        div( style = "margin: 25px; 0px; 0px;",
          checkboxInput("show_events", "Show events", FALSE)
        )
      ),
      column(width = 3,
        sliderInput(inputId = "event_alpha", 
                    label = "Event transparency",
                    min = 1, 
                    max = 100, 
                    value = 100)
      )
    ),
      plotOutput('net_plot')
    )# Panel Plot
  )#NavBar 
)#ui fluidpage