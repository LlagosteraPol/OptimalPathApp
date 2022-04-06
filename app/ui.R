# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput(inputId = "file_nodes",
                label = "Upload nodes CSV file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput(inputId = "file_edges",
                label = "Upload edges CSV file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      fileInput(inputId = "file_events",
                label = "Upload event CSV file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),

      # Horizontal line ----
      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        column(width = 4, class = "well",
               
          h4("Brush and double-click to zoom"),
               
          fluidRow(
            
            DT::dataTableOutput("node_table")
          )
        ),
        column(width = 4, class = "well2",
               
               h4("Brush and double-click to zoom"),
               
               fluidRow(
                 dataTableOutput("edge_table")
               )
        )
      )
      # Output: Data file ----
      # fluidRow(
      #   column(width = 4, class = "well",
      #          h4("Brush and double-click to zoom"),
      #          fluidRow(
      #            column(width = 4,
      #                   dataTableOutput("node_table")
      #                   ),
      #            column(width = 4,
      #                   tableOutput("node_table")
      #                   )
      #          ),
      # 
      #   ),
      #   column(width = 8, class = "well",
      #          h4("Left plot controls right plot"),
      #          fluidRow(
      #            column(width = 6,
      #                   plotOutput("plot2", height = 300,
      #                              brush = brushOpts(
      #                                id = "plot2_brush",
      #                                resetOnNew = TRUE
      #                              )
      #                   )
      #            ),
      #            column(width = 6,
      #                   plotOutput("plot3", height = 300)
      #            )
      #          )
      #   )
      # 
      # )
      
    )#mainPanel
    
  )#sidebarLayout
)#ui fluidpage