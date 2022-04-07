
#--------------------------------------------------------------------06/04/2022--------------------------------------------------------------


library(shiny)
library(shinyFiles) # Provides functionality for client-side navigation of the server side file system in shiny apps. 
library(shinyjs) # Perform common useful JavaScript operations in Shiny 
library(shiny.router) # It is a simple router for your Shiny apps
library(igraph)
library(intensitynet)
#source("./main.R", local=TRUE)


# Define server logic to read selected file ----
server <- function(input, output) {
  output$test1 <- renderTable( matrix(1:20, nrow=4))
  
  output$node_table <- DT::renderDataTable({
    req(input$file_nodes)
    tryCatch(
      {
        df_nodes <- read.csv(input$file_nodes$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      info <- head(df_nodes)
    }
    else {
      info <- df_nodes
    }
    
    DT::datatable(info, escape=FALSE, 
                  options = list(
                    pageLength = 10, 
                    autoWidth = TRUE,
                    scrollX = TRUE
                  ))
  })
  
  output$edge_table <- renderDataTable({
    req(input$file_edges)
    tryCatch(
      {
        df_edges <- read.csv(input$file_edges$datapath,
                             header = input$header,
                             sep = input$sep,
                             quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df_edges))
    }
    else {
      return(df_edges)
    }
  })
  
  output$event_table <- renderDataTable({
    req(input$file_events)
    tryCatch(
      {
        df_events <- read.csv(input$file_events$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df_events))
    }
    else {
      return(df_events)
    }
  })
  
  
  
  # output$contents <- renderTable({
  # 
  #   # input$file1 will be NULL initially. After the user selects
  #   # and uploads a file, head of that data file by default,
  #   # or all rows if selected, will be shown.
  # 
  #   req(input$file_nodes)
  #   req(input$file_edges)
  # 
  #   # when reading semicolon separated files,
  #   # having a comma separator causes `read.csv` to error
  #   tryCatch(
  #     {
  #       df_nodes <- read.csv(input$file_nodes$datapath,
  #                      header = input$header,
  #                      sep = input$sep,
  #                      quote = input$quote)
  #     },
  #     error = function(e) {
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )
  # 
  #   tryCatch(
  #     {
  #       df_edges <- read.csv(input$file_edges$datapath,
  #                            header = input$header,
  #                            sep = input$sep,
  #                            quote = input$quote)
  #     },
  #     error = function(e) {
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )
  # 
  #   if(input$disp == "head") {
  # 
  #     output$node_table <- renderDataTable({head(df_nodes)})
  #     #return(head(df_nodes))
  #   }
  #   else {
  #     return(df_nodes)
  #   }
  # 
  # 
  # })
  
}

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

#--------------------------------------------------------------------07/04/2022--------------------------------------------------------------
library(shiny)
library(shinyFiles) # Provides functionality for client-side navigation of the server side file system in shiny apps. 
library(shinyjs) # Perform common useful JavaScript operations in Shiny 
library(shiny.router) # It is a simple router for your Shiny apps
library(igraph)
library(intensitynet)
#source("./main.R", local=TRUE)


# Define server logic to read selected file ----
server <- function(input, output) {
  output$test1 <- renderTable( matrix(1:20, nrow=4))
  
  output$node_table <- DT::renderDataTable({
    req(input$file_nodes)
    tryCatch(
      {
        df_nodes <- read.csv(input$file_nodes$datapath,
                             header = input$header,
                             sep = input$sep)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      info <- head(df_nodes)
    }
    else {
      info <- df_nodes
    }
    
    DT::datatable(info, escape=FALSE, 
                  options = list(
                    pageLength = 10, 
                    autoWidth = TRUE,
                    scrollX = TRUE
                  ))
  })
  
  output$edge_table <- DT::renderDataTable({
    req(input$file_edges)
    tryCatch(
      {
        df_edges <- read.csv(input$file_edges$datapath,
                             header = input$header,
                             sep = input$sep)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      info <- head(df_edges)
    }
    else {
      info <- df_edges
    }
    
    DT::datatable(info, escape=FALSE, 
                  options = list(
                    pageLength = 10, 
                    autoWidth = TRUE,
                    scrollX = TRUE
                  ))
  })
  
  output$event_table <- DT::renderDataTable({
    req(input$file_events)
    tryCatch(
      {
        df_events <- read.csv(input$file_events$datapath,
                              header = input$header,
                              sep = input$sep)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      info <- head(df_events)
    }
    else {
      info <- df_events
    }
    
    DT::datatable(info, escape=FALSE, 
                  options = list(
                    pageLength = 10, 
                    autoWidth = TRUE,
                    scrollX = TRUE
                  ))
  })
  
  observeEvent(input$load_net, {
    
    if(is.null(input$file_nodes$datapath) || is.null(input$file_edges$datapath) || is.null(input$file_events$datapath)){
      showModal(modalDialog(
        title = "Warning",
        "Please, select the files in 'Data Source' tab to create the network.",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      df_nodes <- read.csv(input$file_nodes$datapath,
                           header = input$header,
                           sep = input$sep)
      
      df_edges <- read.csv(input$file_edges$datapath,
                           header = input$header,
                           sep = input$sep)
      
      df_events <- read.csv(input$file_events$datapath,
                            header = input$header,
                            sep = input$sep)
      
      edges <- cbind(df_edges$FID_1, df_edges$FID_12)
      # net <- igraph::graph_from_edgelist(edges)
      # adj_mtx <- as.matrix(igraph::as_adjacency_matrix(net))
      # node_coords <- data.frame(xcoord = df_nodes$x3, 
      #                           ycoord = df_nodes$y3)
      
      # event_coords <- data.frame(xcoord = df_events$x, 
      #                            ycoord = df_events$y)
      # 
      # 
      # intnet <- intensitynet::intensitynet(adjacency_mtx = adj_mtx, 
      #                                      node_coords = node_coords,
      #                                      event_data = event_coords)
      
      #intnet <- RelateEventsToNetwork(intnet)
      #intensitynet::PlotHeatmap(intnet)
      
      
      net_df <- data.frame( from = df_edges[, 'FID_1'],
                            to = df_edges[, 'FID_12'],
                            imd = df_edges[, 'IMD2015'])
      net <- graph_from_data_frame(net_df, vertices = df_nodes[, c('FID', 'x', 'y')])
      
      
      adj_mtx <- as.matrix(as_adjacency_matrix(net))
      
      intnet <- intensitynet::intensitynet(adj_mtx, df_nodes[, c('x3', 'y3')], df_events[, c('x', 'y')])
      
      output$net_plot <- renderPlot({ 
        intensitynet::PlotHeatmap(intnet)
      })
    }
  })#observeEvent: load_net
}


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
