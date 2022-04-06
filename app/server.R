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
