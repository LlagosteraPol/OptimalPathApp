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
