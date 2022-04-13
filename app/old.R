
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

#--------------------------------------------------------------------12/04/2022--------------------------------------------------------------
library(shiny)
library(shinyFiles) # Provides functionality for client-side navigation of the server side file system in shiny apps. 
library(shinyjs) # Perform common useful JavaScript operations in Shiny 
library(shiny.router) # It is a simple router for your Shiny apps
library(visNetwork)
library(igraph)
library(intensitynet)
#source("./main.R", local=TRUE)


# Define server logic to read selected file ----
server <- function(input, output) {
  intnet <- NULL
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
      
      net_df <- data.frame( from = df_edges[, 'FID_1'],
                            to = df_edges[, 'FID_12'],
                            imd = df_edges[, 'IMD2015'])
      net <- graph_from_data_frame(net_df, vertices = df_nodes[, c('FID', 'x', 'y')])
      
      
      adj_mtx <- as.matrix(as_adjacency_matrix(net))
      
      intnet <- intensitynet::intensitynet(adj_mtx, df_nodes[, c('x3', 'y3')], df_events[, c('x', 'y')])
      
      g <- intnet$graph
      g <- igraph::set_edge_attr(g, name ='imd', value = df_edges[, 'IMD2015'][1:914])
      
      nodes <- data.frame(id = df_nodes[, 'FID'],
                          x = df_nodes[, 'x3'],
                          y = df_nodes[, 'y3'])
      edges <- data.frame(id = df_edges[, 'FID'],
                          from = df_edges[, 'FID_1'],
                          to = df_edges[, 'FID_12'])
      
      #visNetwork(nodes, edges) %>%
      #visIgraph(g) %>%
      output$network <- visNetwork::renderVisNetwork({
        visNetwork(nodes, edges) %>%
          visIgraphLayout(layout = "layout.norm", layoutMatrix = as.matrix(df_nodes[, c('y3', 'x3')]), type = "full") %>%
          visNodes(size = 15) %>%
          visEvents(selectNode = "function(nodes) {
                       Shiny.onInputChange('current_node_id', nodes.nodes);
                      ;}",
                    deselectNode = "function(nodes) {
                       Shiny.onInputChange('current_node_id', nodes.nodes);
                      ;}",
                    selectEdge = "function(edges = null) {
                      Shiny.onInputChange('current_edge_id', edges.edges);
                      ;}",
                    deselectEdge = "function(edges = null) {
                      Shiny.onInputChange('current_edge_id', edges.edges);
                      ;}") %>%
          visInteraction(zoomView = TRUE, 
                         multiselect = TRUE, 
                         hover = TRUE) #, dragNodes = FALSE) 
      })
      
      observeEvent(
        input$current_node_id, {
          visNetworkProxy("network") %>%
            visGetSelectedNodes()
        })
      
      observeEvent(
        input$current_edge_id, {
          visNetworkProxy("network") %>%
            visGetSelectedEdges()
        })
      
      
      output$node_info <- renderText({
        paste0("<B>Id: </B>", input$current_node_id, "<br>",
               "<B>x: </B>",  igraph::vertex_attr(g, 'xcoord', input$current_node_id), "<br>",
               "<B>y: </B>",  igraph::vertex_attr(g, 'ycoord', input$current_node_id), "<br>",
               "<B>Intensity: </B>",  igraph::vertex_attr(g, 'intensity', input$current_node_id), "<br><br>")
      })
      
      output$edge_info <- renderText({
        paste0("<B>Id: </B>", input$current_edge_id, "<br>",
               "<B>Nº events: </B>", edge_attr(g, 'n_events', input$current_edge_id), "<br>",
               "<B>Intensity: </B>",  edge_attr(g, 'intensity', input$current_edge_id), "<br>",
               "<B>IMD: </B>",  edge_attr(g, 'imd', input$current_edge_id), "<br><br>")
      })
      
      observe({
        if (!is.null(input$node_display)){
          if(input$node_display == 1){
            nodes <- data.frame(id = paste(igraph::vertex_attr(g)$name),
                                label = paste(igraph::vertex_attr(g)$name))
          }else if(input$node_display == 2){
            nodes <- data.frame(id = paste(igraph::vertex_attr(g)$name),
                                label = paste(round(igraph::vertex_attr(g)$intensity, 4)))
          }else{
            nodes <- data.frame(id = paste(igraph::vertex_attr(g)$name),
                                label = "")
          }
          visNetworkProxy("network") %>% visUpdateNodes(nodes)
        }
      })
      
      observe({
        if (!is.null(input$edge_display)){
          edge_ids <- get.edge.ids(g, as.vector(t(get.edgelist(g))))
          
          if(input$edge_display == 1){
            edges <- data.frame(id = edge_ids,
                                from = get.edgelist(g)[,1],
                                to = get.edgelist(g)[,2],
                                label = paste(edge_ids))
          }else if(input$edge_display == 2){
            edges <- data.frame(id = edge_ids,
                                from = get.edgelist(g)[,1],
                                to = get.edgelist(g)[,2],
                                label = paste(round(edge_attr(g)$intensity, 4)))
          }else if(input$edge_display == 3){
            edges <- data.frame(id = edge_ids,
                                from = get.edgelist(g)[,1],
                                to = get.edgelist(g)[,2],
                                label = paste(round(edge_attr(g)$imd, 4)))
          }else{
            edges <- data.frame(id = edge_ids,
                                from = get.edgelist(g)[,1],
                                to = get.edgelist(g)[,2],
                                label = " ")
          }
          visNetworkProxy("network") %>% visUpdateEdges(edges)
        }
      })
      
      observe({
        print(input$network_selectedNodes)
      })
      
    }
  })#observeEvent: load_net
  
  observeEvent(input$show_plot, {
    # if(is.null(intnet)){
    #   showModal(modalDialog(
    #     title = "Warning",
    #     "Please, load the network first.",
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # }else{
    #   output$net_plot <- renderPlot({
    #     intensitynet::PlotHeatmap(intnet)
    #   })
    # }
    
    df_nodes <- read.csv(input$file_nodes$datapath,
                         header = input$header,
                         sep = input$sep)
    
    df_edges <- read.csv(input$file_edges$datapath,
                         header = input$header,
                         sep = input$sep)
    
    df_events <- read.csv(input$file_events$datapath,
                          header = input$header,
                          sep = input$sep)
    
    
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
    net <- graph_from_data_frame(net_df, vertices = df_nodes[, c('FID', 'x3', 'y3')])
    
    
    adj_mtx <- as.matrix(as_adjacency_matrix(net))
    
    intnet <- intensitynet::intensitynet(adj_mtx, df_nodes[, c('x3', 'y3')], df_events[, c('x', 'y')])
    
    
    output$net_plot <- renderPlot({
      intensitynet::PlotHeatmap(intnet)
    })
  })#observeEvent: show_plot
}


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

#--------------------------------------------------------------------13/04/2022--------------------------------------------------------------
library(shiny)
library(shinyFiles) # Provides functionality for client-side navigation of the server side file system in shiny apps. 
library(shinyjs) # Perform common useful JavaScript operations in Shiny 
library(shiny.router) # It is a simple router for your Shiny apps
library(visNetwork)
library(igraph)
library(intensitynet)

source("../R/functions.R", local=TRUE)


server <- function(input, output) {
  intnet <- NULL
  output$test1 <- renderTable( matrix(1:20, nrow=4))
  
  
  #-------------------------------------------------------------Read files-------------------------------------------------------------
  df_nodes <- reactive({
    validate(need(input$file_nodes, "Please upload a file"))
    
    tryCatch(
      {
        read.csv2(input$file_nodes$datapath,
                  header = input$header,
                  sep = input$sep)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  
  df_edges <- reactive({
    validate(need(input$file_edges, "Please upload a file"))
    
    tryCatch(
      {
        read.csv2(input$file_edges$datapath,
                  header = input$header,
                  sep = input$sep)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  
  df_events <- reactive({
    validate(need(input$file_events, "Please upload a file"))
    
    tryCatch(
      {
        read.csv2(input$file_events$datapath,
                  header = input$header,
                  sep = input$sep)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  
  #-------------------------------------------------------------Show files-------------------------------------------------------------
  output$node_table <- DT::renderDataTable({
    
    if(input$disp == "head") {
      info <- head(df_nodes())
    }
    else {
      info <- df_nodes()
    }
    
    DT::datatable(info, escape=FALSE, 
                  options = list(
                    pageLength = 10, 
                    autoWidth = TRUE,
                    scrollX = TRUE
                  ))
  })
  
  
  output$edge_table <- DT::renderDataTable({
    
    if(input$disp == "head") {
      info <- head(df_edges())
    }
    else {
      info <- df_edges()
    }
    
    DT::datatable(info, escape=FALSE, 
                  options = list(
                    pageLength = 10, 
                    autoWidth = TRUE,
                    scrollX = TRUE
                  ))
  })
  
  
  output$event_table <- DT::renderDataTable({
    
    if(input$disp == "head") {
      info <- head(df_events())
    }
    else {
      info <- df_events()
    }
    
    DT::datatable(info, escape=FALSE, 
                  options = list(
                    pageLength = 10, 
                    autoWidth = TRUE,
                    scrollX = TRUE
                  ))
  })
  
  g <- reactive({
    observeEvent(input$load_net, {
      if(is.null(df_nodes()) && is.null(df_edges())) return()
      PrepareIgraph(net_data = df_edges(), node_data = df_nodes(), invert_cov1 = input$invert_cov1, invert_cov2 = input$invert_cov2)
    })
  })
  
  #-----------------------------------------------------------Network-------------------------------------------------------------
  observeEvent(input$load_net, {
    
    if(is.null(input$file_nodes$datapath) || is.null(input$file_edges$datapath) || is.null(input$file_events$datapath)){
      showModal(modalDialog(
        title = "Warning",
        "Please, select the files in 'Data Source' tab to create the network.",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      
      
      #visNetwork(nodes, edges) %>%
      #visIgraph(g) %>%
      output$network <- visNetwork::renderVisNetwork({
        visIgraph(g())  %>%
          visIgraphLayout(layout = "layout.norm", layoutMatrix = as.matrix(df_nodes()[, 2:3]), type = "full") %>%
          visNodes(size = 15) %>%
          visEvents(selectNode = "function(nodes) {
                       Shiny.onInputChange('current_node_id', nodes.nodes);
                      ;}",
                    deselectNode = "function(nodes) {
                       Shiny.onInputChange('current_node_id', nodes.nodes);
                      ;}",
                    selectEdge = "function(edges) {
                      Shiny.onInputChange('current_edge_id', edges.edges);
                      ;}",
                    deselectEdge = "function(edges) {
                      Shiny.onInputChange('current_edge_id', edges.edges);
                      ;}") %>%
          visInteraction(zoomView = TRUE, 
                         multiselect = TRUE, 
                         hover = TRUE) #, dragNodes = FALSE) 
      })
      
      observeEvent(
        input$current_node_id, {
          visNetworkProxy("network") %>%
            visGetSelectedNodes()
        })
      
      observeEvent(
        input$current_edge_id, {
          visNetworkProxy("network") %>%
            visGetSelectedEdges()
        })
      
      
      output$node_info <- renderText({
        info_node <- paste0("<B>Id: </B>", input$current_node_id, "<br>")
        for(element in igraph::vertex_attr_names(g())[2:length(igraph::vertex_attr_names(g()))]){
          info_node <- paste0(info_node, "<B>", element,"</B>: ", igraph::vertex_attr(g(), element, index = input$current_node_id), "<br>")
        }
        info_node <- paste0(info_node, "<br>")
        
        info_node
      })
      
      output$edge_info <- renderText({
        print(input$current_edge_id)
        
        info_edge <- paste0("<B>Id: </B>", input$current_edge_id, "<br>")
        for(element in igraph::edge_attr_names(g())[2:length(igraph::edge_attr_names(g()))-1]){
          info_edge <- paste0(info_edge, "<B>", element,"</B>: ", igraph::edge_attr(g(), element, index = input$current_edge_id), "<br>")
        }
        info_edge <- paste0(info_edge, "<br>")
        
        info_edge
      })
      
      observe({
        if (!is.null(input$node_display)){
          if(input$node_display == 1){
            nodes <- data.frame(id = paste(igraph::vertex_attr(g())$name),
                                label = "")
          }else if(input$node_display == 2){
            nodes <- data.frame(id = paste(igraph::vertex_attr(g())$name),
                                label = paste(igraph::vertex_attr(g())$name))
          }else if(input$node_display == 3){
            nodes <- data.frame(id = paste(igraph::vertex_attr(g())$name),
                                label = paste0("x: ", round(igraph::vertex_attr(g())[[2]], 4), " y: ", round(igraph::vertex_attr(g())[[3]], 4)))
          }else{
            index <- as.numeric(input$node_display) - 2
            
            if(is.numeric(igraph::vertex_attr(g())[[index]])){
              node_info <- paste(round(igraph::vertex_attr(g())[[index]], 4))
            }else{
              index <- as.numeric(input$edge_display) - 2
              node_info <- igraph::edge_attr(g())[[index]]
              
              if(is.numeric(node_info)){
                node_info <- paste(round(node_info, 4))
              }else{
                node_info <- paste(node_info)
              }
              
              nodes <- data.frame(id = paste(igraph::vertex_attr(g())$name),
                                  label = node_info
              )
            }
          }
          visNetworkProxy("network") %>% visUpdateNodes(nodes)
        }
      })
      
      observe({
        if (!is.null(input$edge_display)){
          edge_ids <- igraph::get.edge.ids(g(), as.vector(t(get.edgelist(g()))))
          
          if(input$edge_display == 1){
            edges <- data.frame(id = edge_ids,
                                from = igraph::get.edgelist(g())[,1],
                                to = igraph::get.edgelist(g())[,2],
                                label = " ")
          }else if(input$edge_display == 2){
            edges <- data.frame(id = edge_ids,
                                from = igraph::get.edgelist(g())[,1],
                                to = igraph::get.edgelist(g())[,2],
                                label = paste(edge_ids))
          }else{
            index <- as.numeric(input$edge_display) - 2
            edge_info <- igraph::edge_attr(g())[[index]]
            
            if(is.numeric(edge_info)){
              edge_info <- paste(round(edge_info, 4))
            }else{
              edge_info <- paste(edge_info)
            }
            
            edges <- data.frame(id = edge_ids,
                                from = igraph::get.edgelist(g())[,1],
                                to = igraph::get.edgelist(g())[,2],
                                label = edge_info
            )
          }
          visNetworkProxy("network") %>% visUpdateEdges(edges)
        }
      })
      
      observe({
        print(input$network_selectedNodes)
      })
    }
  })#observeEvent: load_net
  
  output$node_select_info <- renderUI({
    if(is.null(df_nodes())) return()
    
    choices_list <- c("none" = 1, "id" = 2, "coords" = 3)
    
    if(length(colnames(df_nodes())) > 3){
      i = 4
      for(element in colnames(df_nodes())[4:length(colnames(df_nodes()))]){
        choices_list <- c(choices_list, setNames(i, element ) )
        i <- i+1
      }
    }
    selectInput(inputId = "node_display",
                label = NULL, 
                choices = choices_list,
                selected = 1
    )
  })
  
  output$edge_select_info <- renderUI({
    if(is.null(df_edges())) return()
    
    choices_list <- c("none" = 1, "id" = 2)
    
    if(length(colnames(df_edges())) > 2){
      i = 3
      for(element in colnames(df_edges())[3:length(colnames(df_edges()))]){
        choices_list <- c(choices_list, setNames(i, element ) )
        i <- i+1
      }
    }
    selectInput(inputId = "edge_display",
                label = NULL, 
                choices = choices_list,
                selected = 1
    )
  })
  
  output$heatmap_select <- renderUI({
    if(is.null(df_edges())) return()
    
    choices_list <- c("none" = 1)
    if(length(colnames(df_edges())) > 2){
      i = 3
      for(element in colnames(df_edges())[3:length(colnames(df_edges()))]){
        choices_list <- c(choices_list, setNames(i, element ) )
        i <- i+1
      }
    }
    selectInput(inputId = "heat_display",
                label = NULL, 
                choices = choices_list,
                selected = 1
    )
  })
  
  output$net_plot <- renderPlot({
    t <- input$heat_display
    PlotNetwork(g(), mode = input$heat_display)
  })
}

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
                        column(3,
                               h4("Interactive plot")
                        ),
                        column(3,
                               checkboxInput("invert_cov1", "Invert covariant 1", FALSE)
                        ),
                        column(3,
                               checkboxInput("invert_cov2", "Invert covariant 2", FALSE)
                        ),
                        column(3,
                               div(style = 'margin: 0px 0px 20px 0px;',
                                   actionButton("load_net", "Load Network")
                               )
                        )
                      ),
                      fluidRow(
                        column(3,
                               # selectInput(inputId = "node_display", 
                               #             label = "Node display:",
                               #             choices = c("none" = 1,
                               #                         "id" = 2, 
                               #                         "coords" = 3),
                               strong("Node display"),
                               uiOutput("node_select_info"),
                               strong("Edge display"),
                               uiOutput("edge_select_info"),
                               # selectInput(inputId = "edge_display", 
                               #             label = NULL, 
                               #             choices = c("none" = 1,
                               #                         "id" = 2, 
                               #                         "distance" = 3,
                               #                         "cov1" = 3, 
                               #                         "cov2" = 4,
                               #                         "W(cov1)" = 5,
                               #                         "W(cov2)" = 6),
                               #             selected = 1),
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
                      fluidRow(
                        # Inputs ----
                        column(width = 3,
                               h4("Heatmap plot")
                        ),
                        column(width = 3,
                               strong("Heatmap Type"),
                               uiOutput("heatmap_select")
                        )
                      ),
                      plotOutput('net_plot')
             )# Panel Plot
  )#NavBar 
)#ui fluidpage