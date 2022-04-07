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
      toggle('loader')
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
      
      output$network <- visNetwork::renderVisNetwork({
        visNetwork(nodes, edges) %>%
          visIgraphLayout() %>%
          visEvents(selectNode = "function(nodes) {
                      Shiny.setInputValue('current_node_id', nodes.nodes);
                      ;}",
                    select = "function(edges) {
                      Shiny.setInputValue('current_edge_id', edges.edges);
                      ;}")
      })
      
      observeEvent(input$current_node_id, {
        visNetworkProxy("network") %>%
          visGetNodes()
      })
      
      observeEvent(input$current_edge_id, {
        visNetworkProxy("network") %>%
          visGetEdges()
      })
      
      node_model <- eventReactive(input$current_node_id, {
        if (!is.null(input$current_node_id)){
          node_int = vertex_attr(g, 'intensity', input$current_node_id)
          
          # return all object as a list
          list(node_id = input$current_node_id, node_intensity = node_int)
        }
      })
      
      edge_model <- eventReactive(input$current_edge_id, {
        if (!is.null(input$current_edge_id)){
          edge_int = edge_attr(g, 'intensity', input$current_edge_id)
          edge_imd = edge_attr(g, 'imd', input$current_edge_id)
          
          # return all object as a list
          list(edge_id = input$current_edge_id, edge_intensity = edge_int, edge_imd = edge_imd)
        }
      })
      
      output$node_info <- renderText({
        paste0("<B>Id: </B>", node_model()$node_id, "<br>",
               "<B>Intensity: </B>",  node_model()$node_intensity, "<br>")
      })
      
      output$edge_info <- renderText({
        paste0("<B>Id: </B>", input$current_edge_id, "<br>",
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
    net <- graph_from_data_frame(net_df, vertices = df_nodes[, c('FID', 'x', 'y')])
    
    
    adj_mtx <- as.matrix(as_adjacency_matrix(net))
    
    intnet <- intensitynet::intensitynet(adj_mtx, df_nodes[, c('x3', 'y3')], df_events[, c('x', 'y')])
    
    
    output$net_plot <- renderPlot({
      intensitynet::PlotHeatmap(intnet)
    })
  })#observeEvent: show_plot
}
