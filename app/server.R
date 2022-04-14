library(shiny)
library(shinyFiles) # Provides functionality for client-side navigation of the server side file system in shiny apps. 
library(shinyjs) # Perform common useful JavaScript operations in Shiny 
library(shiny.router) # It is a simple router for your Shiny apps
library(visNetwork)
library(igraph)
library(intensitynet)

source("../R/functions.R", local=TRUE)


server <- function(input, output, session) {
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
  
  g <- eventReactive(input$load_net, {
    if(is.null(input$file_nodes$datapath) || is.null(input$file_edges$datapath) || is.null(input$file_events$datapath)){
      showModal(modalDialog(
        title = "Warning",
        "Please, select the files in 'Data Source' tab to create the network.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    else {
      if(as.integer(input$cov1_display) == 1 || as.integer(input$cov2_display) == 1){
        showModal(modalDialog(
          title = "Warning",
          "Please, select two covariants to calculate its linear combination W(l_i).",
          easyClose = TRUE,
          footer = NULL
        ))
      }else if(input$cov1_display == input$cov2_display ){
        showModal(modalDialog(
          title = "Warning",
          "The two covariants cannot be the same",
          easyClose = TRUE,
          footer = NULL
        ))
        
      }else{
        PrepareIgraph(net_data = df_edges(), 
                         node_data = df_nodes(), 
                         cov1 = colnames(df_edges())[as.integer(input$cov1_display)],
                         cov2 = colnames(df_edges())[as.integer(input$cov2_display)],
                         prop = input$weight_prop / 100,
                         invert_cov1 = input$invert_cov1, 
                         invert_cov2 = input$invert_cov2)
      }
    }
  })
  
  #-----------------------------------------------------------Network-------------------------------------------------------------
  #visNetwork(nodes, edges) %>%
  #visIgraph(g) %>%
  output$network <- visNetwork::renderVisNetwork({
    if(is.null(g())) return()
    
    visIgraph(g())  %>%
      visIgraphLayout(layout = "layout.norm", layoutMatrix = cbind(df_nodes()[, 2], df_nodes()[, 3] * -1 ), type = "full") %>%
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

  
  observeEvent(input$current_node_id, {
    visNetworkProxy("network") %>%
      visGetSelectedNodes()
  })

  
  observeEvent(input$current_edge_id, {
    visNetworkProxy("network") %>%
      visGetSelectedEdges()
  })


  output$node_info <- renderText({
    if(is.null(g())) return()
    
    info_node <- paste0("<B>Id: </B>", input$current_node_id, "<br>")
    for(element in igraph::vertex_attr_names(g())[2:length(igraph::vertex_attr_names(g()))]){
      info_node <- paste0(info_node, "<B>", element,"</B>: ", igraph::vertex_attr(g(), element, index = input$current_node_id), "<br>")
    }
    info_node <- paste0(info_node, "<br>")

    info_node
  })

  
  output$edge_info <- renderText({
    if(is.null(g())) return()

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

  
  output$node_select_info <- renderUI({
    if(is.null(g())) return()
    
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
    if(is.null(g())) return()
    
    choices_list <- c("none" = 1, "id" = 2)
    
    i = 3
    for(element in igraph::edge_attr_names(g())[1:length(igraph::edge_attr_names(g())) - 1]){
      choices_list <- c(choices_list, setNames(i, element) )
      i <- i+1
    }
    selectInput(inputId = "edge_display",
                label = NULL, 
                choices = choices_list,
                selected = 1
    )
  })
  
  
  output$cov1 <- renderUI({
    if(is.null(df_edges())) return()
    
    choices_list <- c("none" = 1)
    
    i = 3
    for(element in colnames(df_edges())[3:length(colnames(df_edges()))]){
      choices_list <- c(choices_list, setNames(i, element) )
      i <- i+1
    }
    selectInput(inputId = "cov1_display",
                label = NULL, 
                choices = choices_list,
                selected = 1
    )
  })
  
  
  output$cov2 <- renderUI({
    if(is.null(df_edges())) return()
    
    choices_list <- c("none" = 1)
    
    i = 3
    for(element in colnames(df_edges())[3:length(colnames(df_edges()))]){
      choices_list <- c(choices_list, setNames(i, element) )
      i <- i+1
    }
    selectInput(inputId = "cov2_display",
                label = NULL, 
                choices = choices_list,
                selected = 1
    )
  })
  
  
  output$heatmap_select <- renderUI({
    if(is.null(g())) return()
    
    choices_list <- c("none" = 1)
    if(length(igraph::edge_attr_names(g()) > 2)){
      i = 2
      for(element in igraph::edge_attr_names(g())[1:length(igraph::edge_attr_names(g())) - 1]){
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
    if(is.null(g())) return()
    
    events <- NULL
    if(input$show_events) events <- as.matrix(df_events())
    
    if(input$heat_display == 1) mode = 'none'
    
    else mode = igraph::edge_attr_names(g())[as.integer(input$heat_display) - 1]
      
    PlotNetwork(g(), mode = mode, events = events, alpha = input$event_alpha / 100)
    
  }, height = function() {
    session$clientData$output_net_plot_width
  })
  
  
  
}
