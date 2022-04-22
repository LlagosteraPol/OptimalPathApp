library(dils)
require(igraph)
library(roxygen2)
library(spatstat)
library(yenpathy)
library(parallel)


#' Creates a network model in igraph class where the edge and node attributes contain information.
#' 
#' @name PrepareIgraph
#' @param net_data dataframe with columns from, to, distance, covariate1 and covariate2 in this order but the names can be different.
#' @param node_data two columns dataframe with the Id of the nodes and its x and y coordinates.
#' @param cov1 select first covariant to calculate the linear combination W(l_i)
#' @param cov2 select second covariant to calculate the linear combination W(l_i)
#' @param prop select the proportion for the covariance linear combination, (cov1 = prop, cov2 = 1 - prop). Default 0.5.
#' @param invert_cov1 invert covariate 1 (max(cov1) - cov1_i), default FALSE
#' @param invert_cov2 invert covariate 2 (max(cov2) - cov2_i), default FALSE
#' @return igraph network class object
#' 
PrepareIgraph <- function(net_data, node_data, cov1, cov2, prop = 0.5, invert_cov1 = FALSE, invert_cov2 = FALSE){
  
  if(prop > 1 || prop < 0) stop('Bad proportion Error: Proportion needs to be in the range [0, 1].')
  
  if(length(node_data) < 3){
    id <-  rep(1:nrow(net_data))
    net_data <- cbind( id, net_data)
  }
  
  
  transformed_weights <- weighted_data(cov1 = net_data[, cov1], 
                                       cov2 = net_data[, cov2], 
                                       a = prop, 
                                       b = 1 - prop, 
                                       invert_cov1 = invert_cov1, 
                                       invert_cov2 = invert_cov2)
  
  transformed_cov1 <- transformed_weights$cov1_comb
  transformed_cov2 <- transformed_weights$cov2_comb
  
  weighted_segments <- cbind(net_data, transformed_cov1)
  weighted_segments <- cbind(weighted_segments, transformed_cov2) 
  weighted_segments <- cbind(weighted_segments, transformed_cov1 + transformed_cov2) 
  
  colnames(weighted_segments) <- c(colnames(net_data), 
                                   paste0("T(", cov1, ")"), 
                                   paste0("T(", cov2, ")"),
                                   paste0("W(l_i)"))
  
  weighted_segments$id <- rep(1:nrow(weighted_segments))
  
  return(graph_from_data_frame(weighted_segments, directed = FALSE, vertices = node_data))
}


#' Calculates the linear combination between two covariates (based on formula W(l_i))
#' 
#' @name weighted_data
#' @param cov1 vector containing the first covariate data
#' @param cov2 vector containing the second covariate data
#' @param a weighting for cov1 (a+b=1)
#' @param b weighting for cov2 (a+b=1)
#' @param invert_cov1 invert covariate 1 (max(cov1) - cov1_i), default FALSE
#' @param invert_cov2 invert covariate 2 (max(cov2) - cov2_i), default FALSE
#' @return list with weighted cov1 and cov2
#' 
weighted_data <- function(cov1, cov2, a, b, invert_cov1 = FALSE, invert_cov2 = FALSE){
  
  if(invert_cov1) cov1 <- mapply(FUN = `-`, max(cov1), cov1)
  if(invert_cov2) cov2 <- mapply(FUN = `-`, max(cov2), cov2)
  
  transformed_cov1 <- mapply(FUN = `-`, cov1, min(cov1))
  
  transformed_cov1 <- mapply(FUN = `/`, transformed_cov1, (max(cov1)-min(cov1)))
  
  transformed_cov1 <- mapply(FUN = `*`, transformed_cov1, a)
  
  transformed_cov2 <-mapply(FUN = `-`, cov2, min(cov2))
  
  transformed_cov2 <- mapply(FUN = `/`, transformed_cov2, (max(cov2)-min(cov2)))
  
  transformed_cov2 <- mapply(FUN = `*`, transformed_cov2, b)
  
  return(list(cov1_comb = transformed_cov1, cov2_comb = transformed_cov2))
}

#' Gives all the shortest paths lenght between each pair of nodes of the given graph (network)
#' 
#' @name get_all_paths
#' @param graph The graph on which calculates the paths
#' @return matrix with all the shortest path lenghts
#' 
get_all_paths <- function(graph){
  direct <- get.adjacency(graph)
  indirect <- direct
  max <- vcount(graph)-1
  for(i in 1:max){
    for(j in 1:max){
      indirect[i,j] <- length(get.all.shortest.paths(graph, from=i, to=j))
    }
  }
  return(indirect)
}

#' Function that returns the crossings of the network
#' 
#' @name getCrossings
#' @param graph The graph on which calculates the paths
#' @return crossings - array with an array of nodes with degree > 2
#' 
getCrossings = function(graph){
  crossings <- c()
  for(node in V(graph)){
    if(degree(graph, node)>2)
    {
      crossings <- c(crossings, node)
    }
  }
  crossings
}

#' Get the top k shortest paths between two nodes. It uses the library 'yenpathy', To install the library do the following:
#' library(remotes)
#' install_github("ecohealthalliance/yenpathy", build_vignettes = TRUE)
#' 
#' @name get_k_shortest_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param weight The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @param k number of shortest paths to be returned
#' @param show_weight show also the total weight of the paths, default = FALSE
#' @return list of lists containing the top k-shortest paths where each path is represented by the vertices constituting it
#' 
get_k_shortest_paths <- function(graph, from, to, weight = 'weight', k, show_weight = FALSE){
  # Get k shortest paths using 'yenpathy' library
  tmp_df <- as_data_frame(as.directed(graph, mode = "mutual")) # the graph must be converted to directed
  
  #Construct the dataframe that will be used to pass to the k_shortest_path 'yenpathy' function
  g_df <- data.frame(
    start = as.numeric(unlist(tmp_df$from)), # The ID's are characters (Ex: '1') must be converted to numbers
    end = as.numeric(unlist(tmp_df$to)),
    weight = as.numeric(unlist(tmp_df[, weight]))
  )
  
  shortest_paths <- yenpathy::k_shortest_paths(g_df, from = from, to = to, k=k)
  
  if(show_weight){
    paths_tweight <- list()
    
    for(element in shortest_paths){
      paths_tweight <- append(paths_tweight, list(list(path = element, weight = PathWeight(graph, element, weight))))
    }
    
    paths_tweight
    
  }else shortest_paths
}

#' Return the total weight of the given path
#' 
#' @name PathWeight
#' @param graph The igraph class network
#' @param path A sequence of nodes which forms the path
#' @param weight the type of weight to be calculated 
#' 
#' @return the total specified weight of the given path
PathWeight <- function(graph, path, weight){
  sum(igraph::edge_attr(graph = graph, name = weight, index = igraph::E(graph, path = unlist(path))))
}

#' Get all the paths between two nodes and gives information about its weight (accident intensity),
#' distance, transformed weight and transformed distance (weight and distance
#' divided by its respective maximums in the network)
#' 
#' @name paths_info
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @return list of lists containing the all the paths between the two given nodes with information 
#' related to them.
#' 
paths_info <- function(graph, from, to){
  all_paths <- all_simple_paths(graph, from = from, to = to)
  
  ipaths <- list()
  
  for (path in all_paths){
    
    tmp_lst <- list(from = from, to=to)
    tmp_lst <- c(tmp_lst, path = list(as.numeric(unlist(as_ids(path)))))
    
    for(attribute in igraph::edge_attr_names(graph)){
      tmp_lst[[attribute]] <- sum( igraph::edge_attr( graph, attribute,  igraph::E( graph, path = unlist(path) ) ))
    }
    ipaths[[length(ipaths)+1]] <- tmp_lst
  }
  return(ipaths)
}


#' Get all the paths between two nodes and gives in which position are each of its weights (best to worst)
#' 
#' @name rate_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @return list of lists containing the all the paths with their rated by position.
#' 
rate_paths <- function(graph, from, to){
  ipaths <- paths_info(graph = graph, from = from, to = to)
  
  if (length(ipaths) == 0){
    return(ipaths)
  }
  attr_names <- names(ipaths[[1]])
  
  for(name_idx in 4:length(attr_names)){
    if(name_idx != 'id'){
      ipaths <- ipaths[order(sapply(ipaths,'[[',name_idx))]
      for(i in 1:length(ipaths)){
        #ipaths[[i]] <- append(ipaths[[i]], list(n_distance=i))
        ipaths[[i]][[paste0('n_', attr_names[name_idx])]] <- i
      }
    }
  }
  return(ipaths)
}


#' Get all the paths between two nodes ordered from less to more weight.
#' 
#' @name ordered_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param weight The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @return list of lists containing the all the paths ordered by weight.
#' 
ordered_paths <- function(graph, from, to, weight){
  all_paths <- all_simple_paths(graph, from=from, to=to)
  
  paths_ordered <- list()
  
  for (path in all_paths){
    
    weight_sum <- sum(igraph::edge_attr(graph, weight, igraph::E(graph, path = unlist(path))))
    
    paths_ordered[[length(paths_ordered)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
  }
  return(paths_ordered[order(sapply(paths_ordered,'[[',1))])
}



#' Get all the paths between two nodes ordered from less to more weight.
#' All paths that contains edges with weight greater to the specified 
#' in the function are saved in another list.
#' 
#' @name filter_paths_old
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param weight The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @param filter <array> Weight limits that an edge of a path can contain
#' @param paths The paths that want to be filtered, if it's empty, will calculate all paths
#' @return list of lists containing the all the paths ordered by weight
#' and another identical list but with the paths with edges that has 
#' the sepecified limit weight or greater.
#' 
filter_paths_old <- function(graph, from, to, weight, filter, paths = NULL){
  if (is.null(paths)){
    paths <- all_simple_paths(graph, from=from, to=to)
  }
  
  
  paths_ordered <- list()
  black_list <- list()
  is_forbiden <- FALSE
  
  for (path in paths){
    weight_sum <- 0
    is_forbiden <- FALSE
    for (edge in path){
      if(edge_attr(g, weight)[edge] >= filter){
        is_forbiden <- TRUE
        break
      }
      weight_sum <- weight_sum + edge_attr(graph, weight)[edge]
    }
    
    if(!is_forbiden){
      if(length(paths_ordered)==0){
        paths_ordered <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
      }else{
        paths_ordered <- list(paths_ordered, list(weight = weight_sum, path = as.numeric(unlist(as_ids(path)))))
      }
      
    }
    # if (is_forbiden){
    #   black_list[[length(black_list)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    # }
    # 
    # else{
    #   paths_ordered[[length(paths_ordered)+1]] <- list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    # }
    
  }
  return(list(paths = paths_ordered[order(sapply(paths_ordered,'[[',1))]))
  # return(list(paths = paths_ordered[order(sapply(paths_ordered,'[[',1))], 
  #             black_list = black_list[order(sapply(black_list,'[[',1))]))
}


#' Get all the paths between two nodes ordered from less to more weight.
#' All paths that contains edges with weight greater to the specified 
#' in the function are saved in another list.
#' 
#' @name filter_paths
#' @param graph The graph on which calculates the paths
#' @param from Source node.
#' @param to Ending node.
#' @param weight The weight to calculate the shortests paths, can be 'weight' or 'distance'
#' @param filters <array> Weight limits that an edge of a path can contain
#' @param paths The paths that want to be filtered, if it's empty, will calculate all paths
#' @return list of lists containing the all the paths ordered by weight
#' and another identical list but with the paths with edges that has 
#' the sepecified limit weight or greater.
#' 
filter_paths <- function(graph, from, to, weight, filters, paths = NULL){
  paths_ordered <- vector(mode="list", length=length(filters))
  names(paths_ordered) <- filters
  filters <- as.numeric(unlist(filters)) # transform strings to integuers
  is_forbiden <- FALSE
  
  max_parameter = max(igraph::edge_attr(graph, weight))
  
  if (is.null(paths)){
    paths <- igraph::all_simple_paths(graph, from=from, to=to)
  }
  
  pb = txtProgressBar(min = 0, max = length(paths), initial = 0) 
  counter <- 0
  cat("Filtering paths...\n")
  for(path in paths){
    setTxtProgressBar(pb,counter)
    weight_sum <- 0
    is_forbiden <- TRUE
    filters_idx <- 1
    
    path_weights <- igraph::edge_attr(graph, weight,  igraph::E(graph, path = unlist(path)))
    
    for(percent in filters){
      
      if(max(path_weights) < (max_parameter*(percent/100))){
        weight_sum <- sum(path_weights)
        is_forbiden <- FALSE
        break
      }
      
      if(is_forbiden){
        filters_idx <- filters_idx+1
      }
    }
    
    if(!is_forbiden){
      paths_ordered[[toString(filters[filters_idx])]][[length(paths_ordered[[toString(filters[filters_idx])]])+1]] <- 
        list(weight = weight_sum, path = as.numeric(unlist(as_ids(path))))
    }
    counter <- counter + 1
  }
  close(pb)
  
  #return(list(paths = paths_ordered[order(sapply(paths_ordered,'[[',1))]))
  return(paths_ordered)
}

#' Delete all the edges with weight equal to or greater than the specified amount
#'
#' @name filter_graph
#' @param graph The graph on which calculates the paths
#' @param filter Maximum weight that the edges can contain
#' @param weight Weight type, can be 'distance' or 'weight'
#' @return graph without the edges with weight equal or greater than the given filter
#' 
filter_graph <- function(graph, filter, weight){
  max_distance  = max(igraph::edge_attr(g, weight))
  return(delete.edges(graph, which(igraph::edge_attr(g, weight) >= (max_distance*(filter/100)))))  
}


#' Print the map of the given ppp object with the given path as green nodes, it also print the other nodes as red
#' 
#' @name print_path_ppp
#' @param ppp_obj Point pattern object
#' @param path Path to be plotted
#' @return a plot of the map with the given path
#' 
print_path_ppp <- function(ppp_obj, path){
  ln_vertices<- linnet(ppp_obj, edges=edgs)
  plot(ln_vertices,col="blue")
  points(ppp_obj,pch=19,col="red")
  points(ppp_obj[path],pch=19,col="green")
}


#' Print the map of the given a 'igraph' object with the given path as green nodes
#' as well as the edges
#' 
#' @name print_path_graph
#' @param graph <igraph> Graph object
#' @param path Path to be plotted
#' @param color <string> Color of the path 
#' @return a plot of the map with the given path
#' 
print_path_graph <- function(graph, path, color){
  vcol <- rep("black", vcount(graph))
  vcol[path] <- color
  vcol[path[1]] <- "sandybrown" # Path first node color
  vcol[path[length(path)]] <- "red" # Path last node color
  
  ecol <- rep("black", ecount(graph))
  ecol[E(graph, path=path)] <- color
  
  E(graph)$width <- 1
  E(graph, path=path)$width <- 3
  
  mtx = matrix(cbind(vertex_attr(graph)$V1, vertex_attr(graph)$V2), ncol=2)
  plot(graph, layout = mtx, 
       vertex.size=3, 
       vertex.color=vcol, 
       #vertex.frame.color=vcol,
       mark.border=NA,
       vertex.label="",
       edge.color=ecol,
       window=FALSE, axes=FALSE)
  rect(-1,-1,1,1,border="black",lwd=2)
}


#' Plot linnet object
#' 
#' @name plot.lpp.lines
#' @param LNnew Linnet object
#' @param width_line Witdth of the plotted lines
#' 
plot.lpp.lines <- function(LNnew,seg_m,width_line=0.1){
  
  x0<-c();y0<-c();x1<-c();y1<-c()
  for(i in 1:LNnew$lines$n){
    x0[i]<-LNnew$lines$ends$x0[i]
    y0[i]<-LNnew$lines$ends$y0[i]
    x1[i]<-LNnew$lines$ends$x1[i]
    y1[i]<-LNnew$lines$ends$y1[i]
  }
  
  #Generate points
  j1<-0;x<-c();y<-c();mk<-c()
  for(i in 1:LNnew$lines$n){
    m<-(y1[i]-y0[i])/(x1[i]-x0[i])
    a<-y0[i]-m*x0[i]
    for(j in 1:100){
      j1<-j1+1
      x[j1]<-runif(1,min(x0[i],x1[i]),max(x0[i],x1[i]))
      y[j1]<-m*x[j1]+a
      mk[j1]<-seg_m[i]
    }
  }
  pppp<-ppp(x,y,marks=mk,window=LNnew$window)
  
  plot(LNnew, main="")
  plot(pppp, pch=19,cex=width_line,cols=topo.colors(100),add=TRUE, main="", image=TRUE)
  rect(LNnew$window$xrange[1],LNnew$window$yrange[1],LNnew$window$xrange[2],LNnew$window$yrange[2],
       border="black",lwd=1)
}


#' Combine two sets of weights into one and using a proportion.
#' 
#' @name combine_weights
#' @param data1 <dataframe> With weights
#' @param data2 <dataframe> With weights
#' @param prop Proportion used in data1 (data2 will use 1-proportion), must be less or equal than 1
#' @return <dataframe> with the weights combined
#' 
combine_weights <- function(data1, data2, prop = 0.5){
  if (prop > 1){
    print("Weight must equal or less than 1. Continuing without proportion.")
    prop <- 1
  }
  
  
  transformed_data1 <- data.frame(V1=mapply(FUN = `*`, data1, prop, SIMPLIFY = FALSE))
  transformed_data2 <- data.frame(V1=mapply(FUN = `*`, data2, (1-prop), SIMPLIFY = FALSE))
  return (data.frame(V1 = transformed_data1+transformed_data2))
}


#' This function plots a heatmap of the given network attribute, or the plain network if no attribute is given.
#' 
#' @name PlotNetwork
#' 
#' @param g igraph object
#' @param mode a string with the desired heatmap (edge attribute) to be plotted or 'none' to plot the plain map (default).
#' @param high_size multiplier for edge and node size
#' @param net_vertices chosen vertices to plot the heatmap (or its related edges in case to plot the edge heatmap)
#' @param net_edges chosen edges to plot the heatmap, can be either the edge id's or its node endpoints (e.j. c(1,2, 2,3, 7,8))
#' @param events if the vector of events occurring on the network are given, then will be shown as orange squares, NULL by default
#' @param alpha optional argument to set the transparency of the events (show_events = TRUE). The range is from 0.1 (transparent) to 1 (opaque). Default: alpha = 1
#' @param ... extra arguments for the class ggplot
#' 
#' @return The plot of the heatmap with class c("gg", "ggplot")
#' 
PlotNetwork <- function(g, net_vertices = NULL, net_edges = NULL, mode = 'none', high_size = 1, events = NULL, alpha = 1, ...){
  
  data_df <- data.frame(xcoord = igraph::vertex_attr(g, 'xcoord'), 
                        ycoord = igraph::vertex_attr(g, 'ycoord'))
  
  highlighted_df <- data_df[as.numeric(net_vertices),]
  
  
  node_coords <- data.frame(xcoord = igraph::vertex_attr(g)$xcoord, ycoord = igraph::vertex_attr(g)$ycoord)
  rownames(node_coords) <- igraph::vertex_attr(g)$name
  #get edges, which are pairs of node IDs
  edgelist <- igraph::get.edgelist(g)
  #convert to a four column edge data frame with source and destination coordinates
  edges_df <- data.frame(node_coords[edgelist[,1],], node_coords[edgelist[,2],])
  colnames(edges_df) <- c("xcoord1","ycoord1","xcoord2","ycoord2")
  
  
  if(mode == 'none'){
    if( is.null(net_vertices) && is.null(net_edges) ){
      hplot <- ggplot2::ggplot(data_df, ggplot2::aes_string(x = 'xcoord', y = 'ycoord'), ...) + 
        ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                                  xend = 'xcoord2', yend = 'ycoord2'), 
                              data = edges_df, 
                              size = 0.8, 
                              colour = "grey") +
        ggplot2::geom_point(shape = 19, 
                            size = 1.7) +
        ggplot2::scale_y_continuous(name = "y-coordinate") + 
        ggplot2::scale_x_continuous(name = "x-coordinate") + 
        ggplot2::theme_bw()
    }else{
      edge_ends <- igraph::ends(g, net_edges)
      
      #convert to a four column edge data frame with source and destination coordinates
      sub_edges_df <- data.frame(node_coords[edge_ends[,1],], node_coords[edge_ends[,2],])
      colnames(sub_edges_df) <- c("xcoord1","ycoord1","xcoord2","ycoord2")
      
      hplot <- ggplot2::ggplot(data_df, ggplot2::aes_string(x = 'xcoord', y = 'ycoord'), ...) + 
        ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                                  xend = 'xcoord2', yend = 'ycoord2'),
                              data = edges_df,
                              size = 0.8,
                              colour = 'grey') +
        ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                                  xend = 'xcoord2', yend = 'ycoord2'),
                              data = sub_edges_df,
                              size = 0.8 * high_size,
                              colour = 'green') +
        ggplot2::geom_point(shape = 19, 
                            size = 1.7,
                            colour="gray") +
        ggplot2::geom_point(data = highlighted_df,
                            shape = 19,
                            size = 1.7 * high_size,
                            colour = 'darkgreen',
                            ggplot2::aes_string(x = 'xcoord', y = 'ycoord')) +
        ggplot2::scale_y_continuous(name = "y-coordinate") + 
        ggplot2::scale_x_continuous(name = "x-coordinate") + 
        ggplot2::theme_bw()
    }
  }else{
    if(is.null(net_edges)){
      net_edges <- igraph::E(g)
    }
    
    if(length(net_edges) == length(igraph::E(g))){
      edge_int <- igraph::edge_attr(g, mode)
      
      hplot <- ggplot2::ggplot(data_df, ggplot2::aes_string(x = 'xcoord', y = 'ycoord'), ...) +
        viridis::scale_color_viridis(option = 'H') +
        ggplot2::labs(title = paste0(mode, ' Heatmap\n'),
                      color = mode) +
        ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                                  xend = 'xcoord2', yend = 'ycoord2',
                                                  colour = 'edge_int'),
                              data = edges_df,
                              size = 0.8) +
        ggplot2::geom_point(shape = 19,
                            size = 1.7,
                            colour="gray") +
        ggplot2::scale_y_continuous(name = "y-coordinate") +
        ggplot2::scale_x_continuous(name = "x-coordinate") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title = ggplot2::element_text(face = "bold"),
                       plot.title = ggplot2::element_text( size = 14,
                                                           face = "bold",
                                                           hjust = 0.5) )
    }else{
      edge_ends <- igraph::ends(g, net_edges)
      
      edge_int <- igraph::edge_attr(g, mode, net_edges)
      
      #convert to a four column edge data frame with source and destination coordinates
      sub_edges_df <- data.frame(node_coords[edge_ends[,1],], node_coords[edge_ends[,2],])
      colnames(sub_edges_df) <- c("xcoord1","ycoord1","xcoord2","ycoord2")
      
      hplot <- ggplot2::ggplot(data_df, ggplot2::aes_string(x = 'xcoord', y = 'ycoord'), ...) +
        viridis::scale_color_viridis(option = 'H') +
        ggplot2::labs(title = paste0(mode, ' Heatmap\n'),
                      color = mode) +
        ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                                  xend = 'xcoord2', yend = 'ycoord2'),
                              data = edges_df,
                              size = 0.8,
                              colour = 'grey') +
        ggplot2::geom_segment(ggplot2::aes_string(x = 'xcoord1', y = 'ycoord1', 
                                                  xend = 'xcoord2', yend = 'ycoord2', 
                                                  colour = 'edge_int'),
                              data = sub_edges_df,
                              size = 0.8) +
        ggplot2::geom_point(shape = 19,
                            size = 1.7,
                            colour="gray") +
        ggplot2::scale_y_continuous(name = "y-coordinate") +
        ggplot2::scale_x_continuous(name = "x-coordinate") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title = ggplot2::element_text(face = "bold"),
                       plot.title = ggplot2::element_text( size = 14,
                                                           face = "bold",
                                                           hjust = 0.5) )
    }
  } 
  
  if(!is.null(events)){
    hplot + ggplot2::geom_point(data = as.data.frame(events),
                                mapping = ggplot2::aes(x = events[,1], y = events[,2]),
                                shape = 22, fill = 'orange', color = 'orange',
                                alpha = alpha)
  }else{
    hplot
  }
}