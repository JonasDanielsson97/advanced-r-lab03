#' Dijkstra algorithm - shortest node-to-node distances (base R)
#' Ref: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @param graph
#' data frame with three variables:
#' v1, v2 = from and to nodes respectively (integers)
#' w = weight of edge
#' @param init_node (integer, present in graph)
#'
#' @returns vector with the shortest distances from init_node to all nodes
#'
#' @examples
dijkstra <- function(graph, init_node){
  stopifnot("Graph data frame must contain columns v1, v2, and w" =
              all(c("v1", "v2", "w") %in% names(graph)))
  stopifnot("init_node must be a numeric scalar present in the graph" =
              is.numeric(init_node)
            & (length(init_node)==1)
            & (init_node %in% graph$v1))

  nodes <- data.frame(
    node = unique(graph$v1),
    dist = Inf,
    prev = NA_integer_,
    visited = FALSE)

  nodes$dist[nodes$node == init_node] <- 0

  while (any(!nodes$visited) & any(nodes$dist != Inf)){ # check second test
    unvisited <- nodes[!nodes$visited, ]
    focus_node <- unvisited$node[which.min(unvisited$dist)]

    focus_neighbors <- graph$v2[graph$v1 == focus_node]

    for (neighbor in focus_neighbors) {
      new_dist <-
        nodes$dist[nodes$node==focus_node] + graph$w[graph$v1==focus_node & graph$v2==neighbor]
      if (new_dist < nodes$dist[nodes$node == neighbor]) {
        nodes$dist[nodes$node == neighbor] <- new_dist
        nodes$prev[nodes$node == neighbor] <- focus_node
      }
    }
    nodes$visited[nodes$node == focus_node] <- TRUE
  }

  return(nodes$dist)
}


#' Dijkstra algorithm - shortest node-to-node distances (dplyr-pipe)
#' (dplyr pipe version)
#' Ref: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @param graph
#' data frame with three variables:
#' v1, v2 = from and to nodes respectively (integers)
#' w = weight of edge
#' @param init_node (integer, present in graph)
#'
#' @returns vector with the shortest distances from init_node to all nodes
#'
#' @examples
dijkstra_dplyr_pipe <- function(graph, init_node){
stopifnot("Graph data frame must contain columns v1, v2, and w" =
            all(c("v1", "v2", "w") %in% names(graph)))
stopifnot("init_node must be a numeric scalar present in the graph" =
            is.numeric(init_node)
          & (length(init_node)==1)
          & (init_node %in% graph$v1))

  nodes <-
    graph |>
    dplyr::select(v1) |>
    dplyr::distinct() |>
    dplyr::rename(node = v1) |>
    dplyr::mutate(dist =  Inf,
                  prev= NA_integer_,
                  visited = FALSE)

  nodes <-
    nodes |>
    dplyr::mutate(dist =
                    ifelse(node==init_node, 0, dist))

  focus_node <-
    nodes |>
    dplyr::filter(!visited) |>
    dplyr::slice_min(dist, n=1, with_ties = FALSE) |>
    dplyr::pull(node)

  while (any(!nodes$visited) & any(nodes$dist != Inf)){
    focus_node <-
      nodes |>
      dplyr::filter(!visited) |>
      dplyr::slice_min(dist, n=1, with_ties = FALSE) |>
      dplyr::pull(node)

    focus_neighbors <-
      graph |>
      dplyr::filter(v1 == focus_node) |>
      dplyr::select(v2) |>
      dplyr::pull()



    for (neighbor in focus_neighbors) {
      new_dist <-
        nodes |>
        dplyr::filter(node==focus_node) |>
        dplyr::pull(dist) +
        graph |>
        dplyr::filter(v1==focus_node & graph$v2==neighbor) |>
        dplyr::pull(w)

      nodes <- nodes |>
        dplyr::mutate(
          update = node == neighbor & new_dist < dist,
          dist = dplyr::if_else(update, new_dist, dist),
          prev = dplyr::if_else(update, focus_node, prev)
        ) |>
        dplyr::select(-update)

    }

    nodes <-
      nodes |>
      dplyr::mutate(visited = (visited | (node == focus_node)))

  }

  node_distances <-
    nodes |>
    dplyr::pull(dist)

  return(node_distances)
}

# wiki_graph <-
# data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#            v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#            w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

# removing redundancy in the undirected wiki_graph:
# wiki_graph |>
# dplyr::filter(v1<v2)


#' Dijkstra algorithm finding the shortest node-to-node distances
#' (dplyr pipe version)
#' Ref: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @param graph
#' data frame with three variables:
#' v1, v2 = from and to nodes respectively (integers)
#' w = weight of edge
#' @param init_node (integer, present in graph)
#'
#' @returns data frame with full indo on nodes, i.e.,
#' node name, distance from init_node, and previous node
#'
#' @examples
dijkstra_full <- function(graph, init_node){
  stopifnot("Graph data frame must contain columns v1, v2, and w" =
              all(c("v1", "v2", "w") %in% names(graph)))
  stopifnot("init_node must be a numeric scalar present in the graph" =
              is.numeric(init_node)
            & (length(init_node)==1)
            & (init_node %in% graph$v1))

  nodes <- data.frame(
    node = unique(graph$v1),
    dist = Inf,
    prev = NA_integer_,
    visited = FALSE)

  nodes$dist[nodes$node == init_node] <- 0

  while (any(!nodes$visited) & any(nodes$dist[!nodes$visited] != Inf)) {
    unvisited <- nodes[!nodes$visited, ]
    focus_node <- unvisited$node[which.min(unvisited$dist)]

    focus_neighbors <- graph$v2[graph$v1 == focus_node]

    for (neighbor in focus_neighbors) {
      new_dist <-
        nodes$dist[nodes$node==focus_node] + graph$w[graph$v1==focus_node & graph$v2==neighbor]
      if (new_dist < nodes$dist[nodes$node == neighbor]) {
        nodes$dist[nodes$node == neighbor] <- new_dist
        nodes$prev[nodes$node == neighbor] <- focus_node
      }
    }
    nodes$visited[nodes$node == focus_node] <- TRUE
  }

  ##### reverse walk to find paths

  nodes$path <- vector("list", nrow(nodes))

  for (node in nodes$node){
    current <- node
    path_walk <- c(current)
    while (!is.na(nodes$prev[nodes$node == current])) {
      current <- nodes$prev[nodes$node == current]
      path_walk <- c(current, path_walk)
      }
    nodes$path[nodes$node == node] <- list(path_walk)
  }

  setNames(nodes$dist, nodes$node)
  nodes <- nodes[-4]

  return(nodes)
}
