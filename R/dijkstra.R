#' Dijkstra algorithm - shortest node-to-node distances (base R)
#' Ref: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @param graph
#' data frame with three variables:
#' v1, v2 = from and to nodes respectively (integers)
#' w = weight of edge
#' @param init_node (integer, present in graph)
#' @export
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

  return(nodes$dist)
}
