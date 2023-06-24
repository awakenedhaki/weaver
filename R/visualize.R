#' Plot Graph
#'
#' This function plots a graph using the ggraph package.
#' It performs the following steps:
#' - Decomposes the input graph into subgraphs based on the specified minimum number of vertices using `decompose()`.
#' - Binds the subgraphs together into a single graph using the `bind_subgraphs()` function.
#' - Optionally removes bridges from the graph using the `bridge_remover()` function.
#' - Plots the resulting graph using the Fruchterman-Reingold layout in ggraph.
#' - Uses alpha blending of edges based on the index attribute.
#' - Colors nodes as light grey with a larger size for the subgraph nodes, and black with a smaller size for other nodes.
#' - Applies a void theme to the plot and removes the legend.
#'
#' @param graph The input graph.
#' @param remove_bridges Logical indicating whether to remove bridges from the graph (default is FALSE).
#' @param min_vertices The minimum number of vertices required for subgraph decomposition (default is 3).
#'
#' @import igraph
#' @importFrom ggraph ggraph geom_edge_link geom_node_point
#' @importFrom ggplot2 aes theme_void theme
#'
#' @export
plot_graph <- function(graph, remove_bridges = FALSE, min_vertices = 3) {
  graph <- igraph::decompose(graph, min.vertices = min_vertices) |>
    bind_subgraphs()

  if (remove_bridges) {
    graph <- graph |>
      bridge_remover()
  }

  ggraph::ggraph(graph, layout = "fr") +
    ggraph::geom_edge_link(ggplot2::aes(alpha = index)) +
    ggraph::geom_node_point(color = "lightgrey", size = 4) +
    ggraph::geom_node_point(color = "black", size = 3) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
