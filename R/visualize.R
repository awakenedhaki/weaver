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
