#' Generate Graph
#'
#' Generates a graph from a tibble based on the specified criteria.
#'
#' @param tbl Tibble containing the graph information.
#' @param n Maximum number of rows to include in the graph.
#' @param prop Proportion of rows to include in the graph.
#'
#' @return A graph object representing the generated graph.
#'
#' @import purrr
#' @import dplyr
#' @import igraph
#'
#' @export
generate_graph <- function(tbl, n = NULL, prop = NULL) {
  if (!is.null(n)) {
    max_slicer <- purrr::partial(dplyr::slice_max, n = n)
  } else if (!is.null(prop)) {
    max_slicer <- purrr::partial(dplyr::slice_max, prop = prop)
  }

  tbl |>
    max_slicer(order_by = index) |>
    igraph::graph_from_data_frame(directed = FALSE)
}

#' Remove Bridges and Isolated Vertices from a Graph
#'
#' This function removes bridges (edges whose removal would increase the number
#' of connected components) and isolated vertices (vertices with no incident edges)
#' from a graph.
#'
#' @param graph The input graph.
#'
#' @return The pruned graph without bridges and isolated vertices.
#'
#' @importFrom igraph delete_edges
#' @importFrom igraph bridges
#' @importFrom igraph delete.vertices
#' @importFrom igraph degree
#'
#' @export
bridge_remover <- function(graph) {
  pruned <- igraph::delete_edges(graph, igraph::bridges(graph))
  pruned <- igraph::delete.vertices(pruned, which(igraph::degree(pruned) == 0))

  return(pruned)
}
