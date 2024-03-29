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
generate_graph <- function(tbl, n = NULL, prop = 1) {
  if (!is.null(n)) {
    max_slicer <- purrr::partial(dplyr::slice_max, n = n)
  } else {
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

#' Bind Subgraphs into a Single Graph
#'
#' This function takes a list of subgraphs and binds them together into a single graph.
#' It performs the following steps:
#' - Converts each subgraph into a data frame representation.
#' - Binds the data frames together using `bind_rows()`.
#' - Constructs a new graph from the combined data frame using `graph_from_data_frame()`.
#'
#' @param subgraphs A list of subgraphs.
#'
#' @return A single graph created by binding the subgraphs together.
#'
#' @import igraph
#' @importFrom dplyr bind_rows
#'
#' @export
bind_subgraphs <- function(subgraphs) {
  lapply(subgraphs, igraph::as_data_frame) |>
    dplyr::bind_rows() |>
    igraph::graph_from_data_frame(directed = FALSE)
}
