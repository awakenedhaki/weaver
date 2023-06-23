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
