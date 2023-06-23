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

#' Calculate Node Ranks
#'
#' Calculates the ranks of nodes based on the provided table.
#'
#' @param tbl Data table containing the node information.
#' @param subgraph_id Indetifier for a given subgraph.
#'
#' @return A tibble with the calculated ranks for each node.
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
calculate_node_ranks <- function(tbl, subgraph_id) {
  tbl <- tbl |>
    dplyr::select(-from, -to) |>
    tidyr::pivot_longer(cols = c(from_name, to_name),
                        values_to = "ontology") |>
    dplyr::select(-name) |>
    dplyr::add_count(ontology, name = "n_edges")

  ranked <- tbl |>
    dplyr::mutate(total_index = sum(index), .by = ontology) |>
    dplyr::distinct(ontology, n_edges, total_index, total_n_edges) |>
    dplyr::mutate(subgraph = subgraph_id,
                  rank = (total_index * (n_edges / total_n_edges))) |>
    dplyr::arrange(dplyr::desc(rank))

  return(ranked)
}

#' Rank Subgraph Nodes
#'
#' Ranks the nodes in a graph based on subgraphs and minimum number of vertices.
#'
#' @param graph Graph object to rank the nodes.
#' @param min_vertices Minimum number of vertices required for a subgraph.
#'
#' @return A tibble with the ranked nodes.
#'
#' @import igraph
#' @import tibble
#'
#' @export
rank_nodes <- function(graph, min_vertices = 2) {
  subgraphs <- igraph::decompose(graph, min.vertices = min_vertices)

  ranked_nodes <- tibble::tibble()
  for (i in 1:length(subgraphs)) {
    subgraph <- subgraphs[[i]]
    total_n_edges <- igraph::ecount(subgraph)

    tbl <- igraph::as_long_data_frame(subgraph) |>
      dplyr::mutate(total_n_edges = total_n_edges) |>
    ranked <- calculate_node_ranks(tbl, subgraph_id = i)

    ranked_nodes <- rbind(ranked_nodes, ranked)
  }

  return(ranked_nodes)
}
