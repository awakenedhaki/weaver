#' Calculate Normalized Jaccard Rank for Subgraph Ontologies
#'
#' This function calculates the normalized Jaccard rank for subgraph ontologies.
#' It takes a subgraph as input and performs the following steps:
#' - Converts the subgraph to a long-format data frame.
#' - Counts the number of edges for each ontology in the subgraph.
#' - Ranks the ontologies based on the Jaccard similarity index.
#' - Returns the ranked ontologies in descending order of Jaccard rank.
#'
#' @param subgraph The input subgraph.
#'
#' @return A data frame containing the ranked ontologies and their Jaccard ranks.
#'
#' @importFrom igraph ecount
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select, add_count, mutate, distinct, arrange, desc
#'
#' @export
normalized_jaccard_ranker <- function(subgraph) {
  total_n_edges <- igraph::ecount(subgraph)

  tbl <- subgraph |>
    igraph::as_long_data_frame() |>
    dplyr::select(-from, -to) |>
    tidyr::pivot_longer(cols = c(from_name, to_name),
                        values_to = "ontology") |>
    dplyr::select(-name) |>
    dplyr::add_count(ontology, name = "n_edges")

  ranked <- tbl |>
    dplyr::mutate(total_index = sum(index), .by = ontology) |>
    dplyr::distinct(ontology, n_edges, total_index) |>
    dplyr::mutate(jaccard_rank = (total_index * (n_edges / total_n_edges))) |>
    dplyr::select(ontology, jaccard_rank) |>
    dplyr::arrange(dplyr::desc(jaccard_rank))

  return(ranked)
}

#' Calculate Scaled Eigenvector Rank for Graph Ontologies
#'
#' This function calculates the scaled eigenvector rank for graph ontologies.
#' It takes a graph as input and performs the following steps:
#' - Calculates the eigenvector centrality for the graph.
#' - Scales the eigenvector centrality values.
#' - Returns the ontologies along with their corresponding eigenvector ranks
#'   in descending order.
#'
#' @param graph The input graph.
#'
#' @return A tibble containing the ontologies and their scaled eigenvector ranks.
#'
#' @import igraph
#' @importFrom tibble enframe
#' @importFrom dplyr arrange, desc
#'
#' @export
scaled_eigenvector_ranker <- function(graph) {
  igraph::evcent(graph, directed = FALSE, scale = TRUE)$vector |>
    tibble::enframe(name = "ontology", value = "eigen_rank") |>
    dplyr::arrange(dplyr::desc(eigen_rank))
}

#' Calculate Normalized Degree Rank for Graph Ontologies
#'
#' This function calculates the normalized degree rank for graph ontologies.
#' It takes a graph as input and performs the following steps:
#' - Calculates the degree centrality for the graph, considering both incoming and outgoing edges.
#' - Normalizes the degree centrality values.
#' - Returns the ontologies along with their corresponding normalized degree ranks in descending order.
#'
#' @param graph The input graph.
#'
#' @return A tibble containing the ontologies and their normalized degree ranks.
#'
#' @import igraph
#' @importFrom tibble enframe
#' @importFrom dplyr arrange, desc
#'
#' @export
normalized_degree_ranker <- function(graph) {
  igraph::degree(graph, mode = "total", normalize = TRUE) |>
    tibble::enframe(name = "ontology", value = "degree_rank") |>
    dplyr::arrange(dplyr::desc(degree_rank))
}

#' Rank Nodes in Subgraphs of a Graph
#'
#' This function ranks the nodes in subgraphs of a graph based on a specified ranking function.
#' It takes a graph, a ranking function, and optional parameters as input and performs the following steps:
#' - Decomposes the graph into subgraphs based on a minimum number of vertices.
#' - If specified, removes bridges from each subgraph.
#' - Applies the ranking function to each subgraph and retrieves the ranked nodes.
#' - Returns a tibble containing the ranked nodes along with the corresponding subgraph indices.
#'
#' @param graph The input graph.
#' @param ranker A ranking function that takes a subgraph as input and returns ranked nodes.
#' @param min_vertices The minimum number of vertices required to form a subgraph (default: 3).
#' @param remove_bridges Logical indicating whether to remove bridges from each subgraph (default: FALSE).
#'
#' @return A tibble containing the ranked nodes and their corresponding subgraph indices.
#'
#' @import igraph
#' @importFrom tibble tibble, rbind
#' @importFrom dplyr mutate
#'
#' @export
rank_subgraph_nodes <- function(graph, ranker, min_vertices = 3, remove_bridges = FALSE) {
  subgraphs <- graph |>
    igraph::decompose(min.vertices = min_vertices)

  if (remove_bridges) {
    subgraphs <- bind_subgraphs(subgraphs) |>
      bridge_remover() |>
      igraph::decompose()
  }

  ranked_nodes <- tibble::tibble()
  for (i in 1:length(subgraphs)) {
    ranked <- ranker(subgraphs[[i]]) |>
      dplyr::mutate(subgraph = i)
    ranked_nodes <- rbind(ranked_nodes, ranked)
  }

  return(ranked_nodes)
}
