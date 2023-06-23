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

scaled_eigenvector_ranker <- function(graph) {
  igraph::evcent(graph, directed = FALSE, scale = TRUE)$vector |>
    tibble::enframe(name = "ontology", value = "eigen_rank") |>
    dplyr::arrange(dplyr::desc(eigen_rank))
}

normalized_degree_ranker <- function(graph) {
  igraph::degree(graph, mode = "total", normalize = TRUE) |>
    tibble::enframe(name = "ontology", value = "degree") |>
    dplyr::arrange(dplyr::desc(degree))
}

rank_subgraph_nodes <- function(graph, ranker, min_vertices = 3, remove_bridges = FALSE) {
  subgraphs <- graph |>
    igraph::decompose(min.vertices = min_vertices)

  if (remove_bridges) {
    subgraphs <- lapply(subgraphs, igraph::as_data_frame) |>
      dplyr::bind_rows() |>
      igraph::graph_from_data_frame(directed = FALSE) |>
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
