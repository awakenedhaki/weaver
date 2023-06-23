#' Calculate Jaccard Index
#'
#' Calculates the Jaccard index between two sets.
#'
#' @param set1 First set.
#' @param set2 Second set.
#'
#' @return Jaccard index value.
#'
#' @export
jaccard_index <- function(set1, set2) {
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  similarity <- intersection / union

  return(similarity)
}

#' Generate Pairs
#'
#' Generates pairs of gene sets from a tibble based on the provided ontology and gene set.
#'
#' @param tbl Tibble containing the gene sets.
#' @param ontology Name of the ontology column in the tibble
#' @param gene_set Name of the gene set column in the tibble
#'
#' @return A tibble with pairs of gene sets and associated ontologies.
#'
#' @import dplyr
#' @import tibble
#' @importFrom purrr map
#'
#' @export
generate_pairs <- function(tbl, ontology, gene_set) {
  tbl |>
    dplyr::pull(var = {{ gene_set }}, name = {{ ontology }}) |>
    combn(m = 2, simplify = FALSE) |>
    tibble::enframe(name = "pair", value = "gene_set") |>
    dplyr::mutate(ontology = purrr::map(gene_set, names))
}


#' Calculate Jaccard Index for Pairs
#'
#' Calculates the Jaccard index for pairs of gene sets based on the provided ontology and gene set.
#'
#' @param tbl Tibble containing the gene sets.
#' @param ontology Name of the ontology column in the tibble
#' @param gene_set Name of the gene set column in the tibble
#'
#' @return A tibble with pairs of gene sets, associated ontologies, and Jaccard index values.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom purrr map
#' @importFrom tidyr unnest, unnest_wider
#'
#' @export
calculate_jaccard_index <- function(tbl, ontology, gene_set) {
  pairs <- generate_pairs(tbl, ontology, gene_set)

  pairs |>
    dplyr::mutate(index = purrr::map(gene_set, ~jaccard_index(.x[[1]], .x[[2]]))) |>
    tidyr::unnest(cols = index) |>
    tidyr::unnest_wider(col = ontology, names_sep = "_") |>
    dplyr::select(ontology_1, ontology_2, index)
}
