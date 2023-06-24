# Welcome to `weaver`!

The aim of `weaver` is to provide powerful gene set enrichment analysis (GSEA) exploration tools.

## Introduction

`weaver` allows you to explore and analyze GSEA results by performing multidimensional scaling using a Jaccard index matrix of gene sets. This approach captures the similarities between gene ontologies based on the intersection of their associated gene sets. The resulting graph, represented as an undirected `igraph` object, visualizes the relationships between gene ontologies.

## Key Features

- Multidimensional scaling of Jaccard index matrix to capture similarities between gene ontologies.
- Construction of an informative graph representation using the `igraph` package.
- Ranking of gene ontologies using three different metrics: total similarity score, eigencentrality, and normalized degree.
- Identification of the most informative gene ontology based on the highest rank within a subgraph.
- Facilitation of GSEA result manual curation using the ranked list of gene ontologies.

## How to Use weaver

TODO

## Contributing

If you have any suggestions, bug reports, or feature requests, please open an issue on GitHub.

## License

`weaver` is released under the MIT License.
