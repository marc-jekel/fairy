#' fairy: formalize and analyze order-constrained hypotheses about proportions
#'
#' Turn verbal hypotheses about proportions into order-constrained models,
#' list their extreme cases and boundary constraints, measure their
#' parsimony, and compare them. The functions mirror the steps of the
#' mathematics fairy in Jekel, Regenwetter, Chen, and Line (in preparation).
#'
#' @section Main functions:
#' * [fairy_h()], [fairy_v()], [fairy_vertices()], [fairy_words()]: the
#'   constraints (H-representation) and extreme cases (V-representation) of a model.
#' * [fairy_volume()], [fairy_max_bf()], [fairy_info()]: parsimony, the largest
#'   possible Bayes factor, and the numbers reported in the paper's tables.
#' * [fairy_intersect()], [fairy_mixture()], [fairy_nested()], [fairy_compare()]:
#'   overlap, mixtures and model relationships.
#' * [fairy_replicate()]: identical, joint, and substitutable replication models.
#' * [fairy_write_qtest()], [fairy_write_multinomineq()]: export for QTest and multinomineq.
#'
#' @section Vignettes:
#' `vignette("fairy")` gives an overview of all functions with examples;
#' `vignette("cringe", package = "fairy")` reproduces the paper's examples step by step; the models
#' are entered as constraint strings in the vignette, the package does not store them.
#'
#' @keywords internal
#' @importFrom utils tar write.table
"_PACKAGE"
