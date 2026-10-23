#' Extreme cases in words
#'
#' Lists the vertices (extreme cases) of a model and, for probabilities of
#' exactly 0 and 1, replaces the numbers by the words `"none"` and `"all"`, as
#' in the paper's tables. This reads naturally when the probabilities are
#' proportions of a finite population that show a behaviour. Other values are
#' shown as fractions.
#'
#' @param h A model (string with `n_p`, or H-representation).
#' @param labels Optional column labels (length `n_p`).
#' @param n_p Number of probabilities for model strings.
#' @param none,all The words for 0 and 1.
#' @param sort Order the rows (vertices) lexicographically? Default `TRUE`.
#'   The vertex order of the underlying algorithm is arbitrary.
#'
#' @return A data frame with one row per vertex, class `"fairy_words"`.
#'
#' @examples
#' fairy_words("p1 > p2 > p3", labels = c("A", "B", "C"))
#' @importFrom MASS fractions
#' @export
fairy_words <- function(h, labels = NULL, n_p = NULL, none = "none", all = "all", sort = TRUE) {
  v <- fairy_vertices(h, n_p = n_p)
  if (sort && nrow(v) > 0) v <- v[do.call(order, as.data.frame(unname(v))), , drop = FALSE]
  tr <- function(x) {
    out <- as.character(fractions(x))
    out[abs(x) < 1e-12] <- none
    out[abs(x - 1) < 1e-12] <- all
    out
  }
  df <- as.data.frame(lapply(seq_len(ncol(v)), function(j) tr(v[, j])), stringsAsFactors = FALSE)
  colnames(df) <- if (!is.null(labels)) labels else colnames(v)
  rownames(df) <- paste0("V", seq_len(nrow(df)))
  class(df) <- c("fairy_words", "data.frame")
  df
}

#' @export
print.fairy_words <- function(x, ...) {
  print.data.frame(x, right = TRUE, ...)
  invisible(x)
}

#' The constraints of a model as readable text
#'
#' Turns an H-representation into one line per constraint, in the form
#' `-1 * p1 + 1 * p2 <= 0` used by the paper's Figure 2. Equalities are
#' written with `=`.
#'
#' @param h An H-representation from [fairy_h()] (or a model string).
#' @param n_p Number of probabilities for model strings.
#' @param labels Optional names for the probabilities.
#' @param times Symbol between coefficient and variable.
#'
#' @return A character vector.
#' @examples
#' fairy_constraints("p1 > p2 > p3")
#' @importFrom MASS fractions
#' @export
fairy_constraints <- function(h, n_p = NULL, labels = NULL, times = " * ") {
  h <- resolve_h(h, n_p = n_p)
  hn <- q2d(h)
  nm <- if (is.null(labels)) paste0("p", seq_len(ncol(h) - 2L)) else labels
  num <- function(x) as.character(fractions(x))
  vapply(seq_len(nrow(hn)), function(i) {
    a <- -hn[i, -(1:2)]     # b + a.x >= 0  is  -a.x <= b
    b <- hn[i, 2]
    if (hn[i, 1] == 0 && all(a == 0)) return(NA_character_)
    terms <- character()
    for (j in which(a != 0)) {
      terms <- c(terms, paste0(if (a[j] > 0) "+ " else "- ", if (abs(a[j]) == 1) "" else paste0(num(abs(a[j])), times), nm[j]))
    }
    lhs <- paste(terms, collapse = " ")
    lhs <- sub("^\\+ ", "", lhs)
    paste(lhs, if (hn[i, 1] == 1) "=" else "<=", num(b))
  }, character(1))
}
