# Small helpers that put the H- and V-description of a model into a table

tbl_num <- function(x) {
  out <- as.character(MASS::fractions(x))
  out[abs(x) < 1e-12] <- "0"
  out
}

# a . p <= b (or =) written without minus signs: positive terms left, negative terms right,
# a constant on the side where it is positive ("p2 <= p1", "1/10 <= p3", "p1 + p2 <= 1 + p3")
tbl_sides <- function(a, b, nm, eq, times = " * ", num = tbl_num) {
  if (eq) {
    fi <- which(a != 0)[1]
    if (!is.na(fi) && a[fi] < 0) { a <- -a; b <- -b }
  }
  side <- function(coef, const) {
    terms <- vapply(which(coef != 0), function(j)
      if (abs(coef[j] - 1) < 1e-12) nm[j] else paste0(num(coef[j]), times, nm[j]), character(1))
    if (abs(const) > 1e-12) terms <- c(terms, num(const))
    if (length(terms) == 0) "0" else paste(terms, collapse = " + ")
  }
  list(lhs = side(pmax(a, 0), max(-b, 0)), rhs = side(pmax(-a, 0), max(b, 0)))
}

tbl_readable <- function(a, b, nm, eq, times = " * ") {
  p <- tbl_sides(a, b, nm, eq, times)
  paste(p$lhs, if (eq) "=" else "<=", p$rhs)
}

#' The H-description of a model as a table
#'
#' One row per constraint. `style = "text"` (default) writes each constraint
#' without minus signs, so that it reads like a sentence (`p2 <= p1`,
#' `1/10 <= p3`, `p1 + p2 <= 1 + p3`); `style = "paper"` gives the form used in
#' the paper's Figure 2 (`- p1 + p2 <= 0`, as in [fairy_constraints()]);
#' `style = "matrix"` gives the coefficients: the constraint `a1 p1 + ... + an pn <= b` (or `=`)
#' is one row with the columns `p1`, ..., `pn`, `relation` and `b`.
#'
#' @details
#' **How to read it.** Each row is one constraint of the model, and the model
#' is the set of all combinations of the probabilities that satisfy *all* rows
#' at once. `p2 <= p1` says that `p2` is at most `p1`; a row with `=`
#' (`type` equality) fixes a relation exactly. Redundant rows are left out; the
#' bounds 0 <= p <= 1 appear only where they restrict the model
#' (for example `p1 <= 1`).
#' This is the H-description (half-space description) of the model's polytope.
#'
#' @param h An H-representation from [fairy_h()] (or a model string).
#' @param n_p Number of probabilities for model strings.
#' @param labels Optional names for the probabilities.
#' @param times Symbol between a coefficient and its probability (`"text"` style).
#' @param style `"text"` (default), `"paper"` or `"matrix"`.
#'
#' @return A data frame of class `"fairy_table"`. Use [fairy_table_write()] to
#'   get it as Markdown, LaTeX or CSV.
#' @examples
#' h <- fairy_h("p1 > p2 > p3", n_p = 3)
#' fairy_h_table(h)
#' fairy_h_table(h, style = "matrix")
#' @export
fairy_h_table <- function(h, n_p = NULL, labels = NULL, style = c("text", "paper", "matrix"), times = " * ") {
  style <- match.arg(style)
  h <- resolve_h(h, n_p = n_p)
  hn <- q2d(h)
  k <- ncol(h) - 2L
  nm <- if (is.null(labels)) paste0("p", seq_len(k)) else labels
  keep <- !(hn[, 1] == 0 & apply(hn[, -(1:2), drop = FALSE], 1, function(r) all(r == 0)))
  hn <- hn[keep, , drop = FALSE]
  if (style == "text") {
    A <- -hn[, -(1:2), drop = FALSE]
    txt <- vapply(seq_len(nrow(hn)), function(i) tbl_readable(A[i, ], hn[i, 2], nm, hn[i, 1] == 1, times), character(1))
    df <- data.frame(Constraint = txt, Type = ifelse(hn[, 1] == 1, "equality", "inequality"), stringsAsFactors = FALSE)
  } else if (style == "paper") {
    df <- data.frame(Constraint = fairy_constraints(h, labels = labels)[keep],
                     Type = ifelse(hn[, 1] == 1, "equality", "inequality"), stringsAsFactors = FALSE)
  } else {
    A <- -hn[, -(1:2), drop = FALSE]
    df <- as.data.frame(lapply(seq_len(k), function(j) tbl_num(A[, j])), stringsAsFactors = FALSE)
    colnames(df) <- nm
    df$relation <- ifelse(hn[, 1] == 1, "=", "<=")
    df$b <- tbl_num(hn[, 2])
  }
  rownames(df) <- paste0("C", seq_len(nrow(df)))
  class(df) <- c("fairy_table", "data.frame")
  df
}

#' The V-description (extreme cases) of a model as a table
#'
#' One row per vertex (`V1`, `V2`, ...), one column per probability. Values
#' are shown as fractions. With `words = TRUE`, 0 and 1 are replaced by
#' `"none"` and `"all"` (see [fairy_words()]).
#'
#' @details
#' **How to read it.** Each row is an extreme case (vertex) of the model: a
#' combination of values of the probabilities that the model allows and that
#' cannot be written as a mixture of two other allowed combinations. Every
#' allowed combination is a weighted mixture of the rows. The more vertices, the
#' more different extreme patterns the model permits. This is the V-description
#' (vertex description) of the model's polytope.
#'
#' @param h A model (string with `n_p`, or H-representation).
#' @param n_p Number of probabilities for model strings.
#' @param labels Optional column labels.
#' @param words Use the words for 0 and 1? Default `FALSE`.
#' @param sort Order the rows lexicographically? Default `TRUE`.
#'
#' @return A data frame of class `"fairy_table"`.
#' @examples
#' fairy_v_table("p1 > p2 > p3", n_p = 3)
#' fairy_v_table("p1 > p2 > p3", n_p = 3, words = TRUE)
#' @export
fairy_v_table <- function(h, n_p = NULL, labels = NULL, words = FALSE, sort = TRUE) {
  if (words) {
    df <- fairy_words(h, labels = labels, n_p = n_p, sort = sort)
    class(df) <- c("fairy_table", "data.frame")
    return(df)
  }
  v <- fairy_vertices(h, n_p = n_p)
  if (sort && nrow(v) > 0) v <- v[do.call(order, as.data.frame(unname(v))), , drop = FALSE]
  df <- as.data.frame(lapply(seq_len(ncol(v)), function(j) tbl_num(v[, j])), stringsAsFactors = FALSE)
  colnames(df) <- if (!is.null(labels)) labels else paste0("p", seq_len(ncol(v)))
  rownames(df) <- paste0("V", seq_len(nrow(df)))
  class(df) <- c("fairy_table", "data.frame")
  df
}

#' @export
print.fairy_table <- function(x, ...) {
  print.data.frame(x, right = TRUE, ...)
  invisible(x)
}

#' Write a model table as Markdown, LaTeX or CSV
#'
#' @param x A table from [fairy_h_table()] or [fairy_v_table()].
#' @param file Optional file name. If `NULL` (default) the text is only returned.
#' @param format `"markdown"` (pipe table), `"latex"` (a `tabular`) or `"csv"`.
#' @param row_names Include the row names (`C1`, `V1`, ...)? Default `TRUE`.
#'
#' @return The table as a character vector (one element per line), invisibly
#'   if written to a file.
#' @examples
#' fairy_table_write(fairy_v_table("p1 > p2", n_p = 2))
#' fairy_table_write(fairy_h_table("p1 > p2", n_p = 2), format = "latex")
#' @export
fairy_table_write <- function(x, file = NULL, format = c("markdown", "latex", "csv"), row_names = TRUE) {
  format <- match.arg(format)
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  rn <- rownames(x)
  if (row_names) { df <- cbind(` ` = rn, df, stringsAsFactors = FALSE) }
  cn <- colnames(df); m <- as.matrix(df)
  out <- switch(format,
    markdown = {
      w <- pmax(nchar(cn), apply(m, 2, function(c) max(nchar(c), 0)))
      pad <- function(s, k) sprintf("%*s", as.integer(k), s)
      c(paste0("| ", paste(pad(cn, w), collapse = " | "), " |"),
        paste0("| ", paste(vapply(w, function(k) paste(rep("-", k), collapse = ""), character(1)), collapse = " | "), " |"),
        apply(m, 1, function(r) paste0("| ", paste(pad(r, w), collapse = " | "), " |")))
    },
    latex = {
      esc <- function(s) gsub(">=", "$\\\\ge$", gsub("<=", "$\\\\le$", gsub("([&%$#_{}])", "\\\\\\1", s)))
      cellf <- function(s) { s <- esc(s); s }
      c(paste0("\\begin{tabular}{", paste(rep("r", ncol(m)), collapse = ""), "}"), "\\hline",
        paste0(paste(vapply(cn, cellf, character(1)), collapse = " & "), " \\\\"), "\\hline",
        apply(m, 1, function(r) paste0(paste(vapply(r, cellf, character(1)), collapse = " & "), " \\\\")),
        "\\hline", "\\end{tabular}")
    },
    csv = c(paste(cn, collapse = ","), apply(m, 1, function(r) paste(r, collapse = ",")))
  )
  if (!is.null(file)) { writeLines(out, file); return(invisible(out)) }
  out
}


# a number as LaTeX: fractions as \frac{a}{b}
tex_num <- function(x) {
  t <- tbl_num(x)
  ifelse(grepl("/", t), sub("^(-?)([0-9]+)/([0-9]+)$", "\\1\\\\frac{\\2}{\\3}", t), t)
}

# probability names as LaTeX; "p_{1}^(2)" (item number in parentheses) becomes "p_{1}^{(2)}"
tex_label <- function(nm) gsub("\\^\\(([^)]*)\\)", "^{(\\1)}", nm)

#' The H-description of a model as LaTeX
#'
#' Writes each constraint as one line of an `align*` block, in the readable
#' form of [fairy_h_table()] (no minus signs: `p_{2} &\\leq p_{1}`). The
#' trivial bounds 0 <= p <= 1 (such as `p_{1} \\leq 1`) can be shown, grayed out
#' or hidden, as in the Fairy app.
#'
#' @param h An H-representation from [fairy_h()] (or a model string).
#' @param n_p Number of probabilities for model strings.
#' @param labels Optional names for the probabilities (LaTeX, e.g. `"p_{1}"`).
#'   Default: the column names of `h`.
#' @param trivial `"show"` (default), `"gray"` or `"hide"` the trivial bounds.
#'
#' @return A character vector, one `align*` line per constraint.
#' @examples
#' fairy_h_latex("p1 > p2 > p3", n_p = 3)
#' fairy_h_latex("p1 > p2 > p3", n_p = 3, trivial = "hide")
#' @export
fairy_h_latex <- function(h, n_p = NULL, labels = NULL, trivial = c("show", "gray", "hide")) {
  trivial <- match.arg(trivial)
  h <- resolve_h(h, n_p = n_p)
  hn <- q2d(h)
  k <- ncol(h) - 2L
  nm <- if (!is.null(labels)) labels else {
    cn <- colnames(h)[-(1:2)]
    if (is.null(cn) || any(!nzchar(cn))) paste0("p_{", seq_len(k), "}") else tex_label(cn)
  }
  keep <- !(hn[, 1] == 0 & apply(hn[, -(1:2), drop = FALSE], 1, function(r) all(r == 0)))
  hn <- hn[keep, , drop = FALSE]
  A <- -hn[, -(1:2), drop = FALSE]
  b <- hn[, 2]
  eq <- hn[, 1] == 1
  is_trivial <- vapply(seq_len(nrow(hn)), function(i) {
    nz <- which(A[i, ] != 0)
    !eq[i] && length(nz) == 1 && abs(abs(A[i, nz]) - 1) < 1e-12 && ((A[i, nz] > 0 && abs(b[i] - 1) < 1e-12) || (A[i, nz] < 0 && abs(b[i]) < 1e-12))
  }, logical(1))
  rows <- vapply(seq_len(nrow(hn)), function(i) {
    p <- tbl_sides(A[i, ], b[i], nm, eq[i], times = "", num = tex_num)
    op <- if (eq[i]) "=" else "\\leq"
    if (trivial == "gray" && is_trivial[i]) {
      paste0("\\textcolor{gray}{", p$lhs, "} &\\textcolor{gray}{", op, " ", p$rhs, "} \\\\")
    } else paste0(p$lhs, " &", op, " ", p$rhs, " \\\\")
  }, character(1))
  if (trivial == "hide") rows <- rows[!is_trivial]
  rows
}

#' Write the H-descriptions of several models as a LaTeX document
#'
#' One `align*` block per model (see [fairy_h_latex()]), in a small
#' `article` document that compiles as it is.
#'
#' @param models A (named) list or character vector. Each element is a model
#'   string or an H-representation.
#' @param n_p Number of probabilities; only needed for model strings.
#' @param file Optional file name (`.tex`). If `NULL` the lines are returned.
#' @param trivial `"show"` (default), `"gray"` or `"hide"` the trivial bounds.
#'
#' @return The LaTeX lines, invisibly if written to a file.
#' @examples
#' fairy_write_latex(list(A = "p1 > p2", B = "p1 > p2 > p3"), n_p = 3)
#' @export
fairy_write_latex <- function(models, n_p = NULL, file = NULL, trivial = c("show", "gray", "hide")) {
  trivial <- match.arg(trivial)
  if (is.null(names(models))) names(models) <- paste0("M", seq_along(models))
  esc <- function(x) gsub("([&%$#_{}])", "\\\\\\1", x)
  out <- c("\\documentclass{article}", "\\usepackage{amsmath}", "\\usepackage{xcolor}", "\\begin{document}", "\\section*{Models}")
  for (nm in names(models)) {
    m <- models[[nm]]
    h <- if (is.character(m) && !is.matrix(m)) {
      if (is.null(n_p)) stop("'n_p' is needed for model strings.")
      fairy_h(m, n_p = n_p)
    } else as_h(m)
    rows <- fairy_h_latex(h, trivial = trivial)
    out <- c(out, paste0("\\subsection*{", esc(nm), "}"),
             if (length(rows) > 0) c("\\begin{align*}", rows, "\\end{align*}") else "(no constraints)", "")
  }
  out <- c(out, "\\end{document}")
  if (!is.null(file)) { writeLines(out, file); return(invisible(out)) }
  out
}
