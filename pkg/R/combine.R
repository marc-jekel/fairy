# Combining models: intersection, mixture, nesting, and comparison tables.

#' Intersection of models
#'
#' The models' constraints hold simultaneously. Each argument is a model
#' string (with `n_p`) or an H-representation.
#'
#' @param ... Two or more model strings or H-representations over the same
#'   probabilities.
#' @param n_p Number of probabilities, needed for model strings (inferred
#'   from the highest `p` index otherwise).
#' @param reduce Remove redundant constraints? Default `TRUE`.
#'
#' @return An H-representation (character matrix, see [fairy_h()]).
#'   An empty intersection returns a matrix with an infeasible set of
#'   constraints; check with [fairy_dim()], which returns `-1`.
#'
#' @examples
#' fairy_intersect("p1 > p2", "p2 > p3")
#' @importFrom rcdd redundant
#' @export
fairy_intersect <- function(..., n_p = NULL, reduce = TRUE) {
  ms <- list(...)
  if (length(ms) < 2L) stop("Give at least two models.")
  if (is.null(n_p)) n_p <- infer_n_p(ms)
  hs <- align_h(lapply(ms, resolve_h, n_p = n_p))
  cn <- colnames(hs[[1]])
  h <- do.call(rbind, hs)
  colnames(h) <- cn
  if (reduce && nrow(h) > 1) {
    r <- tryCatch(redundant(h)$output, error = function(e) h)
    colnames(r) <- cn
    h <- r
  }
  h
}

#' Mixture of models
#'
#' The mixture model says that each person follows one of the component
#' models, with unknown weights. Geometrically it is the convex hull of the
#' component models, so its V-representation is the union of the
#' components' vertices.
#'
#' @inheritParams fairy_intersect
#' @param ... Two or more model strings or H-representations over the same
#'   probabilities.
#'
#' @return An H-representation of the convex hull.
#'
#' @examples
#' m4 <- fairy_h("p1>p2>p3; p4<p5<p6; p1<p4; p2<p5; p3<p6", n_p = 6)
#' m9 <- fairy_h("p1<p2<p3; p4>p5>p6; p1>p4; p2>p5; p3>p6", n_p = 6)
#' nrow(fairy_mixture(m4, m9))   # 64 constraints
#' @importFrom rcdd scdd d2q makeV
#' @export
fairy_mixture <- function(..., n_p = NULL) {
  ms <- list(...)
  if (length(ms) < 2L) stop("Give at least two models.")
  if (is.null(n_p)) n_p <- infer_n_p(ms)
  hs <- align_h(lapply(ms, resolve_h, n_p = n_p))
  cn <- colnames(hs[[1]])
  # exact rational vertices, so that decimals such as .8 stay 4/5
  vq <- function(h) { o <- scdd(h)$output; o[o[, 1] == "0" & o[, 2] == "1", -(1:2), drop = FALSE] }
  v <- unique(do.call(rbind, lapply(hs, vq)))
  dimnames(v) <- NULL
  h <- scdd(makeV(points = v))$output
  h <- redundant(h)$output
  colnames(h) <- cn
  h
}

# Models can only be combined when they live in the same space: the same
# probabilities, in any order. Models of the same unpacking with differently
# grouped items have the same columns in a different order, so the columns
# are matched by name.
align_h <- function(hs) {
  if (length(hs) < 2L) return(hs)
  nc <- vapply(hs, ncol, integer(1))
  if (length(unique(nc)) > 1L)
    stop("The models do not live in the same space: they have ", paste(nc - 2L, collapse = " and "),
         " probabilities. Unpacked models can only be combined if they have the same number of items for every probability.")
  ref <- colnames(hs[[1]])
  lapply(hs, function(h) {
    cn <- colnames(h)
    if (is.null(cn) || is.null(ref) || identical(cn, ref)) return(h)
    a <- cn[-(1:2)]; b <- ref[-(1:2)]
    if (any(!nzchar(a)) || any(!nzchar(b))) return(h)
    if (anyDuplicated(a) || !setequal(a, b))
      stop("The models do not have the same probabilities (columns), so they cannot be combined.")
    h2 <- h[, c(1:2, 2L + match(b, a)), drop = FALSE]
    colnames(h2) <- ref
    attr(h2, "representation") <- "H"
    h2
  })
}

infer_n_p <- function(ms) {
  mats <- Filter(is.matrix, ms)
  if (length(mats)) return(ncol(mats[[1]]) - 2L)
  idx <- as.integer(unlist(regmatches(unlist(ms), gregexpr("(?<=p)[0-9]+", unlist(ms), perl = TRUE))))
  if (length(idx) == 0) stop("Cannot infer n_p.")
  max(idx)
}

#' Is one model nested in another?
#'
#' Model A is nested in model B when every extreme case of A satisfies the
#' constraints of B.
#'
#' @param a,b Models (strings or H-representations).
#' @param n_p Number of probabilities for model strings.
#' @param tol Numerical tolerance.
#' @return `TRUE` or `FALSE`.
#' @examples
#' fairy_nested("p1 > p2 > p3", "p1 > p2", n_p = 3)   # TRUE
#' @export
fairy_nested <- function(a, b, n_p = NULL, tol = 1e-9) {
  if (is.null(n_p)) n_p <- infer_n_p(list(a, b))
  ha <- resolve_h(a, n_p = n_p); hb <- resolve_h(b, n_p = n_p)
  va <- fairy_vertices(ha)
  if (nrow(va) == 0) return(TRUE)
  hbn <- q2d(hb)
  s <- sweep(va %*% t(hbn[, -(1:2), drop = FALSE]), 2, hbn[, 2], "+")   # b + a x
  eq <- hbn[, 1] == 1
  ok <- (if (any(!eq)) all(s[, !eq, drop = FALSE] >= -tol) else TRUE) &&
        (if (any(eq)) all(abs(s[, eq, drop = FALSE]) <= tol) else TRUE)
  ok
}

#' Compare a set of models
#'
#' Reproduces the layout of the paper's model-relationship table: on the
#' diagonal each model's volume, above the diagonal the volume of the
#' overlap of each pair, below the diagonal whether the row model is nested
#' in the column model (`"subset"`, printed as the subset sign) or contains it
#' (`"superset"`), and otherwise the dimension of their overlap (`"3D"`);
#' `"empty"` marks an empty overlap. The last row gives
#' each model's maximal Bayes factor against the unconstrained model.
#'
#' @param models A named list of models (strings or H-representations).
#' @param n_p Number of probabilities for model strings.
#' @param method Volume method, see [fairy_volume()].
#' @param ... Passed on to [fairy_volume()].
#'
#' @return An object of class `"fairy_compare"` with elements `volume`
#'   (numeric matrix: diagonal and upper triangle), `relation` (character
#'   matrix: lower triangle), `dim` and `max_bf` (vectors).
#'
#' @examples
#' fairy_compare(list(A = "p1 > p2 > p3", B = "p1 > p2", C = "p3 > p1"), n_p = 3)
#' @export
fairy_compare <- function(models, n_p = NULL, method = "volesti", ...) {
  if (is.null(names(models))) names(models) <- seq_along(models)
  if (is.null(n_p)) n_p <- infer_n_p(models)
  hs <- lapply(models, resolve_h, n_p = n_p)
  k <- length(hs); nm <- names(models)
  vol <- matrix(NA_real_, k, k, dimnames = list(nm, nm))
  rel <- matrix("", k, k, dimnames = list(nm, nm))
  dims <- integer(k); names(dims) <- nm
  mbf <- numeric(k); names(mbf) <- nm
  for (i in seq_len(k)) {
    vi <- fairy_volume(hs[[i]], method = method, ...)
    vol[i, i] <- vi$volume; dims[i] <- vi$dim; mbf[i] <- vi$max_bf
  }
  for (i in seq_len(k)) for (j in seq_len(k)) {
    if (i == j) next
    hij <- fairy_intersect(hs[[i]], hs[[j]], reduce = FALSE)
    dij <- fairy_dim(hij)
    if (i < j) {
      vol[i, j] <- if (dij < 0) 0 else if (dij < n_p) 0 else fairy_volume(hij, method = method, ...)$volume
    } else {
      rel[i, j] <-
        if (fairy_nested(hs[[i]], hs[[j]], n_p = n_p)) "subset"
        else if (fairy_nested(hs[[j]], hs[[i]], n_p = n_p)) "superset"
        else if (dij < 0) "empty"
        else paste0(dij, "D")
    }
  }
  structure(list(volume = vol, relation = rel, dim = dims, max_bf = mbf), class = "fairy_compare")
}

#' @export
print.fairy_compare <- function(x, digits = 2, ...) {
  k <- nrow(x$volume)
  out <- matrix("", k + 1L, k, dimnames = list(c(rownames(x$volume), "maxBF"), colnames(x$volume)))
  fmt <- function(z) {
    if (is.na(z)) return("")
    if (z == 0) return("0")
    if (z < 0.001) return(formatC(z, digits = digits, format = "e"))
    formatC(z, digits = digits, format = "fg")
  }
  for (i in seq_len(k)) for (j in seq_len(k)) {
    out[i, j] <- if (i <= j) fmt(x$volume[i, j]) else x$relation[i, j]
  }
  if (isTRUE(l10n_info()[["UTF-8"]])) {
    out[out == "subset"] <- "\u2282"; out[out == "superset"] <- "\u2283"
  }
  out[k + 1L, ] <- ifelse(is.infinite(x$max_bf), "Inf", formatC(x$max_bf, digits = 3, format = "fg"))
  print(noquote(out), ...)
  invisible(x)
}
