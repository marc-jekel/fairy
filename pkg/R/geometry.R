# Geometry helpers: vertices, dimension, volume of an H-represented polytope
# inside the unit hypercube [0, 1]^n.

# Coerce an H-representation (rcdd character or numeric matrix) to rcdd's
# rational character format.
as_h <- function(h) {
  if (!is.matrix(h)) stop("A model must be a model string or a matrix as returned by fairy_h() or fairy_v().")
  # a V-representation (as returned by fairy_v()) is converted to the H-representation first
  if (identical(attr(h, "representation"), "V")) {
    cn <- colnames(h)
    out <- redundant(scdd(h)$output)$output
    colnames(out) <- if (!is.null(cn)) cn else c("", "", paste0("p", seq_len(ncol(out) - 2L)))
    return(out)
  }
  if (is.numeric(h)) return(d2q(h))
  h
}

#' H-representation from the extreme cases of a model
#'
#' If a model is given by its V-representation, that is its vertices (extreme
#' cases), this converts it to the H-representation. All other functions of
#' the package (`fairy_volume()`, `fairy_info()`, `fairy_nested()`, ...)
#' accept either representation directly, so this function is only needed to
#' look at the constraints, e.g. with [fairy_constraints()].
#'
#' @param vertices A matrix with one vertex per row and one column per
#'   probability. Numbers or exact fractions as strings (`"1/3"`) are
#'   accepted. A V-representation in rcdd format (from [fairy_v()]) also works.
#' @param labels Optional column names for the probabilities.
#'
#' @return An H-representation (see [fairy_h()]).
#' @examples
#' v <- rbind(c(0, 0), c(1, 0), c(1, 1))
#' h <- fairy_from_vertices(v)
#' fairy_constraints(h)
#' fairy_volume(h)$volume   # 1/2
#' @importFrom rcdd makeV scdd redundant
#' @export
fairy_from_vertices <- function(vertices, labels = NULL) {
  if (identical(attr(vertices, "representation"), "V")) return(as_h(vertices))
  v <- as.matrix(vertices)
  dimnames(v) <- NULL
  if (is.numeric(v)) v <- d2q(v)
  h <- redundant(scdd(makeV(points = v))$output)$output
  colnames(h) <- c("", "", if (is.null(labels)) paste0("p_{", seq_len(ncol(v)), "}") else labels)
  h
}

# Resolve either a model string or an H-representation to an H-representation.
resolve_h <- function(x, n_p = NULL, approx_tol = 0, ...) {
  if (is.character(x) && !is.matrix(x)) {
    if (length(x) != 1L) stop("Pass a single model string (or an H-representation).")
    return(fairy_h(x, n_p = n_p, approx_tol = approx_tol, ...))
  }
  as_h(x)
}

#' Vertices of an order-constrained model
#'
#' Returns the extreme cases (the V-representation) of a model as a plain
#' numeric matrix, one vertex per row.
#'
#' @param h An H-representation as returned by [fairy_h()], or a single model
#'   string (then `n_p` is needed or inferred).
#' @param n_p Number of probabilities, only used when `h` is a model string.
#'
#' @return A numeric matrix with one column per probability. Rows are
#'   the vertices of the polytope.
#'
#' @examples
#' fairy_vertices("p1 > p2 > p3")
#' @export
fairy_vertices <- function(h, n_p = NULL) {
  h <- resolve_h(h, n_p = n_p)
  v <- q2d(scdd(h)$output)
  if (length(v) == 0) return(matrix(numeric(0), 0, ncol(h) - 2L))
  v <- v[v[, 1] == 0 & v[, 2] == 1, -(1:2), drop = FALSE]
  colnames(v) <- colnames(h)[-(1:2)]
  v
}

#' Dimension of a model
#'
#' The dimension of the affine hull of the model's polytope. A model that
#' contains an equality constraint has a lower dimension than the number of
#' probabilities and therefore volume zero.
#'
#' @inheritParams fairy_vertices
#' @return An integer; `-1` for an empty model.
#' @examples
#' fairy_dim("p1 > p2 > p3")        # 3
#' fairy_dim("p1 = p2; p2 > p3")    # 2
#' @export
fairy_dim <- function(h, n_p = NULL) {
  v <- fairy_vertices(h, n_p)
  if (nrow(v) == 0) return(-1L)
  if (nrow(v) == 1) return(0L)
  qr(sweep(v, 2, v[1, ]), tol = 1e-9)$rank
}

# Exact volume of the polytope with the given vertices (convex hull, qhull).
volume_from_vertices <- function(v) {
  n <- ncol(v)
  if (nrow(v) <= n) return(0)
  if (qr(sweep(v, 2, v[1, ]), tol = 1e-9)$rank < n) return(0)
  if (n == 1L) return(diff(range(v)))
  geometry::convhulln(v, options = "FA")$vol
}

#' Volume (parsimony) of an order-constrained model
#'
#' The volume of the model polytope inside the unit hypercube of all
#' probability vectors. A small volume means a restrictive, parsimonious, easily falsified
#' model. Its inverse is the largest Bayes factor the model can reach against
#' the unconstrained model (uniform prior, unbounded sample size); see
#' [fairy_max_bf()].
#'
#' @param h An H-representation from [fairy_h()], or a single model string.
#' @param n_p Number of probabilities (only if `h` is a string).
#' @param method `"volesti"` (default): random-walk volume estimation with
#'   the package \pkg{volesti} and its own default settings; further arguments
#'   in `...` are passed on to `volesti::volume()` (e.g. `settings = list(error = 0.01)`),
#'   so any non-default volesti option is up to the user. `"exact"`: convex hull
#'   of the vertices, exact up to floating-point error, but can be very slow
#'   or run out of memory in high dimensions. `"mc"`: uniform Monte Carlo
#'   draws from the hypercube. The method is never switched automatically.
#' @param ... Passed on to `volesti::volume()` when `method = "volesti"`.
#' @param n_samples Monte Carlo draws for `method = "mc"`.
#'
#' @return A list with `volume`, `se` (`NA` unless Monte Carlo), `method`,
#'   `dim` (dimension of the model) and `max_bf` (`Inf` when the volume is zero).
#'
#' @examples
#' fairy_volume("p1 > p2 > p3")$volume          # 1/6
#' fairy_volume("p1 = p2; p2 > p3")$max_bf      # Inf, a lower-dimensional model
#' @importFrom stats runif
#' @export
fairy_volume <- function(h, n_p = NULL, method = c("volesti", "exact", "mc"),
                         n_samples = 1e6, ...) {
  method <- match.arg(method)
  h <- resolve_h(h, n_p = n_p)
  jt <- attr(h, "fairy_joint")
  if (!is.null(jt)) {
    v1 <- fairy_volume(jt$h, method = method, n_samples = n_samples, ...)
    return(list(volume = v1$volume^jt$r, se = NA_real_, method = paste0("product of ", v1$method),
                dim = if (v1$dim < 0) -1L else v1$dim * jt$r,
                max_bf = if (v1$volume > 0) v1$volume^(-jt$r) else Inf))
  }
  n <- ncol(h) - 2L
  v <- fairy_vertices(h)
  dm <- if (nrow(v) == 0) -1L else if (nrow(v) == 1) 0L else qr(sweep(v, 2, v[1, ]), tol = 1e-9)$rank
  out <- function(vol, se, how) list(volume = vol, se = se, method = how, dim = dm,
                                     max_bf = if (vol > 0) 1 / vol else Inf)
  if (dm < n) return(out(0, NA_real_, "lower-dimensional"))

  if (method == "exact") {
    return(out(volume_from_vertices(v), NA_real_, "exact"))
  }
  if (method == "volesti") {
    if (!requireNamespace("volesti", quietly = TRUE)) stop("Package 'volesti' is not installed.")
    hn <- q2d(h)
    A <- -hn[, -(1:2), drop = FALSE]; b <- hn[, 2]
    P <- volesti::Hpolytope(A = A, b = b)
    vol <- volesti::volume(P, ...)
    return(out(as.numeric(vol), NA_real_, "volesti"))
  }
  # Monte Carlo in the unit hypercube
  hn <- q2d(h); b <- hn[, 2]; A <- hn[, -(1:2), drop = FALSE]
  chunk <- 2e5; hits <- 0; done <- 0
  while (done < n_samples) {
    m <- min(chunk, n_samples - done)
    x <- matrix(runif(m * n), m, n)
    ok <- rep(TRUE, m)
    s <- sweep(x %*% t(A), 2, b, "+")      # b + A x >= 0
    ok <- rowSums(s < -1e-12) == 0
    hits <- hits + sum(ok); done <- done + m
  }
  p <- hits / n_samples
  out(p, sqrt(p * (1 - p) / n_samples), "mc")
}

#' Maximal Bayes factor of a model against the unconstrained model
#'
#' The inverse of the model's volume: the largest Bayes factor in favour of the
#' model against the unconstrained model that any data set of any size can
#' produce, under a uniform prior. It is `Inf` for models with an equality
#' constraint (volume zero).
#'
#' @inheritParams fairy_volume
#' @return A single number.
#' @examples
#' fairy_max_bf("p1 > p2 > p3 > p4")   # 24
#' @export
fairy_max_bf <- function(h, n_p = NULL, method = "volesti", n_samples = 1e6, ...) {
  fairy_volume(h, n_p = n_p, method = method, n_samples = n_samples, ...)$max_bf
}

#' Summary numbers of a model
#'
#' Dimension, number of vertices, of non-redundant inequalities and equalities
#' in the H-representation, volume and maximal Bayes factor. These are the
#' quantities reported in the model tables of the paper.
#'
#' @inheritParams fairy_volume
#' @param volume Compute the volume as well? Skip it (`FALSE`) when the
#'   polytope is large.
#' @return A one-row data frame.
#' @examples
#' fairy_info("p1 > p2 > p3")
#' @export
fairy_info <- function(h, n_p = NULL, method = "volesti", volume = TRUE, ...) {
  h <- resolve_h(h, n_p = n_p)
  hn <- q2d(h)
  v <- fairy_vertices(h)
  vol <- if (volume) fairy_volume(h, method = method, ...) else list(volume = NA_real_, max_bf = NA_real_, dim = NA_integer_)
  data.frame(
    dim = if (volume) vol$dim else fairy_dim(h),
    vertices = nrow(v),
    inequalities = sum(hn[, 1] == 0),
    equalities = sum(hn[, 1] == 1),
    volume = vol$volume,
    max_bf = vol$max_bf,
    row.names = NULL
  )
}
