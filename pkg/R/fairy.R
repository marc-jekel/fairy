#' Compute H-representation for a single order-constrained model
#'
#' Parses a model specification expressed as linear (in)equalities over
#' proportions p1, ..., p_k and returns the non-redundant
#' H-representation (half-space description) of the corresponding
#' convex polytope.
#'
#' @param model A single character string specifying the model. Use `;`
#'   or newlines to separate multiple constraints within one model, e.g.
#'   `"p1 > p2; p2 > p3"`. Chain notation (`p1 > p2 > p3`), batch
#'   notation (`{p1,p2} > {p3,p4}`), and free-form linear expressions
#'   (`2*p1 - p2 > 0.5`) are all accepted.
#' @param n_p Integer. Number of probability parameters (dimensions).
#'   If omitted, inferred from the highest `p` index mentioned in the model.
#' @param p_names Optional character vector of length `n_p` with
#'   human-readable labels for the probabilities (used as column names).
#' @param approx_tol Numeric. If > 0, equality constraints are relaxed
#'   to `|a*p - b| <= approx_tol` (two inequalities). Default `0`.
#' @param reduce Logical. Remove redundant rows via [rcdd::redundant()]?
#'   Default `TRUE`.
#'
#' @return A character matrix in rcdd H-representation format with
#'   columns `["", "", p1, p2, ...]`.  Column 1 is `"0"` (inequality)
#'   or `"1"` (equality); column 2 is the RHS `b`; columns 3+ are the
#'   coefficients. Convert to numeric with [rcdd::q2d()].
#'
#' @seealso [fairy_v()], [fairy_parsimony()], [fairy()]
#'
#' @examples
#' h <- fairy_h("p1 > p2 > p3", n_p = 3)
#' print(rcdd::q2d(h))
#'
#' @importFrom rcdd makeH redundant
#' @export
fairy_h <- function(model, n_p = NULL, p_names = NULL,
                    approx_tol = 0, reduce = TRUE) {
  if (length(model) != 1L) stop("fairy_h() takes a single model string. Use fairy() for multiple models.")

  if (is.null(n_p)) {
    idx <- as.integer(unlist(regmatches(
      model,
      gregexpr("(?<=p)[0-9]+", model, perl = TRUE)
    )))
    if (length(idx) == 0) stop("Cannot infer n_p: no p-indices found in model.")
    n_p <- max(idx)
  }

  parsed <- extract_info(model, n_p = n_p, approx_tol = approx_tol)

  iel   <- parsed$ineq_eq_left
  ier   <- parsed$ineq_eq_right
  ops   <- parsed$all_operators
  n_p   <- ncol(iel)   # may have been expanded inside extract_info

  has_ineq <- any(ops != "equal")
  has_eq   <- any(ops == "equal")

  if (has_ineq && has_eq) {
    h <- makeH(
      a1 = iel[ops != "equal", , drop = FALSE],
      b1 = ier[ops != "equal"],
      a2 = iel[ops == "equal", , drop = FALSE],
      b2 = ier[ops == "equal"]
    )
  } else if (has_ineq) {
    h <- makeH(a1 = iel, b1 = ier)
  } else {
    h <- makeH(a2 = iel, b2 = ier)
  }

  if (reduce && nrow(h) > 1) h <- redundant(h)$output

  if (!is.null(p_names)) {
    if (length(p_names) != n_p) {
      warning("Length of p_names (", length(p_names), ") != n_p (", n_p, "); ignoring.")
    } else {
      colnames(h) <- c("", "", p_names)
    }
  } else {
    colnames(h) <- c("", "", paste0("p_{", seq_len(n_p), "}"))
  }

  h
}


#' Compute V-representation (vertices) of an order-constrained model
#'
#' Converts an H-representation to its V-representation using [rcdd::scdd()].
#'
#' @param h H-representation matrix as returned by [fairy_h()].
#'   Numeric matrices are converted to rational automatically.
#'
#' @return A character matrix in rcdd V-representation format.
#'   Use [rcdd::q2d()] to convert to numeric.
#'
#' @examples
#' h <- fairy_h("p1 > p2 > p3", n_p = 3)
#' v <- fairy_v(h)
#' print(rcdd::q2d(v))
#'
#' @importFrom rcdd scdd d2q
#' @export
fairy_v <- function(h) {
  if (is.numeric(h)) h <- d2q(h)
  scdd(h)$output
}


#' Compute parsimony of an order-constrained model
#'
#' Parsimony is the volume of the model polytope inside the unit hypercube
#' of all probability vectors \eqn{[0, 1]^n}: the share of all conceivable
#' families of Binomial parameters that the model permits. The smaller the
#' volume, the more restrictive, the more parsimonious and the more easily falsified the model.
#' Its inverse is the largest Bayes factor the model can reach against the
#' unconstrained model. This is a thin wrapper around [fairy_volume()], kept
#' for compatibility; the volume is exact where feasible and otherwise
#' estimated.
#'
#' @param h H-representation as returned by [fairy_h()].
#' @param n_p Integer. Number of parameters. Inferred from `ncol(h) - 2`
#'   when not supplied.
#' @param n_samples Integer. Monte Carlo draws, used only when the volume
#'   cannot be computed exactly and volesti is not installed.
#' @param method See [fairy_volume()].
#'
#' @return A named list with `parsimony` (the volume), `se` (`NA` when exact),
#'   `n_samples`, `method` and `max_bf`.
#'
#' @examples
#' h <- fairy_h("p1 > p2 > p3", n_p = 3)
#' fairy_parsimony(h, n_p = 3)
#'
#' @seealso [fairy_volume()], [fairy_max_bf()]
#' @export
fairy_parsimony <- function(h, n_p = NULL, n_samples = 1e5, method = "auto") {
  vol <- fairy_volume(h, n_p = NULL, method = method, n_samples = n_samples)
  list(parsimony = vol$volume, se = vol$se, n_samples = n_samples,
       method = vol$method, max_bf = vol$max_bf)
}


#' Formalize one or more order-constrained models
#'
#' The main entry point for the fairy package. Pass one or more verbal
#' hypotheses as a character vector - one model per element - and get
#' back their H-representations, V-representations, and parsimony values.
#'
#' Within each model string, separate multiple constraints with `;` or
#' newlines. Chain notation (`p1 > p2 > p3`), batch notation
#' (`{p1,p2} > {p3,p4}`), and free-form linear expressions are all
#' accepted.
#'
#' @param models Named or unnamed character vector. Each element is one
#'   model. E.g. `c(H1 = "p1 > p2 > p3", H2 = "p1 > p2; p3 > p4")`.
#' @param n_p Integer. Number of probability parameters. If omitted,
#'   inferred from the highest `p` index across all models.
#' @param p_names Optional character vector of length `n_p` with
#'   human-readable labels for the probabilities.
#' @param approx_tol Numeric. Tolerance for approximate equalities.
#'   Default `0`.
#' @param reduce Logical. Remove redundant H-rep rows. Default `TRUE`.
#' @param n_samples Integer. Set to `0` to skip the parsimony computation
#'   (the volume is exact for small models; this value only matters when it
#'   has to be estimated by Monte Carlo).
#'
#' @return If `models` has length 1, a list of class `"fairy"`. If
#'   `models` has length > 1, a named list of `"fairy"` objects, one
#'   per model.
#'
#' @examples
#' # Single model
#' fairy("p1 > p2 > p3", n_p = 3)
#'
#' # Multiple models
#' fairy(c(
#'   H1 = "p1 > p2 > p3",
#'   H2 = "p1 > p2; p3 > p4",
#'   H3 = "p1 = p2 = p3"
#' ), n_p = 3)
#'
#' @seealso [fairy_h()], [fairy_v()], [fairy_parsimony()]
#' @export
fairy <- function(models, n_p = NULL, p_names = NULL,
                  approx_tol = 0, reduce = TRUE, n_samples = 1e5) {
  if (is.null(names(models))) names(models) <- paste0("M", seq_along(models))

  if (is.null(n_p)) {
    idx <- as.integer(unlist(regmatches(
      models,
      gregexpr("(?<=p)[0-9]+", models, perl = TRUE)
    )))
    if (length(idx) == 0) stop("Cannot infer n_p: no p-indices found in models.")
    n_p <- max(idx)
  }

  results <- lapply(names(models), function(nm) {
    h      <- fairy_h(models[[nm]], n_p = n_p, p_names = p_names,
                     approx_tol = approx_tol, reduce = reduce)
    n_p_h  <- ncol(h) - 2L   # actual n_p after any auto-expansion
    v      <- fairy_v(h)

    if (n_samples > 0) {
      pars         <- fairy_parsimony(h, n_p = n_p_h, n_samples = n_samples)
      parsimony    <- pars$parsimony
      parsimony_se <- pars$se
      max_bf       <- pars$max_bf
    } else {
      parsimony    <- NULL
      parsimony_se <- NULL
      max_bf       <- NULL
    }

    structure(
      list(
        name         = nm,
        model        = models[[nm]],
        h            = h,
        v            = v,
        parsimony    = parsimony,
        parsimony_se = parsimony_se,
        max_bf       = max_bf,
        n_p          = n_p_h
      ),
      class = "fairy"
    )
  })
  names(results) <- names(models)

  if (length(results) == 1L) results[[1L]] else results
}


#' @export
print.fairy <- function(x, ...) {
  cat("fairy model:", x$name, "\n")
  cat("  Model:       ", x$model, "\n")
  cat("  Parameters:  ", x$n_p, "\n")
  cat("  H-rep rows:  ", nrow(x$h), "\n")
  v_num  <- rcdd::q2d(x$v)
  n_vert <- sum(v_num[, 1] == 0 & v_num[, 2] == 1)
  cat("  Vertices:    ", n_vert, "\n")
  if (!is.null(x$parsimony)) {
    if (is.na(x$parsimony_se)) cat(sprintf("  Volume:       %.6g\n", x$parsimony))
    else cat(sprintf("  Volume:       %.6g  (SE = %.3g)\n", x$parsimony, x$parsimony_se))
    cat("  Max. BF:      ", format(x$max_bf, digits = 4), "\n")
  }
  invisible(x)
}
