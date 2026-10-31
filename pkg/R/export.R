# ---- helpers shared by the writers -------------------------------------------

# one model (string or H-representation) as H-representation
# puts the probabilities (columns 3 onwards) of an H-representation into the order `perm`
# (perm[j] = the old column that becomes the j-th probability); NULL keeps the order
reorder_h_columns <- function(h, perm) {
  if (is.null(perm)) return(h)
  n_p <- ncol(h) - 2L
  perm <- as.integer(perm)
  if (length(perm) != n_p || !setequal(perm, seq_len(n_p)))
    stop("'order' must be a permutation of 1..", n_p, " (one entry per probability).", call. = FALSE)
  h[, c(1L, 2L, 2L + perm), drop = FALSE]
}

export_h_of <- function(m, n_p, approx_tol, reduce) {
  if (is.character(m) && !is.matrix(m)) {
    if (is.null(n_p)) stop("'n_p' is needed for model strings.")
    fairy_h(m, n_p = n_p, approx_tol = approx_tol, reduce = reduce)
  } else as_h(m)
}

# Eliminates every equality of an H-representation by substitution (solves it for one probability and
# substitutes it into the other rows), leaving a pure inequality system. Returns the reduced inequality
# matrix, the original column index of each remaining probability, how each eliminated probability is
# recovered, and whether the equalities contradict each other.
eliminate_h_equalities <- function(h_mat) {
  h_num <- q2d(h_mat)
  n_p   <- ncol(h_num) - 2L
  is_eq <- h_num[, 1] == 1
  eq_pool   <- h_num[is_eq, , drop = FALSE]
  ineq_rows <- h_num[!is_eq, , drop = FALSE]
  remaining_idx <- seq_len(n_p)
  elim_raw <- list(); elim_order <- character(); infeasible <- FALSE

  # drops column `pivot_pos` after subtracting the multiple of `eq_row` that zeroes it; the type flag
  # (column 1) is copied from the row itself and not part of the linear system
  reduce_matrix <- function(M, pivot_pos, a_piv, eq_row) {
    if (nrow(M) == 0) return(M[, -(2L + pivot_pos), drop = FALSE])
    out <- matrix(0, nrow = nrow(M), ncol = ncol(M) - 1L)
    for (r in seq_len(nrow(M))) {
      k <- M[r, 2L + pivot_pos]
      new_row <- if (abs(k) < 1e-12) M[r, ] else M[r, ] - (k / a_piv) * eq_row
      new_row[1] <- M[r, 1]
      out[r, ] <- new_row[-(2L + pivot_pos)]
    }
    out
  }

  while (nrow(eq_pool) > 0 && length(remaining_idx) > 0) {
    row <- eq_pool[1, ]
    eq_pool <- eq_pool[-1, , drop = FALSE]
    coefs <- row[3L:(2L + length(remaining_idx))]
    pivot_pos <- which(abs(coefs) > 1e-9)[1]
    if (is.na(pivot_pos)) {
      if (abs(row[2]) > 1e-9) { infeasible <- TRUE; break }   # 0 = nonzero
      next                                                    # 0 = 0: already implied
    }
    pivot_orig <- remaining_idx[pivot_pos]
    a_piv <- coefs[pivot_pos]
    other_pos <- setdiff(seq_along(remaining_idx), pivot_pos)
    elim_raw[[as.character(pivot_orig)]] <- list(
      const = -row[2] / a_piv,
      idx   = remaining_idx[other_pos],
      coefs = if (length(other_pos) > 0) -coefs[other_pos] / a_piv else numeric(0))
    elim_order <- c(elim_order, as.character(pivot_orig))
    eq_pool   <- reduce_matrix(eq_pool,   pivot_pos, a_piv, row)
    ineq_rows <- reduce_matrix(ineq_rows, pivot_pos, a_piv, row)
    remaining_idx <- remaining_idx[-pivot_pos]
  }

  # resolve each formula down to the surviving probabilities (backward pass)
  resolved <- list()
  for (ki in rev(seq_along(elim_order))) {
    key <- elim_order[ki]; e <- elim_raw[[key]]
    const <- e$const; new_idx <- numeric(0); new_coefs <- numeric(0)
    for (ii in seq_along(e$idx)) {
      dep_key <- as.character(e$idx[ii])
      if (dep_key %in% names(resolved)) {
        dep <- resolved[[dep_key]]
        const <- const + e$coefs[ii] * dep$const
        new_idx <- c(new_idx, dep$idx); new_coefs <- c(new_coefs, e$coefs[ii] * dep$coefs)
      } else { new_idx <- c(new_idx, e$idx[ii]); new_coefs <- c(new_coefs, e$coefs[ii]) }
    }
    if (length(new_idx) > 0) {
      u <- unique(new_idx)
      new_coefs <- vapply(u, function(ui) sum(new_coefs[new_idx == ui]), numeric(1)); new_idx <- u
    }
    resolved[[key]] <- list(const = const, idx = new_idx, coefs = new_coefs)
  }
  list(ineq = ineq_rows, remaining_idx = remaining_idx, eliminated = resolved, infeasible = infeasible)
}

# text of a QTEST H-representation file (integer-scaled A and b) for a numeric inequality matrix
format_qtest_ineq_file <- function(ineq_mat, n_p) {
  a_frac <- fractions(-ineq_mat[, 3L:(2L + n_p), drop = FALSE])
  b_frac <- fractions(ineq_mat[, 2])
  .nom   <- function(x) { n <- sub("/.*", "", as.character(x)); as.numeric(ifelse(grepl("/", as.character(x)), n, as.character(x))) }
  .denom <- function(x) { d <- sub(".*/", "", as.character(x)); as.numeric(ifelse(grepl("/", as.character(x)), d, "1")) }
  all_denoms <- c(.denom(a_frac), .denom(b_frac))
  scale <- prod(unique(all_denoms[all_denoms != 0]))
  a_scaled <- format(a_frac * scale, scientific = FALSE)
  b_scaled <- format(b_frac * scale, scientific = FALSE)
  paste(paste(nrow(ineq_mat), n_p), "\n\n",
        paste(apply(a_scaled, 1, paste, collapse = " "), collapse = "\n"), "\n\n",
        paste(b_scaled, collapse = "\n"), sep = "")
}

# note that ships next to a QTEST file whose equalities were eliminated
format_qtest_substitution_note <- function(elim, orig_names) {
  lines <- c("This model included equalities. QTEST's H-representation format",
             "has no way to express \"=\", so one parameter was eliminated per",
             "independent equality (solved for and substituted out) instead --",
             "the accompanying .txt file is the resulting pure-inequality",
             "system in the parameters listed below as \"kept\".", "")
  kept_names <- orig_names[elim$remaining_idx]
  lines <- c(lines, "Kept parameters (in the order they appear in the QTEST file):",
             paste0("  ", seq_along(kept_names), ": ", kept_names), "")
  if (length(elim$eliminated) > 0) {
    lines <- c(lines, "Eliminated parameters (recover after solving the reduced system):")
    for (key in names(elim$eliminated)) {
      e <- elim$eliminated[[key]]
      terms <- character(0)
      for (ii in order(match(e$idx, elim$remaining_idx))) {
        c_i <- e$coefs[ii]
        if (abs(c_i) < 1e-9) next
        coef_txt <- if (abs(abs(c_i) - 1) < 1e-9) "" else paste0(format(round(abs(c_i), 6)), "*")
        terms <- c(terms, paste(if (c_i >= 0) "+" else "-", paste0(coef_txt, orig_names[e$idx[ii]])))
      }
      lines <- c(lines, paste0("  ", orig_names[as.integer(key)], " = ", paste(c(format(round(e$const, 6)), terms), collapse = " ")))
    }
  }
  paste(lines, collapse = "\n")
}

#' Write QTEST input files for one or more models
#'
#' Generates the plain-text input files expected by QTEST (Regenwetter &
#' Davis-Stober, 2012), H-representation. Each model produces one `.txt` file
#' with the integer-scaled A matrix and b vector. QTEST cannot express
#' equalities, so for a model with equalities one probability is eliminated per
#' independent equality (solved for and substituted out) and a second file
#' `<name>_substitutions.txt` lists the kept probabilities and how the
#' eliminated ones are recovered. A model whose equalities contradict each other
#' gets a notice file instead. All files are bundled in a single `.tar` archive.
#'
#' @param models A (named) character vector or list. Each element is a model
#'   string, as accepted by [fairy()], or an H-representation (for example from
#'   [fairy_intersect()], [fairy_mixture()] or [fairy_unpack()]).
#' @param n_p Integer. Number of probabilities; only needed for model strings.
#' @param file Path for the output `.tar` file. Defaults to
#'   `"qtest_YYYY-MM-DD.tar"` in the current directory.
#' @param approx_tol Numeric. Tolerance for approximate equalities (model strings only): one number for all, or one per equality. Default `0`.
#' @param reduce Logical. Remove redundant H-rep rows (model strings only). Default `TRUE`.
#' @param order Optional named list: for a model (by name) the order of its probabilities in the
#'   file. `order[["m1"]] = c(3, 1, 2)` makes the third probability of the model the first one in
#'   the QTEST file, the first one the second, and so on. Models that are not listed keep their own
#'   order. Useful when QTEST expects the probabilities in a different order than they have here.
#'
#' @return Invisibly returns the path to the `.tar` file.
#'
#' @examples
#' \dontrun{
#' fairy_write_qtest(
#'   c(H1 = "p1 > p2 > p3", H2 = "p1 > p2"),
#'   n_p = 3,
#'   file = "qtest_output.tar"
#' )
#' # the third probability first
#' fairy_write_qtest(c(H1 = "p1 > p2 > p3"), n_p = 3, order = list(H1 = c(3, 1, 2)))
#' }
#'
#' @importFrom MASS fractions
#' @importFrom rcdd q2d d2q
#' @importFrom utils tar write.table
#' @export
fairy_write_qtest <- function(models, n_p = NULL, file = NULL, approx_tol = 0, reduce = TRUE, order = NULL) {
  if (is.null(file)) file <- paste0("qtest_", Sys.Date(), ".tar")
  if (!grepl("^/|^~", file)) file <- file.path(getwd(), file)
  file <- path.expand(file)
  if (is.null(names(models))) names(models) <- paste0("M", seq_along(models))

  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  files <- character()
  put <- function(txt, name) { write.table(txt, name, quote = FALSE, col.names = FALSE, row.names = FALSE); files <<- c(files, name) }

  for (nm in names(models)) {
    h <- export_h_of(models[[nm]], n_p, approx_tol, reduce)
    h <- reorder_h_columns(h, order[[nm]])
    h_num <- q2d(h)
    safe_nm <- gsub("[^A-Za-z0-9_.-]", "_", nm)
    orig_names <- colnames(h)[3:ncol(h)]
    if (any(h_num[, 1] == 1)) {
      elim <- eliminate_h_equalities(h)
      if (elim$infeasible) {
        put("This model's equalities are contradictory (no assignment of probabilities satisfies all of them) -- no QTEST file is possible.",
            paste0(safe_nm, "_no_QTEST_file_available.txt"))
      } else if (nrow(elim$ineq) == 0) {
        next
      } else {
        put(format_qtest_ineq_file(elim$ineq, length(elim$remaining_idx)), paste0(safe_nm, ".txt"))
        put(format_qtest_substitution_note(elim, orig_names), paste0(safe_nm, "_substitutions.txt"))
      }
    } else {
      ineq <- h_num[h_num[, 1] == 0, , drop = FALSE]
      if (nrow(ineq) == 0) next
      put(format_qtest_ineq_file(ineq, ncol(h) - 2L), paste0(safe_nm, ".txt"))
    }
  }

  tar(file, files)
  invisible(file)
}


#' Write multinomineq input files for one or more models
#'
#' Generates the A matrix and b vector CSV files expected by the
#' `multinomineq` R package (Heck & Davis-Stober, 2019). Each model
#' produces two files: `A_<name>.csv` and `b_<name>.csv`. All files
#' are bundled in a single `.tar` archive.
#'
#' The A matrix satisfies `A %*% p <= b` and is derived directly from
#' the H-representation. An equality is written as two inequalities (both
#' directions), so the polytope is exactly the model.
#'
#' @inheritParams fairy_write_qtest
#' @param file Path for the output `.tar` file. Defaults to
#'   `"multinomineq_YYYY-MM-DD.tar"` in the current directory.
#'
#' @return Invisibly returns the path to the `.tar` file.
#'
#' @examples
#' \dontrun{
#' fairy_write_multinomineq(
#'   c(H1 = "p1 > p2 > p3", H2 = "p1 > p2"),
#'   n_p = 3,
#'   file = "multinomineq_output.tar"
#' )
#' }
#'
#' @importFrom rcdd q2d d2q
#' @importFrom utils tar write.table
#' @export
fairy_write_multinomineq <- function(models, n_p = NULL, file = NULL, approx_tol = 0, reduce = TRUE, order = NULL) {
  if (is.null(file)) file <- paste0("multinomineq_", Sys.Date(), ".tar")
  if (!grepl("^/|^~", file)) file <- file.path(getwd(), file)
  file <- path.expand(file)
  if (is.null(names(models))) names(models) <- paste0("M", seq_along(models))

  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  files <- character()

  for (nm in names(models)) {
    h <- export_h_of(models[[nm]], n_p, approx_tol, reduce)
    h <- reorder_h_columns(h, order[[nm]])
    safe_nm <- gsub("[^A-Za-z0-9_.-]", "_", nm)

    # an equality row means a.p = -b: add the sign-flipped copy so that both directions are inequalities
    is_eq <- q2d(h)[, 1] == 1
    if (any(is_eq)) {
      flipped <- h[is_eq, , drop = FALSE]
      flipped[, 2:ncol(flipped)] <- d2q(-1 * q2d(h[is_eq, 2:ncol(h), drop = FALSE]))
      h <- rbind(h, flipped)
    }

    # b is column 2; A is -(columns 3+), giving A %*% p <= b
    b   <- h[, 2]
    A   <- d2q(-1 * q2d(h[, 3:ncol(h), drop = FALSE]))
    colnames(A) <- colnames(h)[3:ncol(h)]

    file_A <- paste0("A_", safe_nm, ".csv")
    write.table(A, file_A, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = ",")
    files <- c(files, file_A)

    file_b <- paste0("b_", safe_nm, ".csv")
    write.table(b, file_b, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = ",")
    files <- c(files, file_b)
  }

  tar(file, files)
  invisible(file)
}
