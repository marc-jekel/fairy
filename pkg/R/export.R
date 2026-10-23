#' Write QTest input files for one or more models
#'
#' Generates the plain-text input files expected by QTest (Regenwetter &
#' Davis-Stober, 2012). Each model produces one `.txt` file containing
#' the integer-scaled A matrix and b vector (inequalities only; models
#' with equalities get a notice file instead). All files are bundled in
#' a single `.tar` archive.
#'
#' @param models Named character vector of model specifications, as
#'   accepted by [fairy()]. Each element is one model.
#' @param n_p Integer. Number of probability parameters.
#' @param file Path for the output `.tar` file. Defaults to
#'   `"qtest_YYYY-MM-DD.tar"` in the current directory.
#' @param approx_tol Numeric. Tolerance for approximate equalities. Default `0`.
#' @param reduce Logical. Remove redundant H-rep rows. Default `TRUE`.
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
#' }
#'
#' @importFrom MASS fractions
#' @importFrom rcdd q2d d2q
#' @export
fairy_write_qtest <- function(models, n_p, file = NULL, approx_tol = 0, reduce = TRUE) {
  if (is.null(file)) file <- paste0("qtest_", Sys.Date(), ".tar")
  if (!grepl("^/|^~", file)) file <- file.path(getwd(), file)
  file <- path.expand(file)
  if (is.null(names(models))) names(models) <- paste0("M", seq_along(models))

  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  files <- character()

  for (nm in names(models)) {
    h     <- fairy_h(models[[nm]], n_p = n_p, approx_tol = approx_tol, reduce = reduce)
    h_num <- q2d(h)

    ineq_rows <- h_num[h_num[, 1] == 0, , drop = FALSE]
    eq_rows   <- h_num[h_num[, 1] == 1, , drop = FALSE]

    safe_nm   <- gsub("[^A-Za-z0-9_.-]", "_", nm)
    file_name <- paste0(safe_nm, ".txt")

    if (nrow(eq_rows) > 0) {
      content <- "The model includes equalities. Use V-representation of model for QTest."
      write.table(content, file_name, quote = FALSE, col.names = FALSE, row.names = FALSE)
    } else {
      a_mat <- -ineq_rows[, 3:ncol(ineq_rows), drop = FALSE]
      b_vec <- ineq_rows[, 2]

      a_frac <- fractions(a_mat)
      b_frac <- fractions(b_vec)

      # extract numerators/denominators to find common integer scale
      .nom   <- function(x) {
        n <- sub("/.*", "", as.character(x))
        as.numeric(ifelse(grepl("/", as.character(x)), n, as.character(x)))
      }
      .denom <- function(x) {
        d <- sub(".*/", "", as.character(x))
        as.numeric(ifelse(grepl("/", as.character(x)), d, "1"))
      }

      all_denoms   <- c(.denom(a_frac), .denom(b_frac))
      scale        <- prod(unique(all_denoms[all_denoms != 0]))

      a_scaled <- format(a_frac * scale, scientific = FALSE)
      b_scaled <- format(b_frac * scale, scientific = FALSE)

      header  <- paste(nrow(a_scaled), ncol(a_scaled))
      content <- paste(
        header, "\n\n",
        paste(apply(a_scaled, 1, paste, collapse = " "), collapse = "\n"),
        "\n\n",
        paste(b_scaled, collapse = "\n"),
        sep = ""
      )

      write.table(content, file_name, quote = FALSE, col.names = FALSE, row.names = FALSE)
    }

    files <- c(files, file_name)
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
#' the H-representation.
#'
#' @param models Named character vector of model specifications, as
#'   accepted by [fairy()].
#' @param n_p Integer. Number of probability parameters.
#' @param file Path for the output `.tar` file. Defaults to
#'   `"multinomineq_YYYY-MM-DD.tar"` in the current directory.
#' @param approx_tol Numeric. Tolerance for approximate equalities. Default `0`.
#' @param reduce Logical. Remove redundant H-rep rows. Default `TRUE`.
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
#' @export
fairy_write_multinomineq <- function(models, n_p, file = NULL, approx_tol = 0, reduce = TRUE) {
  if (is.null(file)) file <- paste0("multinomineq_", Sys.Date(), ".tar")
  if (!grepl("^/|^~", file)) file <- file.path(getwd(), file)
  file <- path.expand(file)
  if (is.null(names(models))) names(models) <- paste0("M", seq_along(models))

  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  files <- character()

  for (nm in names(models)) {
    h     <- fairy_h(models[[nm]], n_p = n_p, approx_tol = approx_tol, reduce = reduce)
    safe_nm <- gsub("[^A-Za-z0-9_.-]", "_", nm)

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
