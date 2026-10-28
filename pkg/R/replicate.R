# Replication models: the same theory in r labs / populations.

#' Replicate a model across labs or populations
#'
#' Builds a model for `r` labs (or populations, or conditions) with `n`
#' probabilities each, so `n * r` probabilities in total. Three ways of
#' unpacking "the theory holds in every lab" are available.
#'
#' * `"identical"`: every probability is the same in all labs. This adds
#'   `n * (r - 1)` equalities, so the model has dimension `n` and volume zero.
#' * `"joint"`: every lab satisfies the model, with no link between the labs.
#'   The volume is the `r`-th power of the single-lab volume.
#' * `"substitutable"`: each order constraint holds whichever lab's
#'   probability is plugged in for each term. For `p1 < p2` this means that
#'   every lab's `p1` is below every lab's `p2`. A constraint with `k`
#'   probabilities produces `r^k` constraints before redundancies are removed.
#'
#' The probabilities of the result are ordered lab by lab: first all `n`
#' probabilities of lab 1, then those of lab 2, and so on.
#'
#' @param h A model (string with `n_p`, or H-representation).
#' @param r Number of labs.
#' @param type `"identical"`, `"joint"` or `"substitutable"`.
#' @param n_p Number of probabilities for a model string.
#' @param reduce Remove redundant constraints? Default `TRUE`. For large
#'   substitutable models this can take a while.
#'
#' @return An H-representation over `n * r` probabilities.
#'
#' @examples
#' m4 <- fairy_h("p1>p2>p3; p4<p5<p6; p1<p4; p2<p5; p3<p6", n_p = 6)
#' fairy_info(fairy_replicate(m4, r = 2, type = "joint"))
#' @importFrom rcdd d2q q2d redundant
#' @export
fairy_replicate <- function(h, r, type = c("identical", "joint", "substitutable"),
                            n_p = NULL, reduce = TRUE) {
  type <- match.arg(type)
  h <- resolve_h(h, n_p = n_p)
  n <- ncol(h) - 2L
  if (r < 1 || r != round(r)) stop("r must be a positive whole number.")
  hn <- q2d(h)
  typ <- hn[, 1]; b <- hn[, 2]; A <- hn[, -(1:2), drop = FALSE]
  nn <- n * r
  base_names <- colnames(h)[-(1:2)]
  newnames <- as.vector(sapply(seq_len(r), function(l) paste0(base_names, "^(", l, ")")))
  col <- function(l, j) (l - 1L) * n + j

  rows <- list()
  add <- function(type_, b_, a_) rows[[length(rows) + 1L]] <<- c(type_, b_, a_)

  if (type == "joint" || type == "identical") {
    for (l in seq_len(r)) {
      if (type == "identical" && l > 1L) next
      for (i in seq_len(nrow(hn))) {
        a <- numeric(nn); a[col(l, seq_len(n))] <- A[i, ]
        add(typ[i], b[i], a)
      }
    }
    if (type == "identical" && r > 1L) {
      for (l in 2:r) for (j in seq_len(n)) {
        a <- numeric(nn); a[col(1L, j)] <- 1; a[col(l, j)] <- -1
        add(1, 0, a)
      }
    }
  } else {
    if (any(typ == 1)) stop("Substitutable replication needs a model without equality constraints (use approx_tol to relax equalities).")
    for (i in seq_len(nrow(hn))) {
      S <- which(A[i, ] != 0)
      combos <- as.matrix(expand.grid(rep(list(seq_len(r)), length(S))))
      for (k in seq_len(nrow(combos))) {
        a <- numeric(nn)
        a[col(combos[k, ], S)] <- A[i, S]
        add(0, b[i], a)
      }
    }
  }
  m <- do.call(rbind, rows)
  m <- unique(m)
  hh <- d2q(m)
  colnames(hh) <- c("", "", newnames)
  if (reduce && nrow(hh) > 1L) {
    rr <- redundant(hh)$output
    colnames(rr) <- c("", "", newnames)
    hh <- rr
  }
  if (type == "joint") {
    # the labs are independent blocks: the volume is the single-lab volume to the power r
    # (computed on demand by fairy_volume() with the method the user picks)
    attr(hh, "fairy_joint") <- list(h = h, r = r)
  }
  hh
}
