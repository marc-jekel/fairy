# Unpacking a model across items (stimuli, trials, labs): the general case.

#' Unpack a model across items
#'
#' Applies a model to several items per probability. This is the general
#' form of [fairy_replicate()]: probabilities can have different numbers of
#' items, can share an item list, and each list can say in its own way how
#' the theory holds across its items.
#'
#' The result has one probability per probability and item, named
#' `p1^(1)`, `p1^(2)`, and so on. Within a list the columns are ordered item
#' by item (all probabilities of the list for item 1, then for item 2, ...),
#' and the lists follow each other in the order in which they first occur.
#'
#' The ways a theory can hold across the items of a list (`type`):
#'
#' * `"joint"`: item by item. Every constraint holds for each item `k`,
#'   with the same item `k` in all probabilities of the list.
#' * `"substitutable"`: for every combination of items. Each constraint
#'   holds whichever item is plugged in for each term. A constraint with `m`
#'   probabilities of a list and `n` items gives `n^m` constraints.
#' * `"identical"`: all items of a probability have the same value, within a
#'   tolerance `eps` (`0` means exactly equal). The constraints hold for
#'   item by item, on top of the equalities.
#' * `"average"`: the constraints hold for the average over the items of
#'   each probability. Individual items may break them, but each item still
#'   has to be a probability between 0 and 1.
#'
#' The function only checks that the numbers of items fit together. That item
#' `k` of one probability means the same thing as item `k` of another is
#' something you decide.
#'
#' @param h A model (string with `n_p`, or H-representation).
#' @param items Number of items. One number for all probabilities, or one per
#'   probability. Probabilities in the same list must have the same number.
#' @param lists Which probabilities share an item list: a vector with one
#'   entry per probability (numbers or labels). The default puts all
#'   probabilities in one list if they have the same number of items, and
#'   gives each its own list otherwise.
#' @param type One of `"joint"`, `"substitutable"`, `"identical"`,
#'   `"average"`. One value for all lists, or one per list in the order in
#'   which the lists first occur (or named by list).
#' @param eps Tolerance for `"identical"`: one number, or one per probability.
#' @param n_p Number of probabilities for a model string.
#' @param reduce Remove redundant constraints? Default `TRUE`. For large
#'   substitutable models this can take a while.
#'
#' @return An H-representation over all probabilities and items, with the
#'   probability names as column names.
#'
#' @examples
#' # three items per probability, the model holds item by item
#' fairy_info(fairy_unpack("p1 > p2; p2 > p3", items = 3, type = "joint", n_p = 3))
#'
#' # p1 and p2 share a list of 2 items, p3 has its own list of 3 items
#' h <- fairy_unpack("p3 > p1 + p2", items = c(2, 2, 3), lists = c("A", "A", "B"),
#'                   type = c(A = "substitutable", B = "average"), n_p = 3)
#' @importFrom rcdd d2q q2d redundant
#' @export
fairy_unpack <- function(h, items, lists = NULL, type = "joint", eps = 0,
                         n_p = NULL, reduce = TRUE) {
  h <- resolve_h(h, n_p = n_p)
  hn <- q2d(h)
  n <- ncol(h) - 2L
  base_names <- colnames(h)[-(1:2)]
  if (is.null(base_names) || any(!nzchar(base_names))) base_names <- paste0("p", seq_len(n))

  if (length(items) == 1L) items <- rep(items, n)
  if (length(items) != n) stop("`items` needs one number, or one per probability (", n, ").")
  if (any(items < 1 | items != round(items))) stop("`items` must be positive whole numbers.")
  items <- as.integer(items)

  if (is.null(lists)) lists <- if (length(unique(items)) == 1L) rep("A", n) else as.character(seq_len(n))
  if (length(lists) != n) stop("`lists` needs one entry per probability (", n, ").")
  lists <- as.character(lists)
  lev <- unique(lists)                         # lists in order of first occurrence
  f_of <- match(lists, lev)                    # list (factor) index of each probability
  F <- length(lev)
  if (any(vapply(seq_len(F), function(f) length(unique(items[f_of == f])) > 1L, logical(1))))
    stop("Probabilities in the same list must have the same number of items.")
  f_ns <- vapply(seq_len(F), function(f) unique(items[f_of == f]), integer(1))

  type_ok <- c("joint", "substitutable", "identical", "average")
  if (length(type) == 1L) type <- rep(type, F)
  if (!is.null(names(type))) type <- type[lev]
  if (length(type) != F || anyNA(type)) stop("`type` needs one value, or one per list (", F, ").")
  type <- vapply(type, function(t) match.arg(t, type_ok), character(1), USE.NAMES = FALSE)
  if (length(eps) == 1L) eps <- rep(eps, n)
  if (length(eps) != n) stop("`eps` needs one number, or one per probability (", n, ").")

  params_of <- lapply(seq_len(F), function(f) which(f_of == f))
  n_pf <- vapply(params_of, length, integer(1))
  tc <- 2L + sum(n_pf * f_ns)
  fbase <- integer(F); acc <- 0L
  for (f in seq_len(F)) { fbase[f] <- acc; acc <- acc + n_pf[f] * f_ns[f] }
  col_f <- function(f, lv, li) 2L + fbase[f] + (lv - 1L) * n_pf[f] + li

  rows <- list()
  add <- function(r) rows[[length(rows) + 1L]] <<- r
  for (ri in seq_len(nrow(hn))) {
    a_vec <- hn[ri, 3:(2L + n)]
    nz_by_f <- lapply(seq_len(F), function(f) { pi <- params_of[[f]]; pi[a_vec[pi] != 0] })
    involved <- which(lengths(nz_by_f) > 0)
    base_row <- numeric(tc); base_row[1] <- hn[ri, 1]; base_row[2] <- hn[ri, 2]
    is_avg <- function(f) identical(type[f], "average")
    for (f in involved[vapply(involved, is_avg, logical(1))]) {
      nz <- nz_by_f[[f]]
      for (slot in seq_along(nz)) {
        li <- which(params_of[[f]] == nz[slot])
        for (lv in seq_len(f_ns[f])) {
          cc <- col_f(f, lv, li); base_row[cc] <- base_row[cc] + a_vec[nz[slot]] / f_ns[f]
        }
      }
    }
    grid_f <- involved[!vapply(involved, is_avg, logical(1))]
    if (length(grid_f) == 0L) { add(base_row); next }
    grids <- lapply(grid_f, function(f) {
      nz <- nz_by_f[[f]]
      if (identical(type[f], "substitutable"))
        as.matrix(expand.grid(rep(list(seq_len(f_ns[f])), length(nz)), KEEP.OUT.ATTRS = FALSE))
      else matrix(rep(seq_len(f_ns[f]), each = length(nz)), ncol = length(nz), byrow = TRUE)
    })
    combo <- as.matrix(expand.grid(lapply(grids, function(g) seq_len(nrow(g))), KEEP.OUT.ATTRS = FALSE))
    for (ci in seq_len(nrow(combo))) {
      row <- base_row
      for (k in seq_along(grid_f)) {
        f <- grid_f[k]; nz <- nz_by_f[[f]]; g <- grids[[k]]
        for (slot in seq_along(nz)) {
          li <- which(params_of[[f]] == nz[slot]); cc <- col_f(f, g[combo[ci, k], slot], li)
          row[cc] <- row[cc] + a_vec[nz[slot]]
        }
      }
      add(row)
    }
  }
  # every copy of an averaged probability stays inside [0, 1] on its own
  for (f in which(type == "average")) for (li in seq_len(n_pf[f])) for (lv in seq_len(f_ns[f])) {
    cc <- col_f(f, lv, li)
    up <- numeric(tc); up[2] <- 1; up[cc] <- -1
    lo <- numeric(tc); lo[cc] <- 1
    add(up); add(lo)
  }
  # identical: the items of a probability equal the first item, within eps
  for (f in which(type == "identical")) {
    if (f_ns[f] < 2L) next
    for (lv in 2:f_ns[f]) for (li in seq_len(n_pf[f])) {
      e <- eps[params_of[[f]][li]]
      a <- numeric(tc); a[col_f(f, 1L, li)] <- 1; a[col_f(f, lv, li)] <- -1
      if (e == 0) { add(c(1, 0, a[-(1:2)])) }
      else { add(c(0, e, a[-(1:2)])); add(c(0, e, -a[-(1:2)])) }
    }
  }
  m <- unique(do.call(rbind, rows))
  newnames <- unlist(lapply(seq_len(F), function(f)
    unlist(lapply(seq_len(f_ns[f]), function(lv) paste0(base_names[params_of[[f]]], "^(", lv, ")")))))
  hh <- d2q(m)
  colnames(hh) <- c("", "", newnames)
  if (reduce && nrow(hh) > 1L) {
    rr <- redundant(hh)$output
    colnames(rr) <- c("", "", newnames)
    hh <- rr
  }
  hh
}
