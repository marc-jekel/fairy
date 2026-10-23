# Same set of rows, ignoring row order and tiny numerical noise?
same_rows <- function(a, b, tol = 1e-8) {
  a <- unname(as.matrix(a)); b <- unname(as.matrix(b))
  if (!identical(dim(a), dim(b))) return(FALSE)
  key <- function(m) {
    m <- round(m / tol) * tol
    m <- m[do.call(order, as.data.frame(m)), , drop = FALSE]
    m
  }
  isTRUE(all.equal(key(a), key(b), tolerance = 1e-6))
}

# Words of the paper's tables as numbers
words_to_num <- function(m) {
  f <- function(x) vapply(x, function(s) {
    if (s == "all") 1 else if (s == "none") 0 else eval(parse(text = s))
  }, numeric(1))
  matrix(f(as.vector(m)), nrow = nrow(m))
}
