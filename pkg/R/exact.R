# Exact (rational) arithmetic for the numbers of a model specification.
#
# A typed number such as 1/3 or 0.0001 is kept as an exact fraction from the text up to rcdd, which then calculates exactly.
# Going through a floating-point number in between would round 1/3 and make "p1 < 1/3; 3*p1 > 1" slightly non-zero
# instead of exactly zero in volume.

#' @importFrom rcdd q2q qpq qmq qxq qdq qinv qneg qabs qsign d2q
#' @noRd
NULL

# one decimal number or a fraction of two ("0.25", ".5", "12", "3/8", "0.1/0.3", optionally with a sign) as an exact rational
# string; NA when the text is anything else
q_from_text <- function(s) {
  num <- "[0-9]+\\.?[0-9]*|\\.[0-9]+"
  m <- regmatches(s, regexec(paste0("^([+-]?)(", num, ")(/(", num, "))?$"), s))[[1]]
  if (length(m) == 0) return(NA_character_)
  dec <- function(x) {
    parts <- strsplit(x, ".", fixed = TRUE)[[1]]
    int <- if (length(parts) >= 1 && nzchar(parts[1])) parts[1] else "0"
    frac <- if (length(parts) >= 2) parts[2] else ""
    # "x." gives one part only; "x.y" gives two
    if (!nzchar(frac)) return(sub("^0+(?=[0-9])", "", int, perl = TRUE))
    n <- sub("^0+(?=[0-9])", "", paste0(int, frac), perl = TRUE)
    paste0(n, "/1", strrep("0", nchar(frac)))
  }
  v <- dec(m[3])
  if (nzchar(m[5])) {
    d <- dec(m[5])
    if (qsign(q2q(d)) == 0) return(NA_character_)
    v <- qdq(q2q(v), q2q(d))
  } else {
    v <- q2q(v)
  }
  if (identical(m[2], "-")) v <- qneg(v)
  v
}

# exact negation of a matrix/vector of rational strings, keeping its shape
qneg_m <- function(x) {
  y <- qneg(as.character(x))
  dim(y) <- dim(x); dimnames(y) <- dimnames(x)
  y
}

# a double (e.g. a tolerance typed as number) as the exact rational of its shortest decimal text, 0.05 -> 1/20
q_from_number <- function(x) {
  r <- q_from_text(format(x, scientific = FALSE, digits = 15, trim = TRUE))
  if (is.na(r)) d2q(x) else r
}

# Linear form of an expression with exact coefficients: list(a = rational per probability, c = rational constant).
# NULL when the expression is not linear (or not understood); then the caller keeps the text as it is.
linear_form <- function(s, n_p) {
  s <- gsub("[[:space:]]", "", s)
  chars <- strsplit(s, "")[[1]]
  pos <- 1L
  nch <- length(chars)
  peek <- function() if (pos <= nch) chars[pos] else ""
  zero <- function() list(a = rep("0", n_p), c = "0")
  is_const <- function(f) all(f$a == "0")
  add <- function(f, g, sign = 1) {
    op <- if (sign > 0) qpq else qmq
    list(a = op(f$a, g$a), c = op(f$c, g$c))
  }
  scale <- function(f, k) list(a = qxq(f$a, rep(k, n_p)), c = qxq(f$c, k))
  fail <- function() stop("not linear", call. = FALSE)

  expr <- function() {
    f <- term()
    while (peek() %in% c("+", "-")) {
      op <- peek(); pos <<- pos + 1L
      g <- term()
      f <- add(f, g, if (op == "+") 1 else -1)
    }
    f
  }
  term <- function() {
    f <- factor_()
    while (peek() %in% c("*", "/")) {
      op <- peek(); pos <<- pos + 1L
      g <- factor_()
      if (op == "*") {
        if (is_const(g)) f <- scale(f, g$c)
        else if (is_const(f)) f <- scale(g, f$c)
        else fail()
      } else {
        if (!is_const(g) || qsign(g$c) == 0) fail()
        f <- scale(f, qinv(g$c))
      }
    }
    f
  }
  factor_ <- function() {
    ch <- peek()
    if (ch == "+") { pos <<- pos + 1L; return(factor_()) }
    if (ch == "-") { pos <<- pos + 1L; return(scale(factor_(), "-1")) }
    if (ch == "(") {
      pos <<- pos + 1L
      f <- expr()
      if (peek() != ")") fail()
      pos <<- pos + 1L
      return(f)
    }
    if (ch == "p") {
      pos <<- pos + 1L
      st <- pos
      while (grepl("^[0-9]$", peek())) pos <<- pos + 1L
      if (pos == st) fail()
      i <- as.integer(paste(chars[st:(pos - 1L)], collapse = ""))
      if (i < 1L || i > n_p) fail()
      f <- zero(); f$a[i] <- "1"
      return(f)
    }
    st <- pos
    while (grepl("^[0-9.]$", peek())) pos <<- pos + 1L
    if (pos == st) fail()
    v <- q_from_text(paste(chars[st:(pos - 1L)], collapse = ""))
    if (is.na(v)) fail()
    f <- zero(); f$c <- v
    f
  }
  tryCatch({
    f <- expr()
    if (pos <= nch) NULL else f
  }, error = function(e) NULL)
}
