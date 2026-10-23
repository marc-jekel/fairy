# Internal helpers for constraint normalization and parsing.
# These are direct extractions from the Fairy Shiny app (app.R).

#' @importFrom MASS fractions
#' @importFrom stringr str_replace_all str_count str_extract str_split_fixed
num2str_lin <- function(x) {
  if (!is.finite(x)) return(format(x, scientific = FALSE, trim = TRUE))
  if (x == round(x)) return(format(x, scientific = FALSE, trim = TRUE))
  fr <- attr(fractions(x), "fracs")
  if (!is.null(fr) && length(fr) == 1 && !is.na(fr)) return(as.character(fr))
  format(x, scientific = FALSE, trim = TRUE, digits = 15)
}

# Rewrites any linear constraint into canonical form c1*p1 + ... <op> constant.
# Works by evaluating each side as a linear function via numerical probing.
# Non-linear constraints are returned unchanged.
normalize_constraint <- function(cstr, n_p) {
  if (is.na(cstr) || cstr == "") return(cstr)

  cstr <- str_replace_all(cstr, ">=|=>", ">")
  cstr <- str_replace_all(cstr, "<=|=<", "<")

  if (str_count(cstr, "<|>|=") != 1) return(cstr)

  op    <- str_extract(cstr, "<|>|=")
  sides <- str_split_fixed(cstr, "<|>|=", 2)
  lhs   <- sides[1]
  rhs   <- sides[2]
  if (lhs == "" || rhs == "") return(cstr)

  ev <- function(s, vals) {
    env <- as.list(vals)
    names(env) <- paste0("p", seq_along(vals))
    tryCatch(
      as.numeric(eval(parse(text = s), envir = env)),
      error   = function(e) NA_real_,
      warning = function(w) NA_real_
    )
  }

  ok_num <- function(x) length(x) == 1 && !is.na(x) && is.finite(x)

  zero <- rep(0,    n_p)
  base <- rep(0.25, n_p)

  c_l <- ev(lhs, zero)
  c_r <- ev(rhs, zero)
  if (!ok_num(c_l) || !ok_num(c_r)) return(cstr)

  coef <- numeric(n_p)
  for (i in seq_len(n_p)) {
    e_i    <- zero
    e_i[i] <- 1
    a <- ev(lhs, e_i)
    b <- ev(rhs, e_i)
    if (!ok_num(a) || !ok_num(b)) return(cstr)
    coef[i] <- (a - c_l) - (b - c_r)
  }

  for (i in seq_len(n_p)) {
    e_i    <- zero
    e_i[i] <- 2
    a <- ev(lhs, e_i)
    b <- ev(rhs, e_i)
    if (!ok_num(a) || !ok_num(b)) return(cstr)
    d2 <- ((a - c_l) - (b - c_r)) - 2 * coef[i]
    if (!ok_num(d2) || abs(d2) > 1e-9) return(cstr)

    f_l <- ev(lhs, base)
    f_r <- ev(rhs, base)
    s_i    <- base
    s_i[i] <- s_i[i] + 1
    g_l <- ev(lhs, s_i)
    g_r <- ev(rhs, s_i)
    if (!ok_num(f_l) || !ok_num(f_r) || !ok_num(g_l) || !ok_num(g_r)) return(cstr)
    d3 <- ((g_l - f_l) - (g_r - f_r)) - coef[i]
    if (!ok_num(d3) || abs(d3) > 1e-9) return(cstr)
  }

  if (all(coef == 0)) return(cstr)

  terms <- paste0(
    ifelse(coef >= 0, "+", "-"),
    vapply(abs(coef), num2str_lin, character(1)),
    "*p", seq_len(n_p)
  )

  paste0(paste(terms, collapse = ""), op, num2str_lin(c_r - c_l))
}
