# The models of the paper and its supplement, used only by the tests. The package itself
# does not store them: the vignette shows how to enter each one.
cringe_models <- function(which = 1:10, as = c("h", "string")) {
  as <- match.arg(as)
  s <- c(
    M1 = "(p1+p4)/2 < (p2+p5)/2 < (p3+p6)/2",
    M2 = "p1 < p2 < p3; p4 < p5 < p6",
    M3 = "p1 > p2 > p3; p4 < p5 < p6; p1+p2+p3 < p4+p5+p6",
    M4 = "p1 > p2 > p3; p4 < p5 < p6; p1 < p4; p2 < p5; p3 < p6",
    M5 = "p1 = p2 = p3 = p4 = p5 = p6",
    M6 = "p1 > p2 > p3; p4 < p5 < p6; p1+p2+p3 = p4+p5+p6",
    M7 = "p1 > p2 > p3; p4 < p5 < p6; (p1+p2+p3)/3 = (p4+p5+p6)/3",
    M8 = "{p1,p2,p3,p4,p5,p6} = {p1,p2,p3,p4,p5,p6}",
    M9 = "p1 < p2 < p3; p4 > p5 > p6; p1 > p4; p2 > p5; p3 > p6"
  )
  names(which) <- NULL
  if (as == "string") return(as.list(s[paste0("M", setdiff(which, 10L))]))
  tol <- c(M7 = 0.05, M8 = 0.05)
  out <- list()
  for (i in which) {
    nm <- paste0("M", i)
    out[[nm]] <- if (i == 10L) {
      fairy_mixture(fairy_h(s[["M4"]], n_p = 6), fairy_h(s[["M9"]], n_p = 6))
    } else {
      fairy_h(s[[nm]], n_p = 6, approx_tol = if (nm %in% names(tol)) tol[[nm]] else 0)
    }
  }
  out
}

cringe_labels <- function() {
  c("illogical: children", "illogical: teens", "illogical: adults",
    "pun: children", "pun: teens", "pun: adults")
}

hsm_models <- function() {
  s <- c(
    pred1 = "p1 < p2; p3 < p4",
    pred2 = "p1 < p3; p2 < p4",
    pred3 = "p1 < p2 < p4; p1 < p3 < p4",
    pred6 = "p1 < p2 < p3; p4 < p5 < p6; p7 < p8 < p9; p1 < p4 < p7; p2 < p5 < p8; p3 < p6 < p9",
    override = "p6 > p3 > p2 = p1; p6 > p4 = p5 > p2",
    tradeoff = "p6 > p3 > p2 > p1; p6 > p5 > p4 > p1; p5 > p2",
    ttb = "p1 = 0; p4 = 0; p2 = 1; p5 = 1; p3 = 1; p6 = 1"
  )
  out <- lapply(s, fairy_h)
  out$override_ttb_mixture <- fairy_mixture(out$override, out$ttb)
  out
}
