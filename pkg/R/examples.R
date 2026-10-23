#' The ten cringyness models of the paper
#'
#' The formalized models of the paper's running example: how cringeworthy are
#' illogical jokes and puns for children, teens and adults? The six
#' probabilities are, in this order,
#' `p1` = child / illogical joke, `p2` = teen / illogical joke,
#' `p3` = adult / illogical joke, `p4` = child / pun, `p5` = teen / pun,
#' `p6` = adult / pun.
#'
#' | Model | Verbal hypothesis |
#' |------:|:------------------|
#' | 1 | Cringyness increases with age (averaged over joke type) |
#' | 2 | Cringyness increases with age within each joke type |
#' | 3 | Illogical jokes decrease, puns increase with age; jokes less cringy than puns on average |
#' | 4 | As 3, and illogical jokes are less cringy than puns in every age group |
#' | 5 | No effect: all six probabilities are equal |
#' | 6 | As 3, with the two averages exactly equal |
#' | 7 | As 3, with the two averages within `0.05` of each other (use `approx_tol = 0.05`) |
#' | 8 | All six probabilities within `0.05` of each other (use `approx_tol = 0.05`) |
#' | 9 | The opposite of model 4 |
#' | 10 | Mixture of models 4 and 9 |
#'
#' @param which Models to return, default all ten.
#' @param as `"h"` (default) returns H-representations, `"string"` the model
#'   strings (model 10 is not a string, it is a mixture and is dropped).
#'
#' @return A named list.
#'
#' @examples
#' m <- cringe_models()
#' names(m)
#' fairy_info(m[["M4"]])
#' cringe_models(as = "string")$M4
#' @export
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

#' Labels of the six cringyness probabilities
#' @return A character vector of length 6.
#' @examples
#' cringe_labels()
#' @export
cringe_labels <- function() {
  c("illogical: children", "illogical: teens", "illogical: adults",
    "pun: children", "pun: teens", "pun: adults")
}

#' Models of the Heuristic-Systematic Model case study
#'
#' The theoretical predictions of the online supplement (a formalization of
#' Chaiken and colleagues' Heuristic-Systematic Model, HSM).
#'
#' * 2x2 design with probabilities `PbW, PbS, PsW, PsS` (big/small gap,
#'   weak/strong argument), in this order as `p1`-`p4`:
#'   `pred1` (strong arguments never hurt), `pred2` (a smaller gap never
#'   hurts), `pred3` (both).
#' * 3x3 design with `PbW, PbM, PbS, PmW, PmM, PmS, PsW, PsM, PsS` as
#'   `p1`-`p9`: `pred6` (the non-redundant constraints of equation 6).
#' * Cue congruence with `PiW, PnW, PcW, PiS, PnS, PcS` (incongruent, no,
#'   congruent cues; weak, strong arguments) as `p1`-`p6`: `override`,
#'   `tradeoff`, `ttb` (take the best), and `override_ttb_mixture`.
#'
#' @return A named list of H-representations.
#' @examples
#' hsm <- hsm_models()
#' sapply(hsm[c("pred1", "pred2", "pred3")], fairy_max_bf)   # 4, 4, 12
#' @export
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
