# Parses a constraint string (or vector of strings) into the matrices
# needed by rcdd::makeH().  Returns a list with:
#   $ineq_eq_left  - character matrix  (rows = constraints, cols = parameters)
#   $ineq_eq_right - character vector  (rhs values as rational strings)
#   $all_operators - character vector  ("below", "above", "equal")
#
# This is a self-contained extraction from extract_info() in app.R,
# with all Shiny reactive references replaced by plain arguments.

#' @importFrom stringr str_replace_all str_count str_extract str_split
#'   str_split_fixed str_locate_all str_length str_detect str_extract_all
#' @importFrom rcdd d2q q2d
extract_info <- function(constraints, n_p, approx_tol = 0) {
  # ---- pre-process: collapse to a single semicolon-delimited string ----
  test <- paste(constraints, collapse = ";")

  limits <- paste(
    paste(paste("p", 1:n_p, sep = ""), "< 1", collapse = ";"),
    paste(paste("p", 1:n_p, sep = ""), "> 0", collapse = ";"),
    sep = ";"
  )

  test <- str_replace_all(test, "\n", ";")
  # "<=", ">=" and the unicode symbols mean the same as "<" and ">": models are closed sets
  test <- str_replace_all(test, "<=|\u2264", "<")
  test <- str_replace_all(test, ">=|\u2265", ">")
  test <- str_replace_all(test, "==", "=")

  commas_outside_braces <- function(s) {
    if (is.na(s) || !str_detect(s, ",")) return(s)
    chars <- unlist(str_split(s, ""))
    depth <- 0L
    for (i in seq_along(chars)) {
      if (chars[i] == "{") depth <- depth + 1L
      if (chars[i] == "}") depth <- depth - 1L
      if (chars[i] == "," && depth <= 0L) chars[i] <- ";"
    }
    paste(chars, collapse = "")
  }

  test <- vapply(test, commas_outside_braces, character(1), USE.NAMES = FALSE)
  test <- paste0(test, ";", limits, collapse = "")
  test <- unlist(str_split(test, ";"))
  test <- str_replace_all(test, "\u2013", "-")
  test <- str_replace_all(test, "\u2014", "-")
  test <- str_replace_all(test, " ", "")
  test <- unlist(test)
  if (any(test == "")) test <- test[test != ""]

  # ---- split chained constraints (a < b < c -> a<b, b<c) ----
  # A run of three or more terms joined by "=" (p1 = p2 = p3) stands for ALL its pairs (p1=p2, p1=p3, p2=p3),
  # so that each pair can have its own tolerance, as in the Fairy app; number = number pairs are dropped.
  is_number <- function(x) grepl("^-?([0-9]+\\.?[0-9]*|\\.[0-9]+)(/[0-9]+)?$", x)
  members <- function(term) if (grepl("{", term, fixed = TRUE)) unlist(strsplit(gsub("[{}]", "", term), ",")) else term
  split_chain <- function(clause) {
    pos_signs <- str_locate_all(clause, "[=><]")[[1]][, 1]
    signs <- substr(rep(clause, length(pos_signs)), pos_signs, pos_signs)
    arguments <- strsplit(clause, "[=><]")[[1]]
    n_arg <- length(arguments)
    if (n_arg < 2) return(character(0))
    out <- character(0); i <- 1L
    while (i <= n_arg) {
      j <- i
      while (j < n_arg && signs[j] == "=") j <- j + 1L
      run <- arguments[i:j]
      if (length(run) >= 3) {
        mem <- unlist(lapply(run, members))
        pr <- utils::combn(length(mem), 2)
        keep <- !(is_number(mem[pr[1, ]]) & is_number(mem[pr[2, ]]))
        out <- c(out, paste0(mem[pr[1, keep]], "=", mem[pr[2, keep]]))
      } else if (length(run) == 2) {
        out <- c(out, paste0(run[1], "=", run[2]))
      }
      if (j < n_arg) out <- c(out, paste0(arguments[j], signs[j], arguments[j + 1]))
      i <- j + 1L
    }
    out
  }
  parts_all <- unlist(lapply(test, split_chain))
  test <- parts_all

  # ---- expand {p1,p2} > {p3,p4} batch notation ----
  expand_scalar <- function(scalar) {
    batches   <- strsplit(gsub("[{} ]", "", scalar), "[><=]")[[1]]
    batch1    <- unlist(strsplit(batches[1], ","))
    batch2    <- unlist(strsplit(batches[2], ","))
    separator <- gsub("[p{}]", "", gsub("[^><=]", "", scalar))
    comps <- character()
    for (p1 in batch1) for (p2 in batch2) comps <- c(comps, paste0(p1, separator, p2))
    paste(comps, collapse = ";")
  }

  test_new <- character()
  for (i in seq_along(test)) {
    if (grepl("{", test[i], fixed = TRUE)) {
      test_new <- c(test_new, expand_scalar(test[i]))
    } else {
      test_new <- c(test_new, test[i])
    }
  }
  test <- unlist(str_split(test_new, ";"))

  # ---- drop trivially true constraints (lhs == rhs as strings) ----
  test_new <- character()
  for (i in seq_along(test)) {
    lr <- unlist(str_split(test[i], ">|=|<"))
    if (lr[1] != lr[2]) test_new <- c(test_new, test[i])
  }
  test <- test_new

  for (i in seq_along(test)) {
    if (substr(test[i], 1, 1) == ".") test[i] <- paste0("0", test[i])
  }

  # ---- normalize ----
  test <- vapply(test, normalize_constraint, character(1), n_p = n_p, USE.NAMES = FALSE)

  # ---- locate operator positions ----
  loc_above   <- str_locate_all(test, ">")
  loc_equal   <- str_locate_all(test, "=")
  loc_below   <- str_locate_all(test, "<")
  loc_add     <- str_locate_all(test, "[+]")
  loc_subtract<- str_locate_all(test, "-")
  loc_divide  <- str_locate_all(test, "/")
  loc_multiply<- str_locate_all(test, "[*]")
  loc_digits  <- str_locate_all(test, "[:digit:]")
  loc_prob    <- str_locate_all(test, "p")
  loc_open    <- str_locate_all(test, "[(]")
  loc_closed  <- str_locate_all(test, "[)]")
  loc_point   <- str_locate_all(test, "[.]")

  loop_all      <- 1L
  all_extracted <- list()

  for (loop_bit in seq_along(test)) {
    test_actual        <- test[loop_bit]
    test_actual_single <- unlist(str_split(test_actual, ""))

    pos <- rbind(
      "above"    = data.frame(loc_above[[loop_bit]]),
      "below"    = data.frame(loc_below[[loop_bit]]),
      "equal"    = data.frame(loc_equal[[loop_bit]]),
      "add"      = data.frame(loc_add[[loop_bit]]),
      "subtract" = data.frame(loc_subtract[[loop_bit]]),
      "multiply" = data.frame(loc_multiply[[loop_bit]]),
      "divide"   = data.frame(loc_divide[[loop_bit]]),
      "digit"    = data.frame(loc_digits[[loop_bit]]),
      "p"        = data.frame(loc_prob[[loop_bit]]),
      "point"    = data.frame(loc_point[[loop_bit]]),
      "open"     = data.frame(loc_open[[loop_bit]]),
      "closed"   = data.frame(loc_closed[[loop_bit]])
    )
    pos       <- pos[order(pos[, 1]), ]
    pos       <- data.frame(t(pos))
    info_type <- colnames(pos)
    info_type <- str_replace_all(info_type, "[.]", "")
    info_type <- str_replace_all(info_type, "[:digit:]", "")

    ineq_equ_parts_loc <- which(info_type %in% c("above", "below", "equal"))
    ineq_equ_parts_loc <- c(1L, ineq_equ_parts_loc, length(info_type))

    loc_subparts <- numeric()
    for (loop_loc in seq_len(length(ineq_equ_parts_loc) - 2)) {
      loc_act <- ineq_equ_parts_loc[c(1, 3) + (loop_loc - 1)]
      if (loop_loc == 1) loc_act[2] <- loc_act[2] - 1L
      if (loop_loc < (length(ineq_equ_parts_loc) - 2) && loop_loc != 1) {
        loc_act[1] <- loc_act[1] + 1L
        loc_act[2] <- loc_act[2] - 1L
      }
      if (loop_loc == (length(ineq_equ_parts_loc) - 2)) {
        loc_act[1] <- loc_act[1] + 1L
      }
      loc_subparts <- c(loc_subparts, loc_act)
    }
    loc_subparts <- t(matrix(loc_subparts, nrow = 2))

    subparts <- list()
    if (nrow(loc_subparts) == 1) {
      subparts[[paste0("part", loop_loc)]] <-
        rbind(info_type[seq_along(info_type)],
              test_actual_single[seq_along(info_type)])
    } else {
      for (loop_loc in seq_len(nrow(loc_subparts))) {
        subparts[[paste0("part", loop_loc)]] <-
          rbind(info_type[loc_subparts[loop_loc, 1]:loc_subparts[loop_loc, 2]],
                test_actual_single[loc_subparts[loop_loc, 1]:loc_subparts[loop_loc, 2]])
      }
    }

    for (loop_sub_part in seq_along(subparts)) {
      subparts_actual <- data.frame(subparts[[loop_sub_part]])

      pos_p        <- which(subparts_actual[1, ] == "p")
      pos_before_p <- pos_p - 1L

      if (pos_before_p[1] == 0L) {
        subparts_actual <- cbind(c("add", "+"), c("digit", "1"), c("multiply", "*"), subparts_actual)
      }
      if (subparts_actual[1, 1] == "digit") {
        subparts_actual <- cbind(c("add", "+"), subparts_actual)
      }

      loc_after_rel <- which(subparts_actual[1, ] %in% c("below", "above", "equal")) + 1L
      if (subparts_actual[1, loc_after_rel] %in% c("digit", "point")) {
        subparts_actual <- cbind(
          subparts_actual[, seq_len(loc_after_rel - 1L)],
          c("add", "+"),
          subparts_actual[, loc_after_rel:ncol(subparts_actual)]
        )
      }

      add_multiply_fn <- function() {
        pp   <- which(subparts_actual[1, ] == "p")
        pbp  <- pp - 1L
        am   <- ifelse(subparts_actual[1, pbp] %in% c("add", "below", "above", "equal"), 1L,
                 ifelse(subparts_actual[1, pbp] == "subtract", -1L, 0L))
        am   <- ifelse(subparts_actual[1, pbp] %in% c("below", "above", "equal"), 2L, am)
        ff   <- pbp[am != 0L]
        am   <- am[am != 0L]
        list(ff, am)
      }

      res  <- add_multiply_fn()
      fix_factors  <- unlist(res[[1]])
      add_multiply <- unlist(res[[2]])

      if (length(add_multiply) > 0) {
        for (k in seq_along(add_multiply)) {
          if (add_multiply[1] %in% c(-1L, 1L)) {
            subparts_actual <- cbind(
              subparts_actual[, seq_len(fix_factors[1])],
              cbind(c("digit", "1"), c("multiply", "*")),
              subparts_actual[, (fix_factors[1] + 1L):ncol(subparts_actual)]
            )
          }
          if (add_multiply[1] == 2L) {
            subparts_actual <- cbind(
              subparts_actual[, seq_len(fix_factors[1])],
              cbind(c("add", "+"), c("digit", "1"), c("multiply", "*")),
              subparts_actual[, (fix_factors[1] + 1L):ncol(subparts_actual)]
            )
          }
          res  <- add_multiply_fn()
          fix_factors  <- unlist(res[[1]])
          add_multiply <- unlist(res[[2]])
        }
      }

      # extract p indices
      keep <- integer(ncol(subparts_actual))
      for (i in seq_len(ncol(subparts_actual))) {
        if (subparts_actual[1, i] == "p") { keep[i] <- 1L; next }
        if (i > 1 && subparts_actual[1, i] != "p" &&
            subparts_actual[1, i] == "digit" && keep[i - 1] == 1L) {
          keep[i] <- 1L
        }
      }
      extract_p <- subparts_actual[2, keep == 1L]
      extract_p <- paste0(extract_p, collapse = "")
      extract_p <- unlist(str_split(extract_p, "p"))
      extract_p <- as.numeric(extract_p[2:length(extract_p)])

      location_relation    <- which(subparts_actual[1, ] %in% c("below", "above", "equal"))
      relation_subpart     <- unlist(subparts_actual[1, location_relation])

      left_side  <- data.frame(subparts_actual[, seq_len(location_relation - 1L)])
      right_side <- data.frame(subparts_actual[, (location_relation + 1L):ncol(subparts_actual)])

      location_add_sub_left  <- which(left_side[1, ]  %in% c("subtract", "add"))
      location_p_left        <- which(left_side[1, ]  == "p")
      location_add_sub_right <- which(right_side[1, ] %in% c("subtract", "add"))
      location_p_right       <- which(right_side[1, ] == "p")

      factor_left <- character()
      if (length(location_p_left) == 0) {
        factor_left <- paste0(left_side[2, ], collapse = "")
        extract_p   <- c("numb", extract_p)
      } else {
        for (k in seq_along(location_add_sub_left)) {
          factor_left <- c(factor_left,
            paste0(left_side[2, location_add_sub_left[k]:(location_p_left[k] - 2L)], collapse = ""))
        }
      }

      factor_right <- character()
      if (length(location_p_right) == 0) {
        factor_right <- paste0(right_side[2, ], collapse = "")
        extract_p    <- c(extract_p, "numb")
      } else {
        for (k in seq_along(location_add_sub_right)) {
          factor_right <- c(factor_right,
            paste0(right_side[2, location_add_sub_right[k]:(location_p_right[k] - 2L)], collapse = ""))
        }
      }

      extracted <- rbind(
        extract_p,
        c(rep("left", length(factor_left)), rep("right", length(factor_right))),
        c(factor_left, factor_right),
        relation_subpart
      )
      rownames(extracted) <- c("p", "left/right", "factor", "relation")
      colnames(extracted) <- paste0("sub_", seq_len(ncol(extracted)))
      all_extracted[[paste0("line", loop_all)]] <- data.frame(extracted)
      loop_all <- loop_all + 1L
    }
  }

  if (length(all_extracted) == 0) {
    return(list(
      ineq_eq_left  = matrix(character(0), 0, n_p),
      ineq_eq_right = character(0),
      all_operators = character(0)
    ))
  }

  numb_p_found <- max(as.numeric(
    unlist(lapply(all_extracted, function(x) {
      vals <- unlist(x["p", ])
      vals[vals != "numb"]
    }))
  ))
  n_p <- max(n_p, numb_p_found)

  numb_equl_ineq <- length(all_extracted)
  ineq_eq_left   <- matrix(NA_character_, nrow = numb_equl_ineq, ncol = n_p)
  ineq_eq_right  <- rep(NA_character_, numb_equl_ineq)
  all_operators  <- rep(NA_character_, numb_equl_ineq)

  for (loop_rel in seq_along(all_extracted)) {
    ar <- data.frame(all_extracted[[loop_rel]])

    extract_factors <- numeric(ncol(ar))
    for (k in seq_len(ncol(ar))) {
      extract_factors[k] <- eval(parse(text = ar[3, k]))
    }
    ar[3, ] <- extract_factors

    ar_left  <- data.frame(ar[, ar[2, ] == "left"])
    ar_right <- data.frame(ar[, ar[2, ] == "right"])
    op       <- unlist(ar[4, 1])

    numb_left      <- which(ar_left[1, ]  == "numb")
    numb_right     <- which(ar_right[1, ] == "numb")
    non_numb_left  <- which(ar_left[1, ]  != "numb")
    non_numb_right <- which(ar_right[1, ] != "numb")

    if (op == "equal" && length(non_numb_right) > 0 && length(non_numb_left) > 0) {
      ar_right[3, non_numb_right] <- -1 * as.numeric(ar_right[3, non_numb_right])
    }
    if (op == "below" && length(non_numb_right) > 0) {
      ar_right[3, non_numb_right] <- -1 * as.numeric(ar_right[3, non_numb_right])
      ar_left[3, numb_left]       <- -1 * as.numeric(ar_left[3, numb_left])
    }
    if (op == "above" && length(non_numb_left) > 0) {
      ar_left[3, non_numb_left]   <- -1 * as.numeric(ar_left[3, non_numb_left])
      ar_right[3, numb_right]     <- -1 * as.numeric(ar_right[3, numb_right])
      op <- "below"
    }

    ar_left[3, ]  <- d2q(as.numeric(ar_left[3, ]))
    ar_right[3, ] <- d2q(as.numeric(ar_right[3, ]))

    total        <- data.frame(ar_left, ar_right)
    total_left   <- data.frame(total[, total[1, ] != "numb"])
    total_right  <- data.frame(total[, total[1, ] == "numb"])

    if (length(total_right) == 0) {
      total_right <- t(data.frame("numb", "right", "0", op))
    }

    ineq_eq_left[loop_rel, as.numeric(unlist(total_left[1, ]))]   <- unlist(total_left[3, ])
    ineq_eq_left[loop_rel, -as.numeric(unlist(total_left[1, ]))]  <- "0"
    ineq_eq_right[loop_rel] <- unlist(total_right[3, ])
    all_operators[loop_rel] <- op
  }

  # ---- approximate equalities ----
  # approx_tol: one number for all equalities, or one number per equality in the order they appear in the model
  # (0 keeps that equality exact).
  eq_idx <- which(all_operators == "equal")
  tol <- as.numeric(approx_tol)
  if (length(tol) > 1 && length(tol) != length(eq_idx) && length(eq_idx) > 0) {
    stop("approx_tol has ", length(tol), " values but the model has ", length(eq_idx), " equalities (one value per equality, or a single value for all).")
  }
  tol_each <- if (length(eq_idx) == 0) numeric(0) else if (length(tol) == 1) rep(tol, length(eq_idx)) else tol
  tol_each[is.na(tol_each)] <- 0
  if (any(tol_each != 0)) {
    add_left  <- matrix(character(0), 0, n_p)
    add_right <- character()
    add_ops   <- character()
    relaxed   <- logical(length(all_operators))

    for (m in seq_along(eq_idx)) {
      i <- eq_idx[m]
      if (tol_each[m] == 0) next
      relaxed[i] <- TRUE
      add_left  <- rbind(add_left,
        ineq_eq_left[i, ],
        d2q(-1 * q2d(ineq_eq_left[i, ]))
      )
      add_right <- c(add_right,
        d2q(q2d(ineq_eq_right[i])  + tol_each[m]),
        d2q(-q2d(ineq_eq_right[i]) + tol_each[m])
      )
      add_ops <- c(add_ops, "below", "below")
    }

    keep          <- !relaxed
    ineq_eq_left  <- rbind(ineq_eq_left[keep, , drop = FALSE], add_left)
    ineq_eq_right <- c(ineq_eq_right[keep], add_right)
    all_operators <- c(all_operators[keep], add_ops)
  }

  list(
    ineq_eq_left  = ineq_eq_left,
    ineq_eq_right = ineq_eq_right,
    all_operators = all_operators
  )
}
