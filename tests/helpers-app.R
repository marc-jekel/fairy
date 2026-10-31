# Loads the pure helper functions of app.R (the ones that work on specification text) without starting the app.
# Each function is cut out of app.R by its name: "  name <- function(" up to the closing "  }" at the same indentation.
load_app_helpers <- function(app_file = file.path("..", "app.R"), env = new.env()) {
  src <- readLines(app_file, encoding = "UTF-8")
  names_wanted <- c("norm_ops", "chain_side_items", "chain_is_number", "pure_prob_chain", "chain_pair_clauses",
                    "eq_row_key", "dedupe_eq_rows", "spec_eq_rows", "expand_equality_chains",
                    "expand_clause_to_rows", "clause_expected_row_count", "align_tolerances")
  `%||%` <- function(a, b) if (is.null(a)) b else a
  assign("%||%", `%||%`, envir = env)
  for (nm in names_wanted) {
    start <- grep(paste0("^  ", nm, " <- function"), src)
    if (length(start) != 1) stop("function not found exactly once in app.R: ", nm)
    end <- start
    while (!grepl("^  }\\s*$", src[end])) end <- end + 1
    eval(parse(text = paste(src[start:end], collapse = "\n"), keep.source = FALSE), envir = env)
  }
  env
}
