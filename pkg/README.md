# fairy

**fairy** is an R package for formalizing verbal hypotheses about proportions as order-constrained models, computing their geometric representations, and measuring how parsimonious they are.

The package implements the framework described in:

> Jekel, M., Regenwetter, M., Chen, M., & Line, E. N. (in preparation). *"Wait, what are you saying, exactly?" A Theoretical Framework for Codifying and Evaluating Verbal Hypotheses about Proportions.*

and it reproduces the examples of that paper and of its online supplement (see the vignette). A companion Shiny app, "the mathematics fairy", is at <https://modeling-for-everyone.eu/posts/fairy/>.

## Installation

```r
# install.packages("remotes")
remotes::install_github("marc-jekel/fairy", subdir = "pkg", build_vignettes = TRUE)
```

The package lives in the folder `pkg` of this repository, next to the Shiny app. Building the vignette on install takes about a minute; rendered copies are in [`pkg/doc/cringe.html`](doc/cringe.html) and [`pkg/doc/cringe.pdf`](doc/cringe.pdf), no R required. Or from a local copy: `R CMD INSTALL pkg`. **Dependencies:** `rcdd`, `geometry`, `stringr`, `MASS`; `volesti` is optional (volume estimates in very high dimensions).

## What the package does

A verbal hypothesis about proportions, "category A is more common than B, which is more common than C", becomes a convex polytope in the unit cube of Binomial parameters. For such a model the package

1. **parses** a constraint string into the minimal list of equalities and inequalities (H-representation),
2. **lists the extreme cases** (V-representation), also in words ("all" / "none"),
3. **measures parsimony**: the volume of the model, and its inverse, the largest Bayes factor the model can reach against the unconstrained model,
4. **combines models**: intersections, mixtures, nesting, and a comparison table,
5. **builds replication models** for several labs (identical, joint, substitutable parameters),
6. **exports** input files for QTest and multinomineq.

## Quick start

```r
library(fairy)

# Hypothesis: p1 > p2 > p3
fairy("p1 > p2 > p3", n_p = 3)

fairy_constraints("p1 > p2 > p3")     # "- p1 + p2 <= 0" "- p2 + p3 <= 0" "p1 <= 1" "- p3 <= 0"
fairy_words("p1 > p2 > p3")           # extreme cases, in words
fairy_max_bf("p1 > p2 > p3")          # 6 = 1 / volume
fairy_info("p1 > p2 > p3")            # dimension, vertices, constraints, volume, max. BF
```

### Constraint syntax

Constraints use `p1`, `p2`, ... for the probabilities. `<` and `<=` (and `>`/`>=`, and the unicode symbols) mean the same thing: models are closed sets.

```r
"p1 > p2"                      # simple inequality
"p1 = p2"                      # equality (the model then has volume zero)
"p1 > p2 > p3 > p4"            # chains
"p1 > p2; p3 > p4"             # several constraints
"{p1, p2} > {p3, p4}"          # every p on the left above every p on the right
"2*p1 - p2 > 0.5"              # linear expressions
"(p1 + p4)/2 < (p2 + p5)/2"    # averages
```

`approx_tol = 0.05` relaxes every equality to "within 0.05".

## Combining and comparing models

```r
m <- cringe_models()                    # the ten models of the paper's running example
fairy_compare(m)                        # volumes, overlaps, nesting, max. Bayes factors (Table 3)
fairy_mixture(m$M4, m$M9)               # mixture model: 12 vertices, 64 constraints
fairy_intersect("p1 > p2", "p2 > p3", n_p = 3)
fairy_nested(m$M4, m$M3)                # TRUE

# Replication across three labs (Table 4)
fairy_info(fairy_replicate(m$M4, 3, "joint"))
```

## Reproducing the papers

`vignette("cringe", package = "fairy")` reproduces, step by step, the tables and numbers of the main text (Tables 1-4, Figures 2 and 3) and of the online supplement on the Heuristic-Systematic Model (Tables A1-A4, the replication bounds and the mixture model). `cringe_models()` and `hsm_models()` contain the models. The tests in `tests/testthat/test-paper-*.R` check every reproduced number.

## Parsimony is a volume

The parsimony of a model is its volume inside the unit hypercube, so the share of all families of Binomial parameters the model allows. `1 / volume` is the largest Bayes factor in favor of the model against the unconstrained model (uniform prior, unbounded sample size). It is `Inf` for models with an equality constraint. Volumes are exact (convex hull) up to about nine dimensions; above that the package uses `volesti` if installed, otherwise Monte Carlo. Joint replication models use the product formula.

## Export

```r
fairy_write_qtest(c(M4 = "p1 > p2 > p3"), n_p = 3, file = "models.tar")
fairy_write_multinomineq(c(M4 = "p1 > p2 > p3"), n_p = 3, file = "models.tar")
```
