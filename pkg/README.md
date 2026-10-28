# fairy

**fairy** is an R package for formalizing verbal hypotheses about proportions as order-constrained models, computing their geometric representations, and measuring how parsimonious they are.

The package implements the framework described in:

> Jekel, M., Regenwetter, M., Chen, M., & Line, E. N. (in preparation). *"Wait, what are you saying, exactly?" A Theoretical Framework for Codifying and Evaluating Verbal Hypotheses about Proportions.*

and it reproduces the examples of that paper and of its online supplement (see the vignette). A companion Shiny app, "the mathematics fairy", is at <https://modeling-for-everyone.eu/posts/fairy/>.

## Installation

```r
# install.packages("remotes")
remotes::install_github("marc-jekel/fairy", subdir = "pkg", upgrade = "always", build_vignettes = TRUE)
```

The package lives in the folder `pkg` of this repository, next to the Shiny app. Building the vignettes on install takes about a minute; rendered copies are in `pkg/doc/` (HTML and PDF), no R required. Or from a local copy: `R CMD INSTALL pkg`. **Dependencies:** `rcdd`, `geometry`, `stringr`, `MASS`; `volesti` is optional (volume estimates in very high dimensions).

## What the package does

A verbal hypothesis about proportions, "category A is more common than B, which is more common than C", becomes a convex polytope in the unit cube of Binomial parameters. For such a model the package

1. **parses** a constraint string into the minimal list of equalities and inequalities (H-representation),
2. **lists the extreme cases** (V-representation), also in words ("all" / "none"),
3. **measures parsimony**: the volume of the model, and its inverse, the largest Bayes factor the model can reach against the unconstrained model,
4. **combines models**: intersections, mixtures, nesting, and a comparison table,
5. **builds replication models** for several labs (identical, joint, substitutable parameters),
6. **exports** input files for QTEST and multinomineq.

## Documentation

- **Overview of all functions:** `vignette("fairy")` ([HTML](doc/fairy.html), [PDF](doc/fairy.pdf)), with the idea behind the package, the constraint syntax and every function with a short example.
- **Reproducing the papers:** `vignette("paper-examples")` ([HTML](doc/paper-examples.html), [PDF](doc/paper-examples.pdf)).
- **Help pages:** `help(package = "fairy")`, or `?fairy_volume` for a single function.

## Functions

| Function | What it does |
|:--|:--|
| `fairy()` | constraints, extreme cases, volume and max. Bayes factor for one or several models |
| `fairy_h()` | non-redundant H-representation of a model string |
| `fairy_v()` | V-representation in rcdd format |
| `fairy_from_vertices()` | H-representation from a table of vertices (V to H) |
| `fairy_constraints()` | H-representation as readable text lines |
| `fairy_vertices()` | extreme cases as a numeric matrix |
| `fairy_words()` | extreme cases in words ("all" / "none" / fractions) |
| `fairy_dim()` | dimension of a model |
| `fairy_volume()` | parsimony: volume in the unit cube (exact, volesti, or Monte Carlo) |
| `fairy_max_bf()` | largest possible Bayes factor against the unconstrained model (1 / volume) |
| `fairy_info()` | dimension, vertices, constraints, volume, max. Bayes factor in one row |
| `fairy_parsimony()` | older interface to the volume |
| `fairy_intersect()` | intersection of models |
| `fairy_mixture()` | mixture model (convex hull of the components) |
| `fairy_nested()` | is one model nested in another? |
| `fairy_compare()` | comparison table of a set of models |
| `fairy_replicate()` | replication across labs: identical, joint, substitutable |
| `fairy_unpack()` | unpacking across items: lists, joint / substitutable / identical (with tolerance) / average |
| `fairy_write_qtest()`, `fairy_write_multinomineq()` | write input files for QTEST and multinomineq |

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
m <- list(M4 = fairy_h("p1 > p2 > p3; p4 < p5 < p6; p1 < p4; p2 < p5; p3 < p6", n_p = 6),
          M9 = fairy_h("p1 < p2 < p3; p4 > p5 > p6; p1 > p4; p2 > p5; p3 > p6", n_p = 6))
fairy_compare(m)                        # volumes, overlaps, nesting, max. Bayes factors
fairy_mixture(m$M4, m$M9)               # mixture model: 12 vertices, 64 constraints
fairy_intersect("p1 > p2", "p2 > p3", n_p = 3)
fairy_nested(m$M4, m$M3)                # TRUE

# Replication across three labs (Table 4)
fairy_info(fairy_replicate(m$M4, 3, "joint"))
```

## Reproducing the papers

`vignette("paper-examples", package = "fairy")` reproduces, step by step, the tables and numbers of the main text (Tables 1-4, Figures 2 and 3) and of the online supplement on the Heuristic-Systematic Model (Tables A1-A4, the replication bounds and the mixture model). The models are entered as constraint strings in the vignette; the package stores none of them. The tests in `tests/testthat/test-paper-*.R` check every reproduced number.

## Parsimony is a volume

The parsimony of a model is its volume inside the unit hypercube, so the share of all families of Binomial parameters the model allows. A smaller volume means a more restrictive, more precise model. `1 / volume` is the largest Bayes factor in favor of the model against the unconstrained model (uniform prior, unbounded sample size). It is `Inf` for models with an equality constraint. By default the volume is estimated with `volesti` and its own default settings (further `volesti::volume()` options can be passed via `...`); `method = "exact"` (convex hull) and `method = "mc"` are available on request. The package never switches method automatically. Joint replication models use the product formula.

## Export

```r
fairy_write_qtest(c(M4 = "p1 > p2 > p3"), n_p = 3, file = "models.tar")
fairy_write_multinomineq(c(M4 = "p1 > p2 > p3"), n_p = 3, file = "models.tar")
```
