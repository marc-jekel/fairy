# Modeling Fairy App

A Shiny app for specifying, testing, and exporting probabilistic order-constrained
models — the kind of linear inequality/equality systems over choice probabilities
used with tools like [QTest](https://www.dwheck.de/software/multinomineq/) and
[multinomineq](https://github.com/danheck/multinomineq).

**Live app:** https://modeling-for-everyone.eu/posts/fairy/
(embeds the app directly from https://fairy.decision-research.de/)

> 🧚 The app is under active development. Found a bug? [Report it here](mailto:mjekel@uni-koeln.de?subject=bug-report%20fairy%20app).
> For computationally heavier models, running it locally (see below) is faster
> than the hosted version.

## What it does

- Define one or more models as linear constraints over named probabilities
  (`p1`, `p2`, ... or custom names), including equalities, shared constraints
  across models, and repeated/replicated items (joint, substitutable,
  identical, or averaged).
- Combine models via intersection or mixture, and compare them side by side.
- Compute and display each model's H-representation (inequality system) and
  V-representation (vertices), with Aligned/Compact layout and optional
  per-parameter coloring.
- Export models for downstream analysis:
  - **QTest** H-representation (`.txt`) — equalities are eliminated by
    substitution first, since QTest's format has no way to express `=`.
  - **multinomineq** H-representation (`A`/`b` CSV pairs) — equalities are
    kept as two inequality rows instead, preserving the full parameter space.
  - LaTeX (H- and V-representation).
- Run parsimony analysis (volume/dimensionality by algorithm) across models.
- Plot polytopes and edge cases.
- Upload/download the full app state as `.xlsx`.

## Running locally

Requires R and the packages the app loads at the top of `app.R` (installed
automatically via `install.packages()` on first run if missing). Then, from
this directory:

```r
shiny::runApp("app.R")
```

Or open `fairy.Rproj` in RStudio and click **Run App**.

## Project files

- `app.R` — the entire app (UI + server).
- `fairy.Rproj` — RStudio project file.
- `rsconnect/` — shinyapps.io deployment metadata (used by `rsconnect::deployApp()`).

## Contributing / bugs

This is a research tool under active development. Bug reports and feedback
are very welcome — email mjekel@uni-koeln.de or open an issue on this repo.
