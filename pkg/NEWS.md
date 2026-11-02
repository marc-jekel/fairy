# fairy 0.6.0

* `fairy_volume(method = "auto")`: the exact volume first, stopped after `time_limit` seconds (default 300, needs callr), then the volesti
  estimate; `$method` says which was used. This is what the app does. The default stays `"volesti"`.
* `fairy_write_multinomineq()` also writes your data (`data`), the settings (`settings`) and the analysis script `analysis_multinomineq.R`
  (`analysis = TRUE`, as the app); the numbers stay with the probability when `order` changes.
* New: `fairy_multinomineq_script()` (text of the analysis script) and `fairy_analyze_multinomineq()` (runs the analysis from R and returns
  `results`, `posterior` and the path of `results.html`).
* New: `fairy_compare_vertices()`: which extreme cases do models share, which are unique to a model (the Compare buttons of the app).
* Vignette: sections on the exact-first volume, on shared extreme cases, and on export (order, equalities, exact fractions, data and analysis).

# fairy 0.5.0

* Constants such as 1/3, 0.1 and 0.0001 stay exact fractions up to rcdd; boundary cases have volume exactly 0.
* Equalities between probabilities in the multinomineq files (`map_<model>.csv`) and in the QTEST files (extra block with a note);
  `order` for the order of the probabilities.
