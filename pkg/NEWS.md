# fairy 0.6.5

* `results.html` of the multinomineq analysis: the labels of the probabilities of unpacked models (p with a stacked superscript and subscript) are no
  longer stretched like the coloured number cells; tables whose first column holds the numbers are centred in every column.

# fairy 0.6.4

* `results.html` of the multinomineq analysis: the tables use the full width (in line with the scale of the evidence categories) and the
  trace and density plots are centred.

# fairy 0.6.3

* `results.html` of the multinomineq analysis: every second row of the tables is lightly shaded, so rows are easier to tell apart.
* The note for a model with an estimate of 0 now says that more samples may give a number.

# fairy 0.6.2

* `results.html` of the multinomineq analysis: the number columns of the tables (from the second column on) are centred; only the model name
  stays on the left.

# fairy 0.6.1

* The analysis script of `fairy_write_multinomineq()` / `fairy_multinomineq_script()` and `fairy_analyze_multinomineq()` now also writes
  `results.pdf`: the settings, the Bayes factors of all models (coloured by the strength of evidence, after Jeffreys, 1961) and, for every
  model, the posterior (mean, SD, 95% interval, effective sample size, Geweke z) with trace and density plots.
* `results.html`: the legend of the evidence categories is one coloured scale (from "< 1/100" to "> 100"); the column "Representation" is
  left out when only the H-representation was analysed, which is what the app and the package do.
* The analysis from the V-representation (`V_<model>.csv`) is still prepared in the script, but switched off (commented out, with a note on how
  to switch it on); the files of the package contain the H-representation (`A_`, `b_`) only.
* Help of the `prior` argument shortened.

# fairy 0.6.0

* `fairy_volume(method = "auto")`: the exact volume first, as in the app, and the volesti estimate if the exact volume cannot be calculated.
  Optional `time_limit` (seconds, needs callr): the estimate replaces the exact volume after that time.
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
