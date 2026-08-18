# Sensitivity tab - follow-up issues

Found while testing the Task-based rewrite of the Sensitivity tab interactively.

## 1. Progress percentages get concatenated

Live progress polling (`serverOptiSensiBatch.R`, the Sensitivity `observe({...})`
block) currently strips newlines from the raw text read since the last poll
and shows it as-is. Since ast2ast made `sensitivity()` fast, several
`print(progress)` calls can land between two polls, so e.g. "21" and "23"
become "2123%" instead of showing just the latest value.

Fix: same pattern already used for VAPRO-Batch's progress bar - parse all
numbers out of the new chunk (`as.integer(lines)`, drop `NA`s) and take the
last/max one, instead of concatenating the raw text.

## 2. Show a plot instead of a table

`output$sensi_table` (`renderDT`) currently renders `sensi_result()` (the
data.frame `tsf::sensitivity()` returns: columns `original, bias,
std. error, min. c.i., max. c.i.`, one row per Sobol term) directly as a
table. Should be a plot instead - a point + errorbar ggplot is a natural
fit: x = rownames(result) (Sobol term), y = original, ymin = min. c.i.,
ymax = max. c.i.

Important: `tsf::sensitivity()`'s own return value (the data.frame) is a
documented, tested public API (see `inst/tinytest/test_sensitivity.R`) -
do NOT change what the exported function returns. Build the plot in the
Shiny layer only, consuming that data.frame as-is.

Open question to resolve with the user before implementing: does
`output$sensi_download` (currently a CSV of the raw data.frame) also need
to change to export the plot/image, or does it stay a CSV export of the
underlying numbers alongside the new plot view?

## 3. Let Sensitivity use a VAPRO result, not just PSO

Currently gated on `opti_result_created()` (PSO single-run tab) only, and
`get_opti_result()` reads `opti_result()$parameter`. Should also accept a
completed VAPRO single-run (`vapro_opti_result_created()` /
`vapro_opti_result()$parameter`) as the source of the already-optimized
parameters to perturb.

Was in the middle of checking whether PSO's and VAPRO's `parameter`
data.frames are shape-compatible for `tsf::sensitivity()` (both are built
via `create_params_df(res, case, n_sigs)` - see `optimize.R:255,277` and
`OptimizeVapro.R:225`, implementation in `Utils.R:203`) - looked consistent
so far but wasn't confirmed end-to-end. Still need to design: how the UI
lets the user pick which source (PSO vs VAPRO) to use when both are
available, and update `check_inputs_sensi()`/`get_opti_result()`
accordingly.
