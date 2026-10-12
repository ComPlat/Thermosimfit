# VAPRO TODO

## I. Adapt loss functions

- [ ] rewrite `lossFctDBAVapro`/`lossFctHGVapro`/`lossFctIDAVapro`/`lossFctGDAVapro` (`lossFunctionsVapro.R`) to use the single-root forward formulation (`equation_h_dba`/`equation_h_ida_gda` from `DerivedForwardEquations.R`) instead of the current two-polynomial-root `uniroot.all` approach
  - single unknown (free host `h`), single bracket, monotonic -> no more multi-root ambiguity
  - keep the `(parameter, env, eval)` / NNLS-profiling interface unchanged so `OptimizeVapro.R`/`BootstrappingVapro.R` keep working without changes

## II. tinytests

- [x] II.1 generate synthetic data with known ground-truth parameters using the existing `forward_dba_dye_const`/`forward_dba_host_const`/`forward_ida`/`forward_gda` simulators (`ForwardLossFunctions.R`) — scripts: `VAPRO/simulate_data_{dba,hg,ida,gda}.R`, output in `VAPRO/simulated_data/` (CSV + ground-truth `.rds` per case)
  - found while writing this: `forward_ida` silently drops any guest concentration where the inner root-finding fails (`next` inside the loop) and returns only `Signal_values + I0` without the surviving `g0` values — if any point is dropped there is no way to tell which one, since the returned vector no longer aligns with the input `g0_values`. Worked around for now by picking a `guestConcentrations` range where nothing gets dropped, guarded by an explicit `stopifnot(length(...) == length(...))` in `simulate_data_ida.R`. Should be fixed properly (return the surviving `g0` alongside the signal, like the other three `forward_*` functions do) before this is relied on more broadly.
- [ ] II.2 run optimizations (`opti_vapro` and `opti`/PSO) on the synthetic data and assert the recovered parameters match the known ground truth
- [ ] II.3 run uncertainty calculations (`opti_vapro_bootstrap`) on the synthetic data with injected known-sigma noise and assert the bootstrap CI covers the ground-truth binding parameter

## III. UI

- [ ] add VAPRO (`opti_vapro`/`opti_vapro_bootstrap`) to the Shiny app (currently only `opti`/PSO is wired into `DBA_UI.R`/`HG_UI.R`/`IDA_UI.R`/`GDA_UI.R` + `server.R`)
