# ast2ast TODO (VAPRO acceleration)

Goal: compile the VAPRO loss functions (see `VAPRO/TODO.md` item I, forward
formulation) via `ast2ast::translate()`. Blockers: no `uniroot`/`nnls`
equivalents exist in `ast2ast` yet. `solve`/`backsolve`/`forwardsolve`/`chol`
already exist and reuse R's own LAPACK/BLAS.

## I. `uniroot` primitive

- [ ] API: inner function signature `f: double -> double` (single scalar arg, single scalar return), plus `lower`/`upper` bracket bounds
- [x] simplification confirmed: with the forward-equation formulation (mass balance on free host `h`, monotonic in `h`) only a single root in a monotonic bracket is ever needed — no multi-root/`uniroot.all`-style primitive required
- [ ] decide: reuse R's internal `R_zeroin2` (Brent's method, same algorithm behind R's own `uniroot()`), consistent with how `solve` reuses LAPACK/BLAS, instead of reimplementing bisection/Brent from scratch
- [ ] return value: scalar double root only (no convergence metadata), consistent with the rest of the API
- [ ] new capability needed: support passing an inner function as an ARGUMENT to a builtin (currently unsupported)
  - [ ] `cmr`'s `infer_fct` (`R/FunctionRegistry.R`) explicitly rejects `fn_node` argument types — needs to allow a function-typed argument for `uniroot`
  - [ ] type inference: a bare symbol referencing an inner function currently becomes a `variable_node` that structurally carries the underlying `fn_node`, but every consumer (`common_type()`, reassignment checks) rejects it today — needs an explicit "function-typed value" path
  - [ ] codegen: translate the referenced inner function into a C++ callable and pass it into a templated `etr::uniroot<F>(F f, double lower, double upper, ...)`, reusing the templated-callable pattern already used internally for AD (`BoundCall`/`jacobian_forward` in `inst/include/etr_bits/Derivatives.hpp`)

## II. `nnls` primitive

- [ ] design API and return shape (coefficient vector only, or also residual/iteration count?)
- [ ] no direct 1:1 LAPACK routine for this (unlike `solve`/`chol`/`backsolve`/`forwardsolve`) — implement Lawson-Hanson active-set algorithm, using repeated `solve()` calls on the passive-set submatrix as the inner building block
- [ ] register in `R/FunctionRegistry.R` with type inference/checking analogous to `solve`

## III. Move the grid/bootstrap loop itself into C++ (and parallelize it)

Two stacked wins, not one: (a) compiling the loss function itself (I+II) removes
the deeply nested R-overhead inside root-finding (uniroot.all called per data
point, itself calling an R closure ~20-50x per call) — likely the bigger win;
(b) moving the outer grid/bootstrap loop into C++ removes ~200k R-call
boundary crossings (nGrid x nBoot) *and*, more importantly, makes cheap
thread-level parallelism possible.

- [ ] once the loss function is an `ast2ast` external pointer, write the grid search (and ideally the whole bootstrap replicate loop) so it calls that pointer directly without returning to the R interpreter in between
- [ ] Konrad is in favor of writing the optimizer/loop logic itself as ast2ast-translated R too, not hand-written C++ — see `paropt/src/optimizer.cpp` (current PSO implementation) as the counter-example of what he does *not* want to keep doing: the reason is maintainability by future contributors who don't know C++, not just performance (see memory: ast2ast maintainability philosophy)
- [ ] parallelization precedent to reuse/adapt: `paropt`'s `wrapper_optimizer` (`paropt/src/optimizer.cpp:81-82,240-246`) uses `RcppThread::ThreadPool pool(number_threads)` with `pool.pushReturn(objfct, ...)` per task and `std::future<double>` collected via `.get()` — a plain fork-join pattern over the population dimension (there: `nswarm` particles per generation; for us: either `nGrid` grid points within one grid search, or — probably the better granularity, less synchronization overhead — the `nBoot` bootstrap replicates, since each is a fully independent grid-search-plus-refit task)
- [ ] the thread-pool harness itself (generic infrastructure, rarely changes, no domain logic) can stay a thin hand-written C++ wrapper like `paropt`'s; only the per-task logic it dispatches (one grid-point evaluation, or one full bootstrap replicate fit) needs to be the ast2ast-compiled/maintainable piece
- [ ] benchmark against the current pure-R implementation once wired up
