# App alignment TODO (tsf ↔ OpenStats ↔ OpenDOE)

Direction: OpenStats is the most mature of the three apps (tsf, OpenStats,
OpenDOE). tsf moves toward OpenStats's conventions in most respects.
OpenDOE already closely resembles OpenStats. Exception: UI framework (see
point 4) - leaning is to keep OpenStats/OpenDOE's existing look and move tsf
toward it, not the reverse, but that's a real visual redesign either way and
needs an explicit go-ahead before starting, unlike points 1-3 which are
lower-risk internal cleanup.

## 1. Drop `NS()`/`ns()` for plain string concatenation

- Both apps use `NS(id, "x")` in places. OpenStats additionally has many
  hardcoded `"MODULE-x"` string literals, inconsistently mixed with `NS()`
  in the same files (e.g. `Server_StatisticalTests.R` uses both
  `NS(id, "aovTest")` and literal `"TESTS-pairwise_test"`).
- `NS(id, "x")` with default settings *is* exactly `paste0(id, "-", x)` -
  `moduleServer()`'s auto-namespacing only needs the DOM id to have the
  `id-x` prefix, it doesn't care how that string was constructed - so
  replacing `NS()` calls with plain string concatenation is a
  behavior-identical rename, not a functional change.
- Apply consistently in both apps: drop `NS()`/`ns()` everywhere, use plain
  `paste0(id, "-", "x")` (or a small helper) instead.

## 2. Port tsf's `ErrorClass` (R6) into OpenStats

- OpenStats currently has ~4 different error-handling idioms coexisting:
  `try()` + `inherits(..., "try-error")`, `tryCatch()`, a narrowly-scoped
  `errorClass` (lowercase) used only inside the LC50 backend
  (`Backend_LC50.R`), and a "validation function returns `NULL` or a
  message string" convention in `Server_CheckFunctions.R`.
- tsf's app-wide `ErrorClass` R6 (explicit typed error-return value,
  checked via `inherits(x, "ErrorClass")`) is the cleaner, more consistent
  pattern of the two apps - port it into OpenStats and consolidate the
  existing idioms onto it.

## 3. Port OpenStats's `bg_process_V1_2` R6 pattern into tsf

- tsf has grown (and is about to grow a third copy of, for VAPRO-Batch) a
  hand-rolled per-feature `reactiveVal` soup for background `callr`
  processes (`process`/`setup_done`/`opti_result_created`/`invalid_time`,
  duplicated for PSO and again for VAPRO), each with its own
  `invalidateLater` poll loop.
- OpenStats centralizes this into one reusable R6
  (`bg_process_V1_2`: `$start()`/`$tick()`/`$cancel()`/`running_status`),
  used by every feature, with one global poll loop instead of one per
  feature.
- Port this pattern into tsf (a shared `BgProcess`-style R6) and migrate
  the existing PSO/VAPRO optimization code (and the upcoming VAPRO-Batch
  code) onto it, removing the duplicated `reactiveVal` scaffolding.

## 4. UI framework

- tsf currently uses shinydashboard (`box()`/`tabBox()`/`tabItems`).
  OpenStats deliberately uses plain `fluidPage()` + `sidebarLayout()` +
  `conditionalPanel()` + custom CSS (`.boxed-output` bordered divs) -
  originally because shinydashboard didn't work for a serverless
  GitHub Pages deployment of OpenStats. That static deployment was later
  removed after adding `callr` (which needs a real R backend, incompatible
  with static hosting), but the reduced UI was kept.
- Users are used to OpenStats/OpenDOE's current UI; visual polish isn't a
  priority for the developer. Leaning: keep OpenStats/OpenDOE's UI as-is
  and move tsf toward that style (drop shinydashboard) rather than the
  reverse - but confirm explicitly before starting, since it's a genuine
  visual redesign of tsf, not internal-only cleanup like points 1-3.
