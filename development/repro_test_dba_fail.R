library(tsf)

path <- system.file("extdata", "dba_dye_const_real.txt", package = "tsf")
df <- read.csv(path, header = FALSE, sep = "\t")
parameter <- c(3e3, 2.0, 1.65e7, 1.6e6)
envSetup <- new.env()
envSetup$d0 <- 5
envSetup$host <- df[, 1]
envSetup$signal <- df[, 2]
envSetup$n_sigs <- 1L
result <- tsf:::lossFctDBA(parameter, envSetup, TRUE)
df[, 2] <- result$insilico
file <- tempfile(fileext = ".txt")
write.csv(df, file, quote = FALSE, row.names = FALSE)

case <- "dba_dye_const"
lowerBounds <- c(1, 0, 1e2, 1e2)
upperBounds <- c(1e8, 1e4, 1e8, 1e8)
additionalParameters <- envSetup$d0
seed <- 1234
npop <- 40
ngen <- 100

df2 <- tsf:::importData(file)
n_sigs <- ncol(df2) - 1

env <- new.env()
env$error_calc_fct <- tsf:::get_error_calc_fct("Rel. Error")
env$n_sigs <- n_sigs
names(df2)[1] <- "host"
env$host <- df2[, 1]
env$signal <- df2[, -1] |> as.data.frame()
env$d0 <- additionalParameters[1]

spec <- tsf:::pso_a2a_spec(case)
add_params <- spec$build_add_params(df2, additionalParameters)
loss_particle_a2a <- ast2ast::translate(spec$loss_fct, args_f = spec$args_f, types_f = spec$types_f)
error_code <- tsf:::pso_error_code("Rel. Error")

# ---- instrumented copy of pso()'s init + first-generation logic, to find
# where a NaN/Inf/empty value first sneaks into swarm_bests / local_best ---
loss_fct <- function(parameter, env, eval = FALSE) {
  if (!isTRUE(eval)) {
    e <- loss_particle_a2a(parameter, add_params, error_code)
  } else {
    e <- tsf:::lossFctDBA(parameter, env, eval)
  }
  e
}

set.seed(seed)
npar <- length(lowerBounds)
lb <- lowerBounds
ub <- upperBounds
swarm <- matrix(0, nrow = npop, ncol = npar)
swarm_bests <- numeric(npop)
swarm_errors <- numeric(npop)

lb2 <- ifelse(lb <= 0, 10^-15, lb)
ub2 <- ifelse(ub <= 0, 10^-15, ub)
lb2 <- log(lb2)
ub2 <- log(ub2)
for (i in seq(npop)) {
  swarm[i, ] <- runif(npar, min = lb2, max = ub2)
  swarm_errors[i] <- loss_fct(exp(swarm[i, ]), env)
  swarm_bests[i] <- swarm_errors[i]
}
swarm <- exp(swarm)

cat("any NA in swarm_bests after init:", anyNA(swarm_bests), "\n")
cat("any Inf in swarm_bests after init:", any(is.infinite(swarm_bests)), "\n")
if (anyNA(swarm_bests)) {
  bad <- which(is.na(swarm_bests))
  cat("bad particle indices:", bad, "\n")
  print(swarm[bad, , drop = FALSE])
  cat("re-evaluating loss directly for bad particle(s):\n")
  for (bi in bad) {
    cat("particle", bi, "params:", swarm[bi, ], "\n")
    cat("  loss_particle_a2a ->", loss_particle_a2a(swarm[bi, ], add_params, error_code), "\n")
    cat("  lossFctDBA (R)    ->", tsf:::lossFctDBA(swarm[bi, ], env), "\n")
  }
}
