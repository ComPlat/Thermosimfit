library(ggplot2)
path <- paste0(system.file("examples", package = "tsf"), "/IDABatch.csv")
lowerBounds <- c(
  kG = 1000,
  I0 = 0,
  IHD = 0,
  ID = 0
)
upperBounds <- c(
  kG = 10^8,
  I0 = 100,
  IHD = 10^7,
  ID = 10^7
)
additionalParameters <- c(
  host = 1e-6,
  dye = 1e-6,
  kHD = 3e6
)
res <- tsf::batch(
  "ida",
  lowerBounds, upperBounds,
  path, additionalParameters,
  ngen = 100,
  num_cores = 6,
  num_rep = 30
)
params <- Reduce(rbind, res[[1]][[2]])
params
save(params, file = "./Tests/testingStuff/parameters.RData")

load("./Tests/testingStuff/parameters.RData")
datasets <- tsf:::importDataBatch(path)
env <- new.env()
env$h0 <- 1e-6
env$d0 <- 1e-6
env$ga <- datasets[[1]][, 1]
env$signal <- datasets[[1]][, 2]
env$kd <- 3e6
env$error_calc_fct <- tsf:::rel_err

make_joint_sampler_kde <- function(df, lb, ub) {
  eps <- 1e-6
  p <- ncol(df)
  to_unit <- function(X) {
    U <- sweep(X, 2, lb, "-")
    U <- sweep(U, 2, (ub - lb), "/")
    pmin(pmax(U, eps), 1 - eps)
  }
  to_param <- function(U) {
    U <- sweep(U, 2, (ub - lb), "*")
    sweep(U, 2, lb, "+")
  }
  U <- to_unit(as.matrix(df))
  Z <- qlogis(U)
  kde_obj <- ks::kde(Z)
  function(n) {
    Znew <- ks::rkde(n = n, fhat = kde_obj)
    Unew <- plogis(Znew)
    Xnew <- to_param(Unew)
    Xdf <- as.data.frame(Xnew)
    colnames(Xdf) <- colnames(df)
    Xdf
  }
}
sobolVariance_dep <- function(parameter_df, lossFct, env, lb, ub, parameterNames, runAsShiny) {
  n <- 10000 # For large values package RANN is required
  nboot <- 100

  joint_sampler <- make_joint_sampler_kde(parameter_df, lb, ub)
  X <- joint_sampler(n)
  names(X) <- parameterNames

  sobolFun <- function(X) {
    p <- NULL
    if (is.data.frame(X) || is.matrix(X)) {
      return(sapply(1:nrow(X), function(x) {
        lossFct(as.numeric(X[x, ]), env, FALSE)
      }))
    } else {
      p <- as.numeric(X)
    }
    lossFct(p, env, FALSE)
  }
  sh <- sensitivity::shapleysobol_knn(model = sobolFun, X = X, nboot = nboot)
  df <- data.frame(sh$conf_int); df$x <- row.names(df)
  p <- ggplot(data = df, aes(x = x, y = original)) +
    geom_point() +
    geom_errorbar(aes(ymin = min..c.i., ymax = max..c.i.)) +
    labs(
      x = NULL,
      y = "Explained fraction of variance (Shapley effects)"
    )
  list(sh, p)
}

set.seed(1234)
res_sens1 <- sobolVariance_dep(
  params[, 1:4], tsf:::lossFctIDA, env,
  lowerBounds, upperBounds,
  names(lowerBounds), FALSE
)
res_sens1[[1]]
res_sens1[[2]]

set.seed(1235)
res_sens2 <- sobolVariance_dep(
  params[, 1:4], tsf:::lossFctIDA, env,
  lowerBounds, upperBounds,
  names(lowerBounds), FALSE
)
res_sens2[[1]]
res_sens2[[2]]
