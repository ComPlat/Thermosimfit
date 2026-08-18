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

df2 <- tsf:::importData(file)
names(df2)[1] <- "host"
host <- df2[, 1]

# ---- same standalone compiled solve-only helper as before, now against the
# fixed ast2ast (Logical bool-conversion + &&/|| truth table) -------------
solve_only <- function(Kd, host, d0) {
  equation_h_dba <- fn(
    f_args = function(h, params) {
      h |> type(double)
      params |> type(EqParams)
    },
    return_value = type(double),
    block = function(h, params) {
      if (h <= 0) {
        return(1.797693e+308)
      }
      denom_Kd <- 1 + params$Kd * h
      h_d <- (params$Kd * h * params$d0) / denom_Kd
      return(h + h_d - params$h0)
    }
  )

  n <- length(host)
  out |> type(SolveOut)
  out$d <- numeric(n)
  out$hd <- numeric(n)
  out$root <- numeric(n)
  out$valid <- numeric(n)

  params |> type(EqParams)
  params$Kd <- Kd

  for (i in 1L:n) {
    h0_i <- host[i]
    if (h0_i <= 0) {
      h0_i <- 1e-15
    }
    params$h0 <- h0_i
    params$d0 <- d0
    ur <- uniroot(equation_h_dba, c(1e-20, h0_i), 1e-14, 200, params)
    out$root[i] <- ur$root
    if (is.na(ur$root)) {
      out$d[i] <- -1
      out$hd[i] <- -1
      out$valid[i] <- 0
    } else if (is.nan(ur$root)) {
      out$d[i] <- -1
      out$hd[i] <- -1
      out$valid[i] <- 0
    } else if (ur$root <= 0) {
      out$d[i] <- -1
      out$hd[i] <- -1
      out$valid[i] <- 0
    } else {
      d_sol <- d0 / (1 + Kd * ur$root)
      if (is.na(d_sol)) {
        out$d[i] <- -1
        out$hd[i] <- -1
        out$valid[i] <- 0
      } else if (is.nan(d_sol)) {
        out$d[i] <- -1
        out$hd[i] <- -1
        out$valid[i] <- 0
      } else if (d_sol < 0) {
        out$d[i] <- -1
        out$hd[i] <- -1
        out$valid[i] <- 0
      } else {
        out$d[i] <- d_sol
        out$hd[i] <- Kd * ur$root * d_sol
        out$valid[i] <- 1
      }
    }
  }
  return(out)
}
args_f_solve_only <- function(Kd, host, d0) {
  Kd |> type(double)
  host |> type(vec(double))
  d0 |> type(double)
}
types_f_solve_only <- function() {
  new_type(EqParams, slots(Kd |> type(double), h0 |> type(double), d0 |> type(double)))
  new_type(SolveOut, slots(
    d |> type(vec(double)), hd |> type(vec(double)),
    root |> type(vec(double)), valid |> type(vec(double))
  ))
}
solve_only_a2a <- ast2ast::translate(solve_only, args_f = args_f_solve_only, types_f = types_f_solve_only)

bad_kds <- c(
  7714771.52, 213142.51, 4474663.43, 26709.68, 547235.10, 8292216.95,
  25775304.88, 40034970.04, 36882892.17, 899948.63, 18422695.07,
  70192716.86, 65340.19
)

cat(sprintf("%-14s %-12s %-12s %-6s\n", "Kd", "R_row1_NA", "a2a_row1_NA", "AGREE"))
for (Kd in bad_kds) {
  r_sol <- tsf:::solve_h_dba(Kd, host, 5)
  r_na <- is.na(r_sol$d[1])
  a2a_sol <- solve_only_a2a(Kd, host, 5)
  a2a_na <- a2a_sol$valid[1] == 0
  cat(sprintf("%-14.2f %-12s %-12s %-6s\n", Kd, r_na, a2a_na, r_na == a2a_na))
}
