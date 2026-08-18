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

# small ngen -- just need to trigger the crash, not a real fit
for (s in 1:200) {
  ok <- tryCatch({
    invisible(capture.output({
      tsf::opti(
        "dba_dye_const",
        c(1, 0, 1e2, 1e2), c(1e8, 1e4, 1e8, 1e8),
        file, envSetup$d0,
        npop = 40, ngen = 30,
        engine = "ast2ast",
        seed = s
      )
    }))
    TRUE
  }, error = function(e) {
    cat("seed", s, "FAILED:", conditionMessage(e), "\n")
    FALSE
  })
  if (!ok) {
    cat(">>> reproduced with seed =", s, "\n")
    break
  }
}
cat("done\n")
