# Quantity	Value
# conc(H)	1.00E-06
# conc(D)	1.00E-06
# conc(G)	1/200000
# Kequ(HD)	3.00E+06
# Kequ(HG)	2.00E+07
# Signal-0	0
# Signal-HD	1.00E+06
# Signal-Dye	2.00E+05
setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA")
library(tsf)

# Test several opti calls in parallel
path <- "/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/IDA/idaBatch.csv"
list_df <- tsf:::importDataBatch(path)
seeds <- 1:length(list_df)
messages <- paste0(1:length(list_df))
lowerBounds = c(
  kG = 1000,
  I0 = 0,
  IHD = 0,
  ID = 0
)
upperBounds = c(
  kG = 10^8,
  I0 = 100, # started at 10^7 but it ended always at 0...
  IHD = 10^7,
  ID = 10^7
)
additionalParameters = c(
  host = 1e-6,
  dye = 1e-6,
  kHD = 3e6
)

call_several_opti_in_bg <- function(case, lb, ub, df_list, ap,
                                    seed_list, npop, ngen, topo,
                                    et, messages) {
  process <- callr::r_bg(
    function(case, lb, ub, df_list, ap,
             seed_list, npop, ngen, topo,
             et, messages) {
      env <- new.env()
      env$intermediate_results <- lapply(seq_len(length((df_list))),
        function(x) x)

      for (i in seq_len(length(df_list))) {
        tryCatch(
          expr = {
            df <- df_list[[i]]
            seed <- seed_list[[i]]
            m <- messages[[i]]
            result <- tsf::opti(
              case, lb, ub, df, ap, seed, npop, ngen,
              topo, et, m
            )
            env$intermediate_results[[i]] <- result
            return(env$intermediate_results)
          },
          interrupt = function(e) {
            warning("interrupted!")
            return(env$intermediate_results)
          },
          error = function(e) {
            warning("\n\n Probably not finished optimisation \n\n")
            return(env$intermediate_results)
          }
        )
      }
    },
    args = list(
      case, lb, ub, df_list,
      ap, seed_list, npop, ngen, topo,
      et, messages
    )
  )
  return(process)
}

res <- call_several_opti_in_bg(
  case = "ida",
  lb = lowerBounds,
  ub = upperBounds,
  df_list = list_df,
  ap = additionalParameters,
  seed_list = seeds,
  npop = 40,
  ngen = 20,
  topo = "random",
  et = 0.7,
  messages = messages
)

counter <- 1
Sys.sleep(5)
while (TRUE) {
  if (!res$is_alive()) break
  cat(res$read_output())
  res$interrupt()
  res$wait()
}
cat("Counter ", counter, "\n")
print("Errors:")
print(res$read_all_error())
result <- res$get_result()
cat("Length results ", length(result), "\n")
trash <- lapply(result, function(x) print(class(x)))
result
