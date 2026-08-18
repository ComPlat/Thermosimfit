DfRepCombi <- R6::R6Class(
  "DfRepCombi",
  public = list(
    df = NULL,
    rep = NULL,
    initialize = function(message) {
      m <- strsplit(message, ";")[[1]]
      df_m <- gsub("Dataset = ", "", m[[1]]) |> as.numeric()
      rep_m <- gsub("Replicate = ", "", m[[2]]) |> as.numeric()
      self$df <- df_m
      self$rep <- rep_m
    }
  )
)
