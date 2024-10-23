# NOTE: forward simulation
forward_simulation <- function(case, df, additionalParameters, parameter, n = 100) {
  if (!(case %in% c("dba_dye_const", "dba_host_const", "ida", "gda"))) {
    stop("case is neither dba_dye_const, dba_host_const, ida or gda")
  }
  if (!is.data.frame(df)) {
    stop("df has to be of type data.frame")
  }
  if (!is.data.frame(parameter)) {
    stop("parameter has to be of type numeric")
  }
  if (!is.numeric(n) && !is.integer(n)) {
    stop("n has to be of type numeric")
  }
  if (n <= nrow(df)) {
    stop("n has to be greater than the number of rows in df")
  }
  if (n > 10000) {
    stop("n has to be smaller than 10000")
  }
  if (case == "hg" && length(additionalParameters) != 1) {
    stop("additionalParameters have to be of length 1")
  }
  if (case == "ida" && length(additionalParameters) != 3) {
    stop("additionalParameters have to be of length 3")
  }
  if (case == "gda" && length(additionalParameters) != 3) {
    stop("additionalParameters have to be of length 3")
  }
  lossFct <- tryCatch(
    expr = {
      if (case == "dba_host_const") {
        forward_dba_host_const
      } else if (case == "dba_dye_const") {
        forward_dba_dye_const
      } else if (case == "ida") {
        forward_ida
      } else if (case == "gda") {
        forward_gda
      }
    },
    error = function(e) {
      return(NULL)
    },
    interrupt = function(e) {
      return(NULL)
    }
  )
  var_new <- seq(min(df[, 1]), max(df[, 1]), length.out = n)
  params <- list()
  result <- NULL
  parameter <- parameter[1, ] |> as.numeric()
  if (case == "dba_host_const") {
    params[[1]] <- parameter[1] # KaHD
    params[[2]] <- parameter[4] # I(D)
    params[[3]] <- parameter[3] # I(HD)
    params[[4]] <- additionalParameters[1] # H0
    params[[5]] <- var_new # Dye
    result <- forward_dba_host_const(
      params[[1]], params[[2]],
      params[[3]], params[[4]], params[[5]]
    )
  } else if (case == "dba_dye_const") {
    params[[1]] <- parameter[1] # KaHD
    params[[2]] <- parameter[4] # I(D)
    params[[3]] <- parameter[3] # I(HD)
    params[[4]] <- additionalParameters[1] # D0
    params[[5]] <- var_new # Host
    result <- forward_dba_dye_const(
      params[[1]], params[[2]],
      params[[3]], params[[4]], params[[5]]
    )
  } else if (case == "ida") {
    params[[1]] <- parameter[1] # KaHG
    params[[2]] <- parameter[4] # I(D)
    params[[3]] <- parameter[3] # I(HD)
    params[[4]] <- additionalParameters[1] # H0
    params[[5]] <- additionalParameters[2] # D0
    params[[6]] <- additionalParameters[3] # KaHD
    params[[7]] <- var_new # Guest
    result <- forward_ida(
      params[[6]], params[[1]],
      params[[2]], params[[3]],
      params[[4]], params[[5]], params[[7]]
    )
  } else if (case == "gda") {
    params[[1]] <- parameter[1] # KaHD
    params[[2]] <- parameter[4] # I(D)
    params[[3]] <- parameter[3] # I(HD)
    params[[4]] <- additionalParameters[1] # H0
    params[[5]] <- additionalParameters[2] # G0
    params[[6]] <- additionalParameters[3] # KaHD
    params[[7]] <- var_new # Dye
    result <- forward_gda(
      params[[6]], params[[1]],
      params[[2]], params[[3]],
      params[[4]], params[[5]], params[[7]]
    )
  }
  return(result)
}
