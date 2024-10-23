# NOTE: foreward simulation
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
        lossFctHG
      } else if (case == "dba_dye_const") {
        lossFctDBA
      } else if (case == "ida") {
        lossFctIDA
      } else if (case == "gda") {
        lossFctGDA
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
  spline_var <- spline(x = df[, 1], y = df[, 2], xout = var_new)$y
  df <- data.frame(var = var_new, signal = spline_var)
  env <- tryCatch(expr = {
    env <- new.env()
    if (case == "dba_host_const") {
      names(df) <- c("dye", "signal")
      env$dye <- df[, 1]
      env$signal <- df[, 2]
      env$h0 <- additionalParameters[1]
    } else if (case == "dba_dye_const") {
      names(df) <- c("host", "signal")
      env$host <- df[, 1]
      env$signal <- df[, 2]
      env$d0 <- additionalParameters[1]
    } else if (case == "ida") {
      names(df) <- c("guest", "signal")
      env$ga <- df[, 1]
      env$signal <- df[, 2]
      env$h0 <- additionalParameters[1]
      env$d0 <- additionalParameters[2]
      env$kd <- additionalParameters[3]
    } else if (case == "gda") {
      names(df) <- c("dye", "signal")
      env$dye <- df[, 1]
      env$signal <- df[, 2]
      env$h0 <- additionalParameters[1]
      env$ga0 <- additionalParameters[2]
      env$kd <- additionalParameters[3]
    }
    env
  }, error = function(e) {
    return(NULL)
  }, interrupt = function(e) {
    return(NULL)
  })
  parameter <- parameter[1, ] |> as.numeric()
  res <- lossFct(parameter, env, eval = TRUE)
  create_data_df(df, list(res), case)
}
