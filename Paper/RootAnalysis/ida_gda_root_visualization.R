load("ida_gda_root_combined.RData")

# Find parameter combinations with more than one dye root
# =====================================
df <- result
df$interaction <- interaction(
  df$kga, df$kd, df$h0, df$d0, df$ga0
)

zero_cases <- function(df) {
  if (df[1, "kga"] <= 1e-15 || df[1, "kd"] <= 1e-15 ||
    df[1, "h0"] <= 1e-15 || df[1, "d0"] <= 1e-15 || df[1, "ga0"] <= 1e-15) {
    return(TRUE)
  }
  return(FALSE)
}

temp <- split(df, df$interaction)
df <- lapply(temp, function(x) {
  if (nrow(x) == 2) {
    diff <- abs(x[1, "d_root"] - x[2, "d_root"])
    if (diff > 10^-6) {
      return(x)
    }
    return(NULL)
  }
  if (nrow(x) > 1) {
    if (zero_cases(x)) {
      return(NULL)
    }

    return(x)
  }
  return(NULL)
})
df <- df[!sapply(df, is.null)]
print(df[[1]])
cat("\n\n\n")

# NOTE: d_root follows the order 0, value and
# all the hd roots are 0
# Than the second root is the correct one
d_zero_val_hd_zero_zero <- function(df) {
  check1 <- df[1, "d_root"] == 0
  check2 <- df[2, "d_root"] > df[1, "d_root"]
  check3 <- all(df[, "hd_root"] <= 1e-15)
  if (check1 && check2 && check3) {
    return(TRUE)
  }
  return(FALSE)
}
print(df[[1]])
cat("\n\n\n")

df <- lapply(df, function(x) {
  if (d_zero_val_hd_zero_zero(x)) {
    return(NULL)
  }
  return(x)
})
df <- df[!sapply(df, is.null)]
print(df[[1]])
cat("\n\n\n")

# NOTE: hd_root has only the same value
hd_root_same_val <- function(df) {
  if (sd(df[, "hd_root"]) == 0) {
    return(TRUE)
  }
  return(FALSE)
}

df <- lapply(df, function(x) {
  if (hd_root_same_val(x)) {
    return(NULL)
  }
  return(x)
})
df <- df[!sapply(df, is.null)]

head(df, n = 100)
length(df)





# Find parameter combinations with more than one host-dye root
# =====================================
df <- result
df$interaction <- interaction(
  df$kga, df$kd, df$h0, df$d0, df$ga0
)

df <- lapply(temp, function(x) {
  if (nrow(x) == 2) {
    diff <- abs(x[1, "hd_root"] - x[2, "hd_root"])
    if (diff > 10^-6) {
      return(x)
    }
    return(NULL)
  }
  if (nrow(x) > 1) {
    if (zero_cases(x)) {
      return(NULL)
    }

    return(x)
  }
  return(NULL)
})
df <- df[!sapply(df, is.null)]

# NOTE: hd_root follows the order 0, value and
# all the hd roots are 0
# Than the second root is the correct one
d_zero_val_hd_zero_zero <- function(df) {
  check1 <- df[1, "hd_root"] == 0
  check2 <- df[2, "hd_root"] > df[1, "hd_root"]
  check3 <- all(df[, "d_root"] <= 1e-15)
  if (check1 && check2 && check3) {
    return(TRUE)
  }
  return(FALSE)
}

df <- lapply(df, function(x) {
  if (d_zero_val_hd_zero_zero(x)) {
    return(NULL)
  }
  return(x)
})
df <- df[!sapply(df, is.null)]

# NOTE: d_root has only the same value
hd_root_same_val <- function(df) {
  if (sd(df[, "d_root"]) == 0) {
    return(TRUE)
  }
  return(FALSE)
}

df <- lapply(df, function(x) {
  if (hd_root_same_val(x)) {
    return(NULL)
  }
  return(x)
})
df <- df[!sapply(df, is.null)]

head(df, n = 100)
length(df)
