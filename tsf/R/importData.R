importData <- function(path) {
  if (!is.character(path)) {
    return(ErrorClass$new("path is not of type character"))
  }
  df <- try(as.data.frame(read_excel(path, col_names = TRUE)), silent = TRUE)
  if (class(df) != "try-error") {
    return(df)
  }
  line <- readLines(path, n = 1)
  semicolon <- grepl(";", line)
  comma <- grepl(",", line)
  tab <- grepl("\t", line)
  seperator <- NULL
  if (semicolon == TRUE) {
    seperator <- ";"
  } else if (comma == TRUE) {
    seperator <- ","
  } else if (tab == TRUE) {
    seperator <- "\t"
  } else {
    return(ErrorClass$new("Could not identify seperator in file"))
  }
  header <- FALSE
  firstLine <- try(readLines(path, n = 1L))
  if (class(firstLine) == "try-error") {
    return(ErrorClass$new("Could not read first line of file"))
  }
  firstLine <- strsplit(firstLine, split = seperator)[[1]]
  firstLine <- as.numeric(firstLine)
  if (all(is.na(firstLine))) header <- TRUE
  df <- try(read.csv(path, header = header, sep = seperator))
  if (class(df) == "try-error") {
    return(ErrorClass$new("Could not import data"))
  }
  if (ncol(df) != 2) {
    return(ErrorClass$new("Data has wrong dimensions, two columns were expected"))
  }
  if (nrow(df) == 0) {
    return(ErrorClass$new("Data has 0 rows"))
  }
  if (nrow(df) > 10000) {
    return(ErrorClass$new("Data has more than 10000 rows"))
  }
  if (any(is.na(df))) {
    return(ErrorClass$new("Data contains missing values"))
  }
  if (!all(sapply(df, is.numeric))) {
    return(ErrorClass$new("Data contains non-numeric values"))
  }
  return(df)
}
