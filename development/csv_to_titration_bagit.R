# Converts a Thermosimfit csv (var;signal) into a Titration_Jasco_Export BagIt zip
# as produced by chemotion-converter-app, so the ELN variations import can read it.
# usage: Rscript csv_to_titration_bagit.R in.csv out.zip [xunits] [sample]

read_points <- function(path) {
  first <- readLines(path, n = 1, warn = FALSE)
  sep <- if (grepl(";", first)) ";" else if (grepl("\t", first)) "\t" else ","
  df <- read.csv(path, sep = sep, header = FALSE, fileEncoding = "UTF-8-BOM")
  df <- data.frame(x = suppressWarnings(as.numeric(df[[1]])), y = suppressWarnings(as.numeric(df[[2]])))
  df <- df[!is.na(df$x) & !is.na(df$y), ]
  if (nrow(df) == 0) stop("No numeric x/y rows found in ", path)
  df
}

jcamp <- function(df, xunits, sample) {
  fmt <- function(v) format(v, digits = 6, trim = TRUE)
  header <- c(
    "##TITLE=Spectrum",
    "##JCAMP-DX=5.00 $$ csv_to_titration_bagit.R",
    "##DATA TYPE=titration",
    "##DATA CLASS=XYPOINTS",
    paste0("##XUNITS=", xunits),
    "##YUNITS=ARBITRARY UNITS",
    paste0("##SAMPLE=", sample),
    paste0("##FIRSTX=", fmt(df$x[1])),
    paste0("##LASTX=", fmt(df$x[nrow(df)])),
    paste0("##MINX=", fmt(min(df$x))),
    paste0("##MAXX=", fmt(max(df$x))),
    paste0("##MINY=", fmt(min(df$y))),
    paste0("##MAXY=", fmt(max(df$y))),
    paste0("##NPOINTS=", nrow(df)),
    paste0("##FIRSTY=", fmt(df$y[1])),
    "##XYPOINTS=(XY..XY)"
  )
  c(header, paste0(fmt(df$x), ", ", fmt(df$y)), "##END=$$ End of the data block")
}

write_bagit <- function(csv_path, zip_path, xunits = "mol/L", sample = "titration") {
  bag <- file.path(tempfile("bag"))
  dir.create(file.path(bag, "data"), recursive = TRUE)
  dir.create(file.path(bag, "metadata"))

  payload <- list(
    "data/table_01.jdx" = jcamp(read_points(csv_path), xunits, sample),
    "metadata/rdf.ttl" = c(
      "@prefix chemotion: <https://chemotion.net/chemotion/#> .",
      "@prefix obi: <http://purl.obolibrary.org/obo/> .",
      "",
      "chemotion:Titration_Jasco_Export a obi:OBI_0000070 ."
    ),
    "metadata/converter.json" = "{}"
  )
  for (name in names(payload)) writeLines(payload[[name]], file.path(bag, name))
  writeLines(c("BagIt-Version: 1.0", "Tag-File-Character-Encoding: UTF-8"), file.path(bag, "bagit.txt"))
  for (algo in c("sha256", "sha512")) {
    hashes <- sapply(names(payload), function(name) {
      as.character(openssl::multihash(file(file.path(bag, name)), algo)[[algo]])
    })
    writeLines(paste(hashes, names(payload)), file.path(bag, paste0("manifest-", algo, ".txt")))
  }

  # normalizePath only resolves existing paths; the zip does not exist yet
  zip_path <- file.path(normalizePath(dirname(zip_path)), basename(zip_path))
  unlink(zip_path)
  old <- setwd(bag)
  on.exit(setwd(old))
  utils::zip(zip_path, list.files(".", recursive = TRUE), flags = "-q")
  invisible(zip_path)
}

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) stop("usage: Rscript csv_to_titration_bagit.R in.csv out.zip [xunits] [sample]")
  write_bagit(args[1], args[2], if (length(args) > 2) args[3] else "mol/L", if (length(args) > 3) args[4] else "titration")
}
