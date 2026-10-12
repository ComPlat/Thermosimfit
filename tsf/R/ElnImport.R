eln_value <- function(field) {
  v <- field$value
  if (is.null(v) || identical(v, "")) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(v))
}

eln_check_segment <- function(seg) {
  technique <- seg$general$Technique$value
  # `general` is not exported by labimotion yet; only reject when it is present
  if (!is.null(technique) && !identical(technique, "") && technique != "ABS") {
    return(ErrorClass$new(paste0("Technique ", technique, " is not supported. Only ABS is supported")))
  }
  stoich <- c("molecule_stoich", "partner_stoich", "cofactor_stoich", "indicator_stoich")
  for (s in stoich) {
    v <- eln_value(seg$bind_partners[[s]])
    if (!is.na(v) && v != 1) {
      return(ErrorClass$new(paste0(s, " is ", v, ". Only a stoichiometry of 1 is supported")))
    }
  }
  TRUE
}

eln_infer_model <- function(partners) {
  conc <- sapply(c("Molecule", "Partner", "Indicator"), function(p) eln_value(partners[[p]]))
  filled <- paste(names(conc)[!is.na(conc)], collapse = "+")
  model <- switch(filled,
    "Molecule" = "HG",
    "Indicator" = "DBA",
    "Molecule+Indicator" = "IDA",
    "Molecule+Partner" = "GDA",
    NULL
  )
  if (is.null(model)) {
    return(ErrorClass$new(paste0(
      "Cannot infer the model from the filled binding partners: ",
      if (filled == "") "none" else filled
    )))
  }
  list(model = model, conc = conc)
}

eln_constants <- function(model, conc, binding_props) {
  kHD <- eln_value(binding_props$Kd)
  switch(model,
    HG = list(H0 = conc[["Molecule"]]),
    DBA = list(D0 = conc[["Indicator"]]),
    IDA = list(H0 = conc[["Molecule"]], D0 = conc[["Indicator"]], kHD = kHD),
    GDA = list(H0 = conc[["Molecule"]], G0 = conc[["Partner"]], kHD = kHD)
  )
}

eln_xy_columns <- function(column_order) {
  material_types <- c("startingMaterials", "reactants", "products", "solvents")
  parts <- strsplit(unlist(column_order), ".", fixed = TRUE)
  parts <- Filter(function(p) length(p) == 3 && p[1] %in% material_types, parts)
  x <- Filter(function(p) p[1] != "products", parts)
  y <- Filter(function(p) p[1] == "products", parts)
  if (length(x) != 1 || length(y) != 1) {
    return(ErrorClass$new(paste0(
      "Expected exactly one material column (x) and one product column (y) but found ",
      length(x), " and ", length(y)
    )))
  }
  list(x = x[[1]], y = y[[1]])
}

eln_cell <- function(row, col) {
  eln_value(row[[col[1]]][[col[2]]][[col[3]]])
}

eln_xy_dfs <- function(variations, cols) {
  group <- sapply(variations, function(r) r$metadata$group$group %||% 1)
  subgroup <- sapply(seq_along(variations), function(i) {
    variations[[i]]$metadata$group$subgroup %||% i
  })
  variations <- variations[order(group, subgroup)]
  group <- sort(group)
  df <- data.frame(
    var = sapply(variations, eln_cell, cols$x),
    signal1 = sapply(variations, eln_cell, cols$y)
  )
  unname(lapply(split(df, group), function(d) `rownames<-`(d, NULL)))
}

eln_prefill_inputs <- function(session, model, constants) {
  for (name in names(constants)) {
    value <- constants[[name]]
    if (is.na(value)) next
    for (suffix in c("", "_vapro", "_batch", "_vapro_batch")) {
      updateTextInput(session, paste0(model, "-", name, suffix), value = format(value))
    }
  }
}

# src: token URL, file path or the already parsed payload
parse_eln_payload <- function(src) {
  payload <- if (is.list(src)) src else try(jsonlite::fromJSON(src, simplifyVector = FALSE), silent = TRUE)
  if (inherits(payload, "try-error")) {
    return(ErrorClass$new("Could not read the ELN payload"))
  }
  seg <- payload$segment[["Mol interactions"]]
  if (is.null(seg)) {
    return(ErrorClass$new("Segment 'Mol interactions' is missing"))
  }
  check <- eln_check_segment(seg)
  if (is(check, "ErrorClass")) {
    return(check)
  }
  inferred <- eln_infer_model(seg$bind_partners)
  if (is(inferred, "ErrorClass")) {
    return(inferred)
  }
  cols <- eln_xy_columns(payload$columnOrder)
  if (is(cols, "ErrorClass")) {
    return(cols)
  }
  list(
    model = inferred$model,
    dfs = eln_xy_dfs(payload$variations, cols),
    constants = eln_constants(inferred$model, inferred$conc, seg$binding_props)
  )
}
