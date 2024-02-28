addArguments <- function(existingFunction, newArgumentNames) {
  existingArgs <- as.list(formals(existingFunction))
  newArgs <- setNames(rep(list(NULL), length(newArgumentNames)), newArgumentNames)
  updatedArgs <- c(existingArgs, newArgs)
  formals(existingFunction) <- updatedArgs
  existingFunction
}

addCode <- function(existingFunction, codeVector) {
  existingBody <- as.list(body(existingFunction))
  newBody <- as.list(parse(text = codeVector))
  updatedBody <- c(existingBody, newBody)
  body(existingFunction) <- as.call(updatedBody)
  existingFunction
}



#' Create a costume loss function
#'
#' @export
#' @param f is a function defining the algebraic system
#' @param listElimVars a list with one or more character vectors which define the variables which have to be eliminated respectively
#' @param unknownRoots a character vector defining the names of the unknown variables. 
#' @param parameterVec a character vector defining the parameters which have to be optimized
#' @param additionalParameterVec a character vector defining additional parameters which are constant.
#' @param variedParameterVec a character vector with two entries defining the name of the variable which is altered, for instance d0 when dye is added to the system. The second entry has to be the independent variable e.g. signal
#' @param linearSystem a function defining the linear system which is used to calculate the insilico dependent variable
#' @return the resulting loss function
#' @examples 
#' f <- function() {
#'   h + hd + -h0 = 0
#'   d + hd -d0 = 0   
#'   hd / (h*d) -kd = 0
#' }
#' listElimVars <- list(c("h", "d"), c("hd", "h"))
#' unknownRoots <- c("d", "hd")
#' parameterVec <- c("kd", "I0", "IHD", "ID")
#' additionalParameterVec <- c("h0")
#' variedParameterVec <- c("d0", "signal")
#' linearSystem <- function() {I0 + IHD * hd + ID * d}
#' createLossFunction(f, listElimVars, unknownRoots, parameterVec, additionalParameterVec,
#'               variedParameterVec, linearSystem)
createLossFunction <- function(f, listElimVars, unknownRoots,
                               parameterVec, additionalParameterVec,
                               variedParameterVec, linearSystem) {
  if(!is.function(f)) return(ErrorClass$new("f is not a function"))
  if(!is.function(linearSystem)) return(ErrorClass$new("linearSystem is not a function"))
  if(!is.list(listElimVars)) return(ErrorClass$new("listElimVars is not a list"))
  if(length(listElimVars) == 0) return(ErrorClass$new("listElimVars is empty"))
  for(i in seq_along(listElimVars)) {
    if(!is.character(listElimVars[[i]])) return(ErrorClass$new("found non character vector in listElimVars"))
  }
  if(!is.character(unknownRoots)) return(ErrorClass$new("unknownRoots is not a character vector"))
  if(!is.character(parameterVec)) return(ErrorClass$new("parameterVec is not a character vector"))
  if(!is.character(additionalParameterVec)) return(ErrorClass$new("additionalParameterVec is not a character vector"))
  if(!is.character(variedParameterVec)) return(ErrorClass$new("variedParameterVec is not a character vector"))
  if(length(unknownRoots) == 0) return(ErrorClass$new("unknownRoots is empty"))
  if(length(parameterVec) == 0) return(ErrorClass$new("parameterVec is empty"))
  if(length(additionalParameterVec) == 0) return(ErrorClass$new("additionalParameterVec is empty"))
  if(length(variedParameterVec) != 2) return(ErrorClass$new("variedParameterVec has to be of length 2"))
  if(length(listElimVars) != length(unknownRoots)) return(ErrorClass$new("length differ between listElimVars and unknownRoots"))
  
  listFcts <- lapply(seq_along(listElimVars), function(x) {
    fct <- function() {}
    fct <- addArguments(fct, unknownRoots[[x]])
    polys <- createPolynom(f, listElimVars[[x]])
    print("test")
    polys <- deparse(polys)
    polys <- unlist(polys)
    polys <- paste0("{", polys, "}")
    polys <- str2lang(polys)
    body(fct) <- polys
    fct <- deparse(fct) 
    fct[[1]] <- paste0(unknownRoots[[x]], "Fct <- ", fct[[1]])
    fct <- paste0(fct, collapse = "\n")
    fct <- str2lang(fct)
    return(fct)
  })
  
  optiParams <- list()
  for(i in seq_along(parameterVec)) {
    optiParams[[i]] <- str2lang(
      paste0(parameterVec[i], " <- parameter[", i, "]")
    )
  }
  
  addParams <- list()
  for(i in seq_along(additionalParameterVec)) {
    addParams[[i]] <- str2lang(
      paste0(additionalParameterVec[i], " <- env$", additionalParameterVec[i])
    )
  }
  
  variedParam <- str2lang(
    paste0("variableParam <- env$", variedParameterVec[[1]])
  )
  signal <- str2lang(
    paste0("signal <- env$", variedParameterVec[[2]])
  )
  
  vecs <- list()
  for(i in seq_along(unknownRoots)) {
    vecs[[i]] <- str2lang(
      paste0(unknownRoots[[i]], " <- numeric(length(variableParam))")
    )
  }
  
  callRooTFct <- lapply(unknownRoots, function(x) {
    rootP <- paste0(x, "Root")
    s <- paste0(x, "Root <- uniroot.all(", x, "Fct, ", 
                "c(0, variableParam[i]), ", 
                "tol = .Machine$double.eps^15, maxiter = 10000, n = 1000) \n",
                "if (length(", rootP, ") > 1)", rootP, "<- ", rootP, "[length(hdRoot)]")
  })
  callRooTFct <- unlist(callRooTFct)
  ps <- function(a, b) {
    paste(a, "\n", b, collapse = "\n")
  }
  callRooTFct <- Reduce(ps, callRooTFct)
  
  assignUnknownRoots <- lapply(unknownRoots, function(x) {
    rootP <- paste0(x, "Root")
    s <- paste0(
      x, "[i] <- ", rootP, "\n"
    )
  })
  assignUnknownRoots <- unlist(assignUnknownRoots)
  assignUnknownRoots <- Reduce(ps, assignUnknownRoots)
  
  constCode <- paste0("for (i in seq_along(variableParam)) {\n",
                      variedParameterVec[[1]], " <- variableParam[i]\n",
                      callRooTFct, "\n",
                      assignUnknownRoots, "\n",
                      "}",
                      collapse = "\n") |> str2lang()
  
  insilico <- paste0("insilico <- ", deparse(substitute(linearSystem)), "()") |> str2lang()
  
  ret <- list(
    str2lang(paste0("if (eval) {", 
      "return(insilico)", 
    "}")),
    str2lang("return( sum( abs(signal - insilico) / signal ) )")
  ) 
  
  l <- c(listFcts, optiParams, addParams, variedParam, signal, vecs, constCode,
         insilico, ret)
  l <- lapply(l, deparse)
  l <- unlist(l)
  
  lf <- function(parameter, env, eval = FALSE) {}
  addCode(lf, l)
}
  
