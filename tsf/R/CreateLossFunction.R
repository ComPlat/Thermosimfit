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



#' Create a loss function in an analogues way as the supplied ones
#'
#' @export
#' @param f is a function defining the algebraic system
#' @param listElimVars a list with one or more character vectors which define the variables which have to be eliminated respectively
#' @param unknownRoots a character vector defining the names of the unknown variables. 
#' @param parameterVec a character vector defining the parameters which have to be optimized
#' @param additionalParameterVec a character vector defining additional parameters which are constant.
#' @param variedParameterVec a character vector with two entries defining the name of the variable which is altered,
#'                           for instance d0 when dye is added to the system. The second entry has to be the independent variable e.g. signal
#' @param fixRoots a list of functions which is called to correct the calculated roots.
#'                each function expects as argument one root. The respective functions have to be in the same order as unknownRoots
#' @param linearSystem a function defining the linear system which is used to calculate the insilico dependent variable
#' @param maxValRoot a character value which defines the max of the interval used for solving the roots. 
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
#' additionalParameterVec <- c("h0", "kd")
#' variedParameterVec <- c("d0", "signal")
#' fixRoots <- list(
#'   function(dRoot) { 
#'       if(dRoot > d0) return(d0)
#'       return(d0)
#'   },
#'   function(hdRoot) {
#'       if(hdRoot > d0 | hdRoot > h0) return(min(c(h0, d0)))
#'       return(hdRoot)
#'   }
#' )
#' linearSystem <- function() {I0 + IHD * hd + ID * d}
#' maxValRoot <- d0
#' createLossFunction(f, listElimVars, unknownRoots, parameterVec, additionalParameterVec,
#'               variedParameterVec, fixRoots, linearSystem)
createLossFunction <- function(f, listElimVars, unknownRoots,
                               parameterVec, additionalParameterVec,
                               variedParameterVec, fixRoots, 
                               linearSystem, maxValRoot) {
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
  if(!is.character(maxValRoot)) return(ErrorClass$new("maxValRoot is not of type character"))
  if(!is.list(fixRoots)) return(ErrorClass$new("fixRoots is not a list"))
  if(length(fixRoots) == 0) return(ErrorClass$new("fixRoots is empty"))
  for(i in seq_along(fixRoots)) {
    if(!is.function(fixRoots[[i]])) return(ErrorClass$new("found non function element in fixRoots"))
    if(deparse(body(fixRoots[[i]])[[1]]) != "{") return(ErrorClass$new("Function has to be enclosed by {}"))
    b <- body(fixRoots[[i]])
    if(length(b) == 0) return(ErrorClass$new("Body of fixRoots is empty"))
    b <- b[2:length(b)]
    for(j in 1:length(b)) {
      temp <- getAST(b[[j]]) 
      if(is(temp, "ErrorClass")) {
        return(temp)
      }
    }
  }
  if(length(unknownRoots) != length(fixRoots)) return(ErrorClass$new("length differ between fixRoots and unknownRoots"))
  if(!is.function(linearSystem)) return(ErrorClass$new("linearSystem is not of type function"))
  b <- body(linearSystem)
  if(deparse(b[[1]]) != "{") return(ErrorClass$new("Body of LinearSystem should be wrapped in {}"))
  if(length(b) == 0) return(ErrorClass$new("Body of linearsystem is empty"))
  b <- b[2:length(b)]
  for(j in 1:length(b)) {
    temp <- getAST(b[[j]]) 
    if(is(temp, "ErrorClass")) {
      return(temp)
    }
  }
  if(length(b) > 2) return(ErrorClass$new("Body of LinearSystem should consist of one line"))
  
  ps <- function(a, b) {
    paste(a, "\n", b, collapse = "\n")
  }
  listFcts <- lapply(seq_along(listElimVars), function(x) {
    fct <- function() {}
    fct <- addArguments(fct, unknownRoots[[x]])
    polys <- createPolynom(f, listElimVars[[x]])
    polys <- deparse(polys)
    polys <- unlist(polys)
    polys <- Reduce(ps, polys)
    polys <- paste0("{", polys, "}")
    polys <- str2lang(polys)
    body(fct) <- polys
    fct <- deparse(fct) 
    fct[[1]] <- paste0(unknownRoots[[x]], "Fct <- ", fct[[1]])
    fct <- paste0(fct, collapse = "\n")
    fct <- str2lang(fct)
    return(fct)
  })
  
  fixRoots <- lapply(seq_along(fixRoots), function(x) {
    fct <- function() {}
    rootP <- paste0(unknownRoots[[x]], "Root")
    fct <- addArguments(fct, rootP)
    b <- deparse(body(fixRoots[[x]]))
    b <- b[2:(length(b) - 1)]
    b <- Reduce(ps, b)
    fct <- addCode(fct, b)
    fct <- deparse(fct)
    fct[[1]] <- paste0(unknownRoots[[x]], "FctFixRoot <- ", fct[[1]])
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
    paste0("variableParam <- ifelse(env$", variedParameterVec[[1]], "== 0, 10^-15, ", "env$", variedParameterVec[[1]], ")")
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
                "c(0,", maxValRoot, "), ", 
                "tol = .Machine$double.eps^15, maxiter = 10000, n = 1000) \n",
                "if (length(", rootP, ") > 1)", rootP, "<- ", rootP, "[length(hdRoot)]")
  })
  callRooTFct <- unlist(callRooTFct)
  callRooTFct <- Reduce(ps, callRooTFct)
  
  assignUnknownRoots <- lapply(unknownRoots, function(x) {
    rootP <- paste0(x, "Root")
    s <- paste0(
      x, "[i] <- ", paste0(x, "FctFixRoot"), "(", rootP, ")", "\n"
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
  
  
  insilico <- paste0("insilico <- ", deparse(body(linearSystem)[[2]]))
  insilico <- insilico |> str2lang()
  
  ret <- list(
    str2lang(paste0("if (eval) {", 
      "return(c(insilico,", paste0(lapply(unknownRoots, paste0), collapse = ","), "))", 
    "}")),
    str2lang("return( sum( abs(signal - insilico) / signal ) )")
  ) 
  
  l <- c(listFcts, fixRoots, optiParams, addParams, variedParam, signal, vecs, constCode,
         insilico, ret)
  l <- lapply(l, deparse)
  l <- unlist(l)
  
  lf <- function(parameter, env, eval = FALSE) {}
  lossf <- addCode(lf, l)
  environment(lossf) <- globalenv()
  return(lossf)
}
  
