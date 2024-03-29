extractPolynom <- function(s) {
  start <- NULL
  for(i in seq_along(s)) {
    if(any(grepl("Result", s[[i]]))) {
      start <- i + 1
      if(start > length(s)) return(ErrorClass$new("Found no result"))
    }
  }
  s <- s[start:length(s)]
  ps <- function(a, b) {
    paste0(a, " ", b)
  }
  s <- Reduce(ps, s)
  
  matches <- regmatches(s, gregexpr("\\[(.*?)\\]", s))
  s <- unlist(matches)
  s <- gsub("\\[|\\]", "", s)
  s <- try(str2lang(s))
  if(class(s) == "try-error") {
    return(ErrorClass$new("Could not convert polynom to language object"))
  }
  return(s)
}

#' Converts an algebraic system to a polynome by eliminating variables
#'
#' @export
#' @param f is a function defining the algebraic system
#' @param elimVars is a character vector defining the variables which have to be eliminated
#' @return the resulting polynome of the elimination
#' @examples 
#' f <- function() {
#'   h + hd + -h0 = 0
#'   d + hd -d0 = 0   
#'   hd / (h*d) -kd = 0
#' }
#' elimVars <- c("h", "d")
#' createPolynom(f, elimVars)
createPolynom <- function(f, elimVars) {
  if(!is.function(f)) return(ErrorClass$new("f is not a function"))
  if(!is.character(elimVars)) return(ErrorClass$new("elimVars is not of type character"))
  
  b <- body(f)
  if(deparse(b[[1]]) != "{") return(ErrorClass$new("Function body has to be enclosed by {}"))
  b <- b[2:length(b)]
  if(length(b) == 0) return(ErrorClass$new("Body of f is empty"))
  for(i in 1:length(b)) {
    temp <- getAST(b[[i]]) 
    if(is(temp, "ErrorClass")) {
      return(temp)
    }
  }
  b <- lapply(b, deparse)
  b <- paste(b, collapse = " , ")
  
  constCode1 <- "
  display2d: false;
  simp: false;
  tellsimp (0^0, 1);
  simp: true;
  algepsilon: 10^8;
  create_eqns() :=
    block(
  eqns: [
  "
  constCode2 <- "
  ],
  return(eqns)
  );
  
  elim_vars(eqns, elim_vars) :=
    block(
      [eliminated_eqns],
      eliminated_eqns: eliminate(eqns, elim_vars), 
      return(eliminated_eqns)
    );
  "
  
  constCode3 <- "
    elim_vars: [
  "
  constCode4 <- "
    ];
  
  Result: elim_vars(create_eqns(), elim_vars);
  
  Result;
  "
  
  code <- paste0(
    constCode1, b, constCode2, constCode3, paste0(elimVars, collapse = ","), constCode4
  )
  
  MaximaInstalled <- try(system("maxima --version"))
  if(MaximaInstalled != 0) return(ErrorClass$new("Maxima was not found. Please install it."))
  
  s <- paste("maxima --batch-string", "\"",
             code, "\"")
  out <- try(system(s, intern = TRUE))
  if(length(out) == 1 & out[[1]] != 0) {
    return(ErrorClass$new("Running elimination was not successfull."))
  }
  extractPolynom(out)
}

