

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
  if(length(b) == 0) return(ErrorClass$new("Body of f is empty"))
  for(i in 1:length(b)) {
    temp <- getAST(b[[i]]) 
    if(is(temp, "ErrorClass")) {
      return(temp)
    }
  }
  b <- paste(b, collapse = " , ")
  print(b)
  
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
  
  elim_vars(create_eqns(), elim_vars);
  "
}

