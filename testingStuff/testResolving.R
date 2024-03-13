library(future)
f <- function() { Sys.sleep(10); return("bla") }
plan(multisession)
fu <- future(f())
resolved(fu) 

