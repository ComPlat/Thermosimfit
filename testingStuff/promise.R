library(promises)
library(future)

plan(multisession)
fa <- future({ cat("Hello world!\n"); print(1:3); 42L })
fb <- future({ str(iris); summary(iris) })
a <- value(fa)
b <- value(fb)

async_function <- function() {
  Sys.sleep(2)  
  return("Async function completed!")
}

res <- future(async_function())
value(res)

