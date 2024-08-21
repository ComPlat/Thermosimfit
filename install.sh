#!/bin/bash

R CMD INSTALL ./tsf

# locally
Rscript -e "tsf::runApp(4000)"
exit 0
# On Server
Rscript -e "
Sys.setenv(SERVER_ENV = \"TRUE\")
tsf::runApp(4000)
Sys.unsetenv(\"SERVER_ENV\")
"
