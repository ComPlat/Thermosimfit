install.packages("./tsf", type = "source", repos = NULL)

Sys.setenv(RUN_MODE = "SERVER")
tsf::runApp(3839)
