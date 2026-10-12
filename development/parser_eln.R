install.packages("./tsf", type = "source", repos = NULL)
tinytest::test_package("tsf")

Sys.setenv(RUN_MODE = "SERVER")
tsf::runApp(3839)
