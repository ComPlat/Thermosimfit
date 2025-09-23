install.packages("tsf", repos = NULL, type = "source")
detach("package:tsf", unload = TRUE)
library(tsf)

lowerBounds <- c(
  kG = 0,
  I0 = 0,
  IHD = 0,
  ID = 0
)
upperBounds <- c(
  kG = 10^10,
  I0 = 10^2,
  IHD = 10^10,
  ID = 10^10
)
additionalParameters <- c(
  host = 4.3 * 10^-6,
  dye = 6 * 10^-6,
  kHD = 1.7E07
)

# NOTE: data var column [M]
data <- readLines("./Paper/IDA.txt")
var <- lapply(data, function(x) {
  strsplit(x, split = "\t")[[1]][1]
}) |> unlist()
signal <- lapply(data, function(x) {
  strsplit(x, split = "\t")[[1]][2]
}) |> unlist()
# NOTE: Only the first dataset is used
df <- data.frame(var = var[2:22], signal = signal[2:22])
df$var <- as.numeric(df$var)
df$signal <- as.numeric(df$signal)

seed <- 1234

opti(
  case = "ida",
  lowerBounds = lowerBounds,
  upperBounds = upperBounds,
  path = df,
  seed = seed,
  ngen = 1000,
  npop = 40,
  errorThreshold = -Inf,
  additionalParameters = additionalParameters,
  add_info = as.character(seed),
  error_calc_fct = "Rel. Error"
)

tsf::runApp(4005)
