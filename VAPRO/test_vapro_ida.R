library(tinytest)

data_dir <- file.path("VAPRO", "simulated_data")
df <- read.csv(file.path(data_dir, "ida.csv"), header = TRUE)
ground_truth <- readRDS(file.path(data_dir, "ida_ground_truth.rds"))

res_vapro <- tsf::opti_vapro(
  case = "ida",
  lowerBounds = 1e3,
  upperBounds = 1e9,
  path = df,
  additionalParameters = c(
    ground_truth$host_conc,
    ground_truth$dye_conc,
    ground_truth$Kd
  ),
  nGrid = 2000
)
res_vapro$parameter
res_vapro$metrices
res_vapro$signal_plots[[1]]
res_vapro$d_hd_plot

res_pso <- tsf::opti(
  case = "ida",
  lowerBounds = c(Kg = 1e3, I0 = 0, IHD = 1e2, ID = 1e-15),
  upperBounds = c(Kg = 1e9, I0 = 1e4, IHD = 1e8, ID = 1e8),
  path = df,
  additionalParameters = c(
    host_conc = ground_truth$host_conc,
    dye_conc = ground_truth$dye_conc,
    kd = ground_truth$Kd
  ),
  seed = 1234, ngen = 1500
)
res_pso$signal_plots
data.frame(
  names = c("Kg", "I0", "IHD", "ID"),
  truth = c(ground_truth$Kg, ground_truth$I0, ground_truth$Ihd, ground_truth$Id),
  pso = c(t(format(res_pso$parameter, scientific = TRUE))),
  vapro = c(t(format(res_vapro$parameter, scientific = TRUE)))
)
