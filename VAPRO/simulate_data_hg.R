library(tsf)

ground_truth <- list(
  case = "dba_host_const",
  Kd = 3e6,
  I0 = 15,
  Ihd = 6e5,
  Id = 1.2e5,
  host_conc = 1e-4,
  dye_conc = seq(1e-15, 1.5e-4, length.out = 30)
)

simulated_data <- tsf:::forward_dba_host_const(
  Kd = ground_truth$Kd,
  Id = ground_truth$Id,
  Ihd = ground_truth$Ihd,
  h0 = ground_truth$host_conc,
  d0_values = ground_truth$dye_conc
)
simulated_data$Signal <- simulated_data$Signal + ground_truth$I0
stopifnot(nrow(simulated_data) == length(ground_truth$dye_conc))

outputDir <- file.path("VAPRO", "simulated_data")
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
write.csv(simulated_data, file.path(outputDir, "dba_host_const.csv"), row.names = FALSE)
saveRDS(ground_truth, file.path(outputDir, "dba_host_const_ground_truth.rds"))
