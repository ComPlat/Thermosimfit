library(tsf)

ground_truth <- list(
  case = "dba_dye_const",
  Kd = 5e6,
  I0 = 25,
  Ihd = 8e5,
  Id = 1.5e5,
  dye_conc = 1e-4,
  host_conc = seq(1e-15, 1.5e-4, length.out = 30)
)

simulated_data <- tsf:::forward_dba_dye_const(
  Kd = ground_truth$Kd,
  Id = ground_truth$Id,
  Ihd = ground_truth$Ihd,
  d0 = ground_truth$dye_conc,
  h0_values = ground_truth$host_conc
)
simulated_data$Signal <- simulated_data$Signal + ground_truth$I0
stopifnot(nrow(simulated_data) == length(ground_truth$host_conc))

outputDir <- file.path("VAPRO", "simulated_data")
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
write.csv(simulated_data, file.path(outputDir, "dba_dye_const.csv"), row.names = FALSE, quote = FALSE)
saveRDS(ground_truth, file.path(outputDir, "dba_dye_const_ground_truth.rds"))
