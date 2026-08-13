library(tsf)

ground_truth <- list(
  case = "ida",
  Kd = 3e6,
  Kg = 2e7,
  I0 = 0,
  Ihd = 1e6,
  Id = 2e5,
  host_conc = 1e-6,
  dye_conc = 1e-6,
  guest_conc = seq(1e-15, 5e-6, length.out = 30)
)

simulated_data <- tsf:::forward_ida(
  Kg = ground_truth$Kg,
  Ihd = ground_truth$Ihd,
  Id = ground_truth$Id,
  Kd = ground_truth$Kd,
  h0 = ground_truth$host_conc,
  d0 = ground_truth$dye_conc,
  g0_values = ground_truth$guest_conc
)
simulated_data$Signal <- simulated_data$Signal + ground_truth$I0
stopifnot(nrow(simulated_data) == length(ground_truth$guest_conc))

outputDir <- file.path("VAPRO", "simulated_data")
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
write.csv(simulated_data, file.path(outputDir, "ida.csv"), row.names = FALSE, quote = FALSE)
saveRDS(ground_truth, file.path(outputDir, "ida_ground_truth.rds"))
