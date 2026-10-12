library(tsf)

ground_truth <- list(
  case = "gda",
  Kd = 1.7e7,
  Kg = 1.8e6,
  I0 = 0.0408218,
  Ihd = 602000,
  Id = 0,
  host_conc = 1.65e-6,
  guest_conc = 1.32e-6,
  dye_conc = seq(1e-15, 7.5e-6, length.out = 30)
)

simulated_data <- tsf:::forward_gda(
  Kd = ground_truth$Kd,
  Kg = ground_truth$Kg,
  Id = ground_truth$Id,
  Ihd = ground_truth$Ihd,
  h0 = ground_truth$host_conc,
  g0 = ground_truth$guest_conc,
  d0_values = ground_truth$dye_conc
)
simulated_data$Signal <- simulated_data$Signal + ground_truth$I0
stopifnot(nrow(simulated_data) == length(ground_truth$dye_conc))

outputDir <- file.path("VAPRO", "simulated_data")
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
write.csv(simulated_data, file.path(outputDir, "gda.csv"), row.names = FALSE, quote = FALSE)
saveRDS(ground_truth, file.path(outputDir, "gda_ground_truth.rds"))
