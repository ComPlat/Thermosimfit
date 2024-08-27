library(openxlsx)
library(ggplot2)
l <- readRDS("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/Batch/res_batch.rds")
setwd("/home/konrad/Documents/GitHub/RProjects/Thermosimfit/Tests/Batch")
result_val <- l
curr_row <- 1
num_rep <- 2
base_size <- 4
p1 <- tsf:::plotStates(result_val, num_rep, base_size, download = TRUE)


p1 <- p1 + theme(
  legend.position = "right",
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(10, 10, 10, 10),
  title = element_text(size = base_size, face = "bold"),
  axis.title = element_text(size = base_size, face = "bold"),
  axis.text = element_text(size = base_size),
  legend.text = element_text(size = base_size),
  legend.title = element_text(size = base_size),
  legend.key.size = unit(0.25, "cm"),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  plot.margin = margin(5, 5, 5, 5),
  strip.text.x = element_text(size = base_size, face = "bold"),
  strip.text.y = element_text(size = base_size, face = "bold")
)

wb <- openxlsx::createWorkbook()
addWorksheet(wb, "Results")
tempfile_plot1 <- tempfile(fileext = ".png")
ggsave(tempfile_plot1,
  plot = p1, units = "cm"
)
insertImage(wb, "Results", tempfile_plot1, 
  startRow = curr_row
)
openxlsx::saveWorkbook(wb, "Results.xlsx", overwrite = TRUE)
unlink(tempfile_plot1)

