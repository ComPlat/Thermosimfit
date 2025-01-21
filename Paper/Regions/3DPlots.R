library(plot3D)

png("GDA.png", width = 1200, height = 2000)
load("GDA_3D.RData")
z <- matrix(new_params$errors, nrow = length(KaHG), ncol = length(IHD))
hist3D(
  x = KaHG, y = IHD, z = z,
  col = viridis::viridis(20),
  # xlab = "Ka(HG) [1/M]", ylab = "I(HD) [1/M]", zlab = "MANE",
  phi = 50,
  cex.lab = 0.000001,
  font.lab = 2,
  font.lab = 2,
  font.axis = 2,
  cex.axis = 2.0,
  nticks = 10,
  ticktype = "detailed",
  colkey = list(
    side = 1,
    length = 0.25,
    width = 1,
    dist = -0.2,
    cex.clab = 2,
    cex.axis = 1.75,
    font.axis = 2
  )
)
text3D(
  x = max(KaHG) + 6 * 10^3, y = max(IHD) * 0.8, z = -1,
  phi = 50,
  labels = "I(HD) [1/M]", col = "black",
  add = TRUE, cex = 2.5, font = 2,
  srt = 55
)
text3D(
  x = max(KaHG) / 1.5, y = min(IHD) - 2.5 * 10^7, z = -5,
  phi = 50,
  labels = "Ka(HG) [1/M]", col = "black",
  add = TRUE, cex = 2.5, font = 2,
  srt = 320
)
text3D(
  x = min(KaHG) - 10000, y = min(IHD) - 10^7, z = max(new_params$errors) * 0.4,
  phi = 50,
  labels = "MANE", col = "black",
  add = TRUE, cex = 2.5, font = 2,
  srt = 295
)
dev.off()

png("DBA.png", width = 1200, height = 2000)
load("DBA_3D.RData")
z <- matrix(new_params$errors, nrow = length(KaHD), ncol = length(IHD))
hist3D(
  x = KaHD, y = IHD, z = z,
  col = viridis::viridis(30),
  # xlab = "Ka(HD) [1/M]", ylab = "I(HD) [1/M]", zlab = "MANE",
  phi = 50,
  cex.lab = 0.0001,
  font.axis = 2,
  cex.axis = 2.0,
  ticktype = "detailed",
  nticks = 10,
  colkey = list(
    side = 1,
    length = 0.25,
    width = 1,
    dist = -0.25,
    cex.clab = 2,
    cex.axis = 1.75,
    font.axis = 2
  )
)
text3D(
  x = max(KaHD) + 600, y = (max(IHD) / 2) - 100, z = -5,
  phi = 50,
  labels = "I(HD) [1/M]", col = "black",
  add = TRUE, cex = 2.5, font = 2,
  srt = 55
)
text3D(
  x = max(KaHD) / 2, y = min(IHD) - 5 * 10^7, z = -5,
  phi = 50,
  labels = "Ka(HD) [1/M]", col = "black",
  add = TRUE, cex = 2.5, font = 2,
  srt = 320
)
text3D(
  x = min(KaHD) / 2, y = min(IHD) - 5 * 10^7, z = max(new_params$errors) / 2,
  phi = 50,
  labels = "MANE", col = "black",
  add = TRUE, cex = 2.5, font = 2,
  srt = 295
)
dev.off()


png("IDA.png", width = 1200, height = 2000)
load("IDA_3D.RData")
z <- matrix(new_params$errors, nrow = length(KaHG), ncol = length(IHD))
hist3D(
  x = KaHG, y = IHD, z = z,
  col = viridis::viridis(20),
  # xlab = "Ka(HG) [1/M]", ylab = "I(HD) [1/M]", zlab = "MANE",
  phi = 50,
  cex.lab = 0.00001,
  font.lab = 2,
  font.axis = 2,
  cex.axis = 1.8,
  ticktype = "detailed",
  nticks = 10,
  colkey = list(
    side = 1,
    length = 0.25,
    width = 1,
    dist = -0.2,
    cex.clab = 2,
    cex.axis = 1.75,
    font.axis = 2
  )
)
text3D(
  x = max(KaHG) + 2 * 10^7, y = max(IHD) * 0.8, z = -1,
  phi = 50,
  labels = "I(HD) [1/M]", col = "black",
  add = TRUE, cex = 2.5, font = 2,
  srt = 55
)
text3D(
  x = max(KaHG) / 1.5, y = min(IHD) - 5 * 10^7, z = -5,
  phi = 50,
  labels = "Ka(HG) [1/M]", col = "black",
  add = TRUE, cex = 2.5, font = 2,
  srt = 320
)
text3D(
  x = min(KaHG), y = min(IHD) - 10^8, z = max(new_params$errors) * 0.7,
  phi = 50,
  labels = "MANE", col = "black",
  add = TRUE, cex = 2.5, font = 2,
  srt = 295
)
dev.off()
