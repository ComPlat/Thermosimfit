library(plot3D)

png("DBA.png", width = 1200, height = 2000)
load("DBA_3D.RData")

z <- matrix(new_params$errors, nrow = length(KaHD), ncol = length(IHD))
hist3D(
  x = KaHD, y = IHD, z = z,
  col = viridis::viridis(30),
  xlab = "Ka(HD) [1/M]", ylab = "I(HD) [1/M]", zlab = "rel. Error",
  phi = 50,
  cex.lab = 3,
  font.lab = 2,
  font.axis = 2,
  cex.axis = 1.0,
  ticktype = "detailed",
  colkey = list(
    side = 1,
    length = 0.25,
    width = 1,
    dist = -0.1,
    cex.clab = 2,
    cex.axis = 1.75,
    font.axis = 2
  )
)
dev.off()


png("IDA.png", width = 1200, height = 2000)
load("IDA_3D.RData")
z <- matrix(new_params$errors, nrow = length(KaHG), ncol = length(IHD))
hist3D(
  x = KaHG, y = IHD, z = z,
  col = viridis::viridis(20),
  xlab = "Ka(HG) [1/M]", ylab = "I(HD) [1/M]", zlab = "rel. Error",
  phi = 50,
  cex.lab = 2,
  font.lab = 2,
  cex.lab = 3,
  font.lab = 2,
  font.axis = 2,
  cex.axis = 1.0,
  ticktype = "detailed",
  colkey = list(
    side = 1,
    length = 0.25,
    width = 1,
    dist = -0.1,
    cex.clab = 2,
    cex.axis = 1.75,
    font.axis = 2
  )
)
dev.off()

png("GDA.png", width = 1200, height = 2000)
load("GDA_3D.RData")
z <- matrix(new_params$errors, nrow = length(KaHG), ncol = length(IHD))
hist3D(
  x = KaHG, y = IHD, z = z,
  col = viridis::viridis(20),
  xlab = "Ka(HG) [1/M]", ylab = "I(HD) [1/M]", zlab = "rel. Error",
  phi = 50,
  cex.lab = 2,
  font.lab = 2,
  cex.lab = 3,
  font.lab = 2,
  font.axis = 2,
  cex.axis = 1.0,
  ticktype = "detailed",
  colkey = list(
    side = 1,
    length = 0.25,
    width = 1,
    dist = -0.1,
    cex.clab = 2,
    cex.axis = 1.75,
    font.axis = 2
  )
)
dev.off()
