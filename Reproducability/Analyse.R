library(tsf)
library(ggplot2)
library(patchwork)
setwd("/home/konrad/Documents/GitHub/Thermosimfit/Reproducability")
files <- list.files(pattern = "*.txt")
kHD <- 2398.833 * 10^6 # log Ka = 3.38 
H0 <- 100 # µM
d0 <- 100 # µM
# guest from 0 - 366 µM

I0 <- 600 # 0.0006 or 6*10^-4 when using µM
ID <- 2.48e06 # 2.48 when using µM

# kGuest = 6.45e4, 1.14e5, 1.05e5, 7.3e4, 1.19e5, 5.63e4, 3.4e4, 4.87e4

# grenzen enger setzten
res <- opti("ida", c(1, 0, 0, 0), c(10^10, 10*10^-4, 100, 100), files[[1]], c(H0, d0, kHD),
            npop = 40, ngen = 400, Topology = "random")
res
res[[2]]

res <- lapply(files, function(x) {
  l <- lapply(1:5, function(y) {
    opti("ida", c(1, 0, 0, 0), c(10^10, 10*10^-4, 100, 100), files[[1]], c(H0, d0, kHD),
         npop = 40, ngen = 400, Topology = "random")
  })
  names(l) <- paste0(1:5, x)
  return(l)
})

saveRDS(res, file = "Runs.rds")
res <- readRDS("Runs.rds")
d <- lapply(res, function(x) {
  l <- lapply(x, function(y) {
    return(y[[1]])
  })
  l <- Reduce(rbind, l)
  l <- as.data.frame(l)
  l <- cbind(l, parent.frame()$i[])
})
d <- Reduce(rbind, d)
names(d)[6] <- "file"
d <- tidyr::pivot_longer(d, cols = -c(guest, file))
d$group <- ifelse(d$name != "d", "signal", "d")  
d$group <- ifelse(d$name == "hd", "hd", d$group)
ggplot(data = d, aes(x = guest, y = value, colour = factor(file))) +
  geom_point() +
  facet_wrap(.~ group, scales = "free")

params <- lapply(res, function(x) {
  l <- lapply(x, function(y) {
    return(y[[2]])
  })
  l <- Reduce(rbind, l)
  l <- as.data.frame(l)
  l <- cbind(l, parent.frame()$i[])
})
means <- lapply(params, function(x) {
  apply(x, 2, mean)
})
means <- Reduce(rbind, means)
means[,1] <- means[, 1] / 10^6
means[, 2:4] <- means[, 2:4] * 10^6

format(means, scientific = TRUE)
# kguest         I0             IHD            ID             parent.frame()$i[]
# init "2.153240e+04" "2.551025e+02" "1.520668e+07" "9.525392e+06" "1.000000e+00"    
# "2.799020e+04" "6.884824e+02" "1.534880e+07" "9.592180e+06" "2.000000e+00"    
# "4.544291e+04" "6.210149e+02" "1.513578e+07" "9.914334e+06" "3.000000e+00"    
# "1.044127e+04" "1.508630e+02" "1.507873e+07" "8.488497e+06" "4.000000e+00"    
# "2.490850e+04" "6.666667e+02" "1.495009e+07" "9.367511e+06" "5.000000e+00"    
# "4.260270e+03" "3.264909e+02" "1.497401e+07" "7.709176e+06" "6.000000e+00"    
# "7.210665e+03" "1.842407e+02" "1.519122e+07" "7.855857e+06" "7.000000e+00"    

params <- Reduce(rbind, params)
names(params) <- c("kguest", "I0", "IHD", "ID", "file")
params <- tidyr::pivot_longer(params, cols = -file)
params$value <- params$value / 10^6 

ggplot(data = params, aes(x = file, y = value, colour = file)) +
  geom_boxplot(data = params, aes(x = file, y = value, group = file, colour = file)) +
  geom_point() +
  facet_wrap(.~ name, scales = "free") +
  stat_summary(aes(group = name), fun.y = mean, geom = "line")




