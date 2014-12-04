library(georob)
data(wolfcamp, package = "geoR")
summary(wolfcamp)
### Fitting Variogram
## fitting an isotropic IRF(0) model
r.sv.iso <- sample.variogram(wolfcamp[["data"]], locations = wolfcamp[[1]],
                             lag.class.def = seq(0, 200, by = 15))
summary(r.sv.iso)
plot(r.sv.iso, type = "l")

## fitting an anisotropic IRF(0) model
r.sv.aniso <- sample.variogram(wolfcamp[["data"]],
                               locations = wolfcamp[[1]], lag.class.def = seq(0, 200, by = 15),
                               xy.angle.def = c(0., 22.5, 67.5, 112.5, 157.5, 180.))
plot(r.sv.aniso, type = "l", add = TRUE, col = 2:5)


