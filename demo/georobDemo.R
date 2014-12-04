library(georob)
data(wolfcamp, package = "geoR")
summary(wolfcamp)
############################################
### Sammple Variogram: sample.variogram()
############################################
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

############################################
### Fitting Variogram: fit.variogram.model()
############################################
r.irf0.iso <- fit.variogram.model(r.sv.iso, variogram.model = "RMfbm",
                                  param = c(variance = 100, nugget = 1000, scale = 1., alpha = 1.),
                                  fit.param = c( variance = TRUE, nugget = TRUE, scale = FALSE, alpha = TRUE),
                                  method = "Nelder-Mead", hessian = FALSE, control = list(maxit = 5000))
summary(r.irf0.iso, correlation = TRUE)
plot( r.sv.iso, type = "l")
lines( r.irf0.iso, line.col = "red")

r.irf0.aniso <- fit.variogram.model(r.sv.aniso, variogram.model = "RMfbm",
                                    param = c(variance = 100, nugget = 1000, scale = 1., alpha = 1.5),
                                    fit.param = c(variance = TRUE, nugget = TRUE, scale = FALSE, alpha = TRUE),
                                    aniso = c(f1 = 0.4, f2 = 1., omega = 135, phi = 90., zeta = 0.),
                                    fit.aniso = c(f1 = TRUE, f2 = FALSE, omega = TRUE, phi = FALSE, zeta = FALSE),
                                    method = "Nelder-Mead", hessian = TRUE, control = list(maxit = 5000))
summary(r.irf0.aniso, correlation = TRUE)

lines(r.irf0.aniso, xy.angle = seq( 0, 135, by = 45))

###
###################
## wolfcamp data ##
###################
data(wolfcamp, package = "geoR")
d.wolfcamp <- data.frame(x = wolfcamp[[1]][,1], y = wolfcamp[[1]][,2],
                         pressure = wolfcamp[[2]])

## fitting isotropic IRF(0) model

r.irf0.iso <- georob(pressure ~ 1, data = d.wolfcamp, locations = ~ x + y,
                     variogram.model = "RMfbm",
                     param = c( variance = 10, nugget = 1500, scale = 1, alpha = 1.5 ),
                     fit.param = c( variance = TRUE, nugget = TRUE, scale = FALSE, alpha = TRUE),
                     tuning.psi = 1000)

summary(r.irf0.iso)

## fitting isotropic IRF(0) model

r.irf0.aniso <- georob(pressure ~ 1, data = d.wolfcamp, locations = ~ x + y,
                       variogram.model = "RMfbm",
                       param = c( variance = 5.9, nugget = 1450, scale = 1, alpha = 1 ),
                       fit.param = c( variance = TRUE, nugget = TRUE, scale = FALSE, alpha = TRUE),
                       aniso = c( f1 = 0.51, f2 = 1, omega = 148, phi = 90, zeta = 0 ),
                       fit.aniso = c( f1 = TRUE, f2 = FALSE, omega = TRUE, phi = FALSE, zeta = FALSE ),
                       tuning.psi = 1000)
summary(r.irf0.aniso)

plot(r.irf0.iso, lag.class.def = seq(0, 200, by = 7.5))
plot(r.irf0.aniso, lag.class.def = seq(0, 200, by = 7.5),
     xy.angle.def = c(0, 22.5, 67.5, 112.5, 157.5, 180.),
     add = TRUE, col = 2:5)

pchisq( 2*(r.irf0.aniso[["loglik"]] - r.irf0.iso[["loglik"]]), 2, lower = FALSE )
## End(Not run)


