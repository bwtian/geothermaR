library(georob)
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd/hkdbh_141201_110703.Rds")
summary(hkdbhs)
hkd3dgrid  <- readRDS("~/Dropbox/2data//dataProduct/hkd/hkd15hgrid_141201_111200.Rds")
spdf  <- hkdbhs
grid  <- hkd3dgrid
summary(hkd3dgrid)
        max(spdf$z)
###  sample variogram
sv.iso <- sample.variogram(log(spdf@data$t), locations = spdf@coords,
                             lag.class.def =c(seq(100,1000,100), seq(2000,45000,5000)))
summary(sv.iso)
plot(sv.iso)
plot(sv.iso, type = "l")

### fit
r.irf0.iso <- fit.variogram.model(sv.iso, variogram.model = "RMfbm",
                                  param = c(variance = 100, nugget = 1000, scale = 1., alpha = 1.),
                                  fit.param = c( variance = TRUE, nugget = TRUE, scale = FALSE, alpha = TRUE),
                                  method = "Nelder-Mead", hessian = FALSE, control = list(maxit = 5000))
summary(r.irf0.iso, correlation = TRUE)
plot( r.sv.iso, type = "l")
lines( r.irf0.iso, line.col = "red")
summary(spdf)

fsv
r.logt.reml <- georob(log(t) ~ z, data = spdf, locations = grid@coords,
                       variogram.model = "RMexp",
                       param = c(variance = 0.28, nugget = 0.05, scale = 35000 ),
                       tuning.psi = 1000)
summary(r.logzn.reml, correlation = TRUE)

## robust REML fit
r.logzn.rob <- update(r.logzn.reml, tuning.psi = 1)

summary(r.logzn.rob, correlation = TRUE)

plot(r.logzn.reml, lag.class.def = seq( 0, 2000, by = 100 ))
lines(r.logzn.rob, col = "red")


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
