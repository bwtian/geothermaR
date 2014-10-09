source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd//hkd_profiles_140806_164333.Rds")
hkd3dgrid  <- readRDS("~/Dropbox/2data/hkd/hkf1k3d1h-1k_df.Rds")

## simple

hkdbh  <- hkdbhs[hkdbhs$Depths >=100 & hkdbhs$Depths <=1000,]
hist(log(hkdbh$Temperature))
x  <- hkdbh$x_lccwgs84
y  <- hkdbh$y_lccwgs84
z  <- hkdbh$Depths
t  <- hkdbh$Temperature
logT  <- log10(hkdbh$Temperature)
xyzv  <- as.data.frame(cbind(x,y,z,t,logT))
class(xyzv)
df  <- na.omit(xyzv)
summary(df)
spdf  <- df
coordinates(spdf) <- ~x+y+z
proj4string(spdf)  <- CRS(lccWgs84)
### grid
grid  <- hkd3dgrid
coordinates(grid) <- ~x+y+z
proj4string(grid)  <- CRS(lccWgs84)
gridded(grid) <- TRUE
head(grid)
## 3D IDW
## 3D variogram
vgmT <- variogram(t~ z, spdf, cutoff = 2500)
vgmT
plot(vgmT)

vgmSphT <- fit.variogram(vgmT, vgm(3000, "Sph", 2500, 0))
vgmSphT
plot(vgmT, vgmSphT)
attr(vgmSphT, "SSErr")
vgmExpT <- fit.variogram(vgmT, vgm(3000, "Exp", 2500, 0))
vgmExpT
plot(vgmT, vgmExpT)
attr(vgmExpT, "SSErr")
vgmLinT <- fit.variogram(vgmT, vgm(3000, "Lin", 2500, 0))
vgmLinT
plot(vgmT, vgmLinT)
attr(vgmLinT, "SSErr")
vgmMatT <- fit.variogram(vgmT, vgm(3000, "Mat", 2500, 0, kappa=3))
vgmMatT
plot(vgmT, vgmMatT)
attr(vgmMatT, "SSErr")

vgmTok  <- fit.variogram(vgmT, vgmSphT)
vgmTok
plot(vgmT, vgmTok)
### log plot


vgmTlog <- variogram(logT ~ x+y+z, spdf, cutoff = 2500)
vgmTlog
plot(vgmTlog)

vgmSphTlog <- fit.variogram(vgmTlog, vgm(0.08, "Sph", 2500, 0))
vgmSphTlog
plot(vgmTlog, vgmSphTlog)
attr(vgmSphTlog, "SSErr")
vgmExpTlog <- fit.variogram(vgmTlog, vgm(0.08, "Exp", 2500, 0))
vgmExpTlog
plot(vgmTlog, vgmExpTlog)
attr(vgmExpTlog, "SSErr")
vgmLinTlog <- fit.variogram(vgmTlog, vgm(0.08, "Lin", 2500, 0))
vgmLinTlog
plot(vgmTlog, vgmLinTlog)
attr(vgmLinTlog, "SSErr")
vgmMatTlog <- fit.variogram(vgmTlog, vgm(0.08, "Mat", 2500, 0, kappa=3))
vgmMatTlog
plot(vgmTlog, vgmMatTlog)
attr(vgmMatTlog, "SSErr")


vgmTlogok  <- fit.variogram(vgmTlog, vgmSphTlog)
vgmTlogok
plot(vgmTlog, vgmTlogok)
