library(sp)
library(gstat)
library(lattice)
source("~/SparkleShare/geothermaR/demo/rSettings.R")
# load data and grid
hkdbh  <- readRDS("~/Dropbox//2data//dataProduct//hkd//hkd_profiles_140806_164333.Rds")
hkd3dgrid  <- readRDS("~/Dropbox/2data/hkd/hkd_grd3d0110k_140527_223205.Rds")
hist(log2(hkdbh$Temperature))
## simple

x  <- hkdbh$x_lccwgs84
y  <- hkdbh$y_lccwgs84
z  <- hkdbh$Depths
v  <- hkdbh$Temperature
xyzv  <- as.data.frame(cbind(x,y,z,v))
df  <- xyzv[complete.cases(xyzv),]
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
grid$vIDW <- idw(v~1,spdf,grid)$var1.pred
