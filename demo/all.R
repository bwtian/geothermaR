library(georob)
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd/hkdbh_141201_110703.Rds")
summary(hkdbhs)
hkd3dgrid  <- readRDS("~/Dropbox/2data//dataProduct/hkd/hkd15hgrid_141201_111200.Rds")
spdf  <- hkdbhs
grid  <- hkd3dgrid
# summary(hkd3dgrid)
#         max(spdf$z)
# t300 <- spdf@data[spdf@data$t > 300,]
###  sample variogram
sv.iso <- sample.variogram(log(spdf@data$t), locations = spdf@coords,
                             lag.class.def =c(seq(100,1000,100), seq(2000,45000,5000)))
summary(sv.iso)
plot(sv.iso)
plot(sv.iso, type = "l")

### fit
library()
fit.iso <- fit.variogram.model(sv.iso, variogram.model = "RMfbm",
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
## Here I took a irregular polygon as example
P <- sp::Polygon(cbind(c(2500, 2500, 3500, 3500, 2500),
                       c(1500, 4500, 4500, 5500, 1500)))
Ps <- sp::Polygons(list(P), "P")
SP <- sp::SpatialPolygons(list(Ps))
sp::plot(SP)
shp <- SP
## then sample use sp package to get a 2D grid, and you can choose the type of grid, here is a regular one
grid2d <- sp::spsample(shp,type = "regular", cellsize = c(100,100), offset =c(0.5, 0.5))
sp::plot(grid2d)
## until now you get xy data,then we create 3d point grid with depth to 100m to 1000m as example

source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# raster2d <- readRDS("~/Dropbox/2data/hkd//hkdmskbig_grdi2d1h.Rds")
# point2d  <- rasterToPoints(raster2d)
# xy <- point@coords
# xyz <- list()
# for (i in 1:10) {
#         xyz[[i]] <- cbind(xy,rep(i*100, nrow(xy), simplify=F))
#
# }
# grid3d <- as.data.frame(do.call(rbind, xyz))
# colnames(grid3d) <- c("x","y","z")
# grid3d$value <- 10
# plot(grid3d)
# write.csv(grid3d, "grid3d.csv",quote = F, row.names = F) ## save to csv
# #' Make 2D 100m * 100m Grid with lccWgs84 CRS
#'
## BBOX Raster
P  <- Polygon(cbind(c(1152500, 1152500, 1673500, 1673500, 1152500),
                     c(1372500, 1843500, 1843500, 1372500, 1372500)))
# P  <- Polygon(cbind(c(1152550, 1152550, 1673550, 1673550, 1152550),
#                     c(1372550, 1843550, 1843550, 1372550, 1372550)))
Ps  <- Polygons(list(P), "P")
SpP  <- SpatialPolygons(list(Ps))
grid2d1k  <- spsample(SpP,type = "regular", cellsize = c(100,100),
                      offset = c(0.5, 0.5))
proj4string(grid2d1k) <- CRS(lccWgs84)
# bbraster1h  <- rasterFromXYZ(bbgrid1h)
# bbraster1h[]  <- 1: ncell(bbraster1h)
# proj4string(bbraster1h) <- CRS(lccWgs84)
# proj4string(bbraster1h)

# plot(bbraster1h)
# saveRDS(bbraster1h, file = "~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
# hkd1h  <- readRDS("~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
# proj4string(hkd1h)
## Hokkaido Shape Raster

dsn  <- "~/Dropbox/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"
hkdshp  <- readShapePoly(dsn)
proj4string(hkdshp) <- CRS(lccWgs84)
hkdLand <- ge.LargestPolys(hkdshp, Polygon = T)
plot(hkdLand)
proj4string(hkdLand)  <- CRS(lccWgs84)
hkdLandPoint1k  <-gIntersection(grid2d1k, hkdLand)
proj4string(hkdLandPoint1k)  <- CRS(lccWgs84)
# saveRDS(hkdmaskb, file = "~/Dropbox/2data/hkd/hkf1kpoint2d.Rds")
# head(hkdmaskb)
# hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
# plot(hkdLandPoint1k)
# plot(hkdLand, add = T)
hkdPoint2d1k.df  <- as.data.frame(hkdLandPoint1k)

l  <- list()
for (i in 1:15) {
        l[[i]] <- cbind(hkdPoint2d1k.df, z = rep(i*100, nrow(hkdPoint2d1k.df), simplify=F))

}
d  <- do.call(rbind, l)
names(d)  <- c("x","y","z")
summary(d)
dd  <- d
coordinates(dd) <- ~x+y+z
proj4string(dd)  <- CRS(lccWgs84)
gridded(dd) <- TRUE
#saveRDS(dd, file = "~/Dropbox/2data/hkd/hkd1k3d15h.df.Rds")
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# raster2d <- readRDS("~/Dropbox/2data/hkd//hkdmskbig_grdi2d1h.Rds")
# point2d  <- rasterToPoints(raster2d)
# xy <- point@coords
# xyz <- list()
# for (i in 1:10) {
#         xyz[[i]] <- cbind(xy,rep(i*100, nrow(xy), simplify=F))
#
# }
# grid3d <- as.data.frame(do.call(rbind, xyz))
# colnames(grid3d) <- c("x","y","z")
# grid3d$value <- 10
# plot(grid3d)
# write.csv(grid3d, "grid3d.csv",quote = F, row.names = F) ## save to csv
# #' Make 2D 100m * 100m Grid with lccWgs84 CRS
#'
## BBOX Raster
P  <- Polygon(cbind(c(1152500, 1152500, 1673500, 1673500, 1152500),
                     c(1372500, 1843500, 1843500, 1372500, 1372500)))
# P  <- Polygon(cbind(c(1152550, 1152550, 1673550, 1673550, 1152550),
#                     c(1372550, 1843550, 1843550, 1372550, 1372550)))
Ps  <- Polygons(list(P), "P")
SpP  <- SpatialPolygons(list(Ps))
grid2d1k  <- spsample(SpP,type = "regular", cellsize = c(1000,1000),
                      offset = c(0.5, 0.5))
proj4string(grid2d1k) <- CRS(lccWgs84)
# bbraster1h  <- rasterFromXYZ(bbgrid1h)
# bbraster1h[]  <- 1: ncell(bbraster1h)
# proj4string(bbraster1h) <- CRS(lccWgs84)
# proj4string(bbraster1h)

# plot(bbraster1h)
# saveRDS(bbraster1h, file = "~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
# hkd1h  <- readRDS("~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
# proj4string(hkd1h)
## Hokkaido Shape Raster

dsn  <- "~/Dropbox/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"
hkdshp  <- readShapePoly(dsn)
proj4string(hkdshp) <- CRS(lccWgs84)
hkdLand <- ge.LargestPolys(hkdshp, Polygon = T)
plot(hkdLand)
proj4string(hkdLand)  <- CRS(lccWgs84)
hkdLandPoint1k  <-gIntersection(grid2d1k, hkdLand)
proj4string(hkdLandPoint1k)  <- CRS(lccWgs84)
# saveRDS(hkdmaskb, file = "~/Dropbox/2data/hkd/hkf1kpoint2d.Rds")
# head(hkdmaskb)
# hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
plot(hkdLandPoint1k)
plot(hkdLand, add = T)
hkdPoint2d1k.df  <- as.data.frame(hkdLandPoint1k)

l  <- list()
for (i in 1:15) {
        l[[i]] <- cbind(hkdPoint2d1k.df, z = rep(i*100, nrow(hkdPoint2d1k.df), simplify=F))

}
d  <- do.call(rbind, l)
names(d)  <- c("x","y","z")
summary(d)
dd  <- d
coordinates(dd) <- ~x+y+z
proj4string(dd)  <- CRS(lccWgs84)
gridded(dd) <- TRUE
summary(dd)
str(dd)
dd@coords
#saveRDS(dd, file = "~/Dropbox/2data/hkd/hkd1k3d15h.df.Rds")
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# raster2d <- readRDS("~/Dropbox/2data/hkd//hkdmskbig_grdi2d1h.Rds")
# point2d  <- rasterToPoints(raster2d)
# xy <- point@coords
# xyz <- list()
# for (i in 1:10) {
#         xyz[[i]] <- cbind(xy,rep(i*100, nrow(xy), simplify=F))
#
# }
# grid3d <- as.data.frame(do.call(rbind, xyz))
# colnames(grid3d) <- c("x","y","z")
# grid3d$value <- 10
# plot(grid3d)
# write.csv(grid3d, "grid3d.csv",quote = F, row.names = F) ## save to csv
# #' Make 2D 100m * 100m Grid with lccWgs84 CRS
#'
## BBOX Raster
P  <- Polygon(cbind(c(1152500, 1152500, 1673500, 1673500, 1152500),
                     c(1372500, 1843500, 1843500, 1372500, 1372500)))
# P  <- Polygon(cbind(c(1152550, 1152550, 1673550, 1673550, 1152550),
#                     c(1372550, 1843550, 1843550, 1372550, 1372550)))
Ps  <- Polygons(list(P), "P")
SpP  <- SpatialPolygons(list(Ps))
grid2d1k  <- spsample(SpP,type = "regular", cellsize = c(1000,1000),
                      offset = c(0.5, 0.5))
proj4string(grid2d1k) <- CRS(lccWgs84)
# bbraster1h  <- rasterFromXYZ(bbgrid1h)
# bbraster1h[]  <- 1: ncell(bbraster1h)
# proj4string(bbraster1h) <- CRS(lccWgs84)
# proj4string(bbraster1h)

# plot(bbraster1h)
# saveRDS(bbraster1h, file = "~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
# hkd1h  <- readRDS("~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
# proj4string(hkd1h)
## Hokkaido Shape Raster

dsn  <- "~/Dropbox/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"
hkdshp  <- readShapePoly(dsn)
proj4string(hkdshp) <- CRS(lccWgs84)
hkdLand <- ge.LargestPolys(hkdshp, Polygon = T)
plot(hkdLand)
proj4string(hkdLand)  <- CRS(lccWgs84)
hkdLandPoint1k  <-gIntersection(grid2d1k, hkdLand)
proj4string(hkdLandPoint1k)  <- CRS(lccWgs84)
# saveRDS(hkdmaskb, file = "~/Dropbox/2data/hkd/hkf1kpoint2d.Rds")
# head(hkdmaskb)
# hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
plot(hkdLandPoint1k)
plot(hkdLand, add = T)
hkdPoint2d1k.df  <- as.data.frame(hkdLandPoint1k)

l  <- list()
for (i in 1:15) {
        l[[i]] <- cbind(hkdPoint2d1k.df, z = rep(i*100, nrow(hkdPoint2d1k.df), simplify=F))

}
d  <- do.call(rbind, l)
names(d)  <- c("x","y","z")
saveRDS(d, file = "~/Dropbox/2data/hkd/hkd1k3d15h.df.Rds")
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd//hkd_profiles_140806_164333.Rds")
hkd3dgrid  <- readRDS("~/Dropbox/2data/hkd/hkf1k3d1h-1k_df.Rds")

## simple

hkdbh  <- hkdbhs[hkdbhs$Depths >=100 & hkdbhs$Depths <=1000,]
# Temp <- (hkdbh$Temperature)
# Le <- log(hkdbh$Temperature)
# L10 <- log10(hkdbh$Temperature)
# L2 <-  log2(hkdbh$Temperature)
# hist(Temp)
# hist(Le, 100)
# hist(L10,100)
# hist(L2,100)
# qqnorm(Temp)
# qqline(Temp, col="red", lwd=3)
# qqnorm(Le)
# qqline(Le, col="red", lwd=3)
# qqnorm(L10)
# qqline(L10, col="red", lwd=3)
# qqnorm(L2)
# qqline(L2, col="red", lwd=3)
# ks.test(Temp, "pnorm", mean = mean(Temp), sd =sd(Temp))
# ks.test(Le, "pnorm", mean = mean(Le), sd =sd(Le))
# ks.test(L2, "pnorm", mean = mean(L2), sd =sd(L2))
# ks.test(L10, "pnorm", mean = mean(L10), sd =sd(L10))
# library("nortest")
# ad.test(Le)
# ad.test(L2)
# ad.test(L10)
# ad.test(Temp)
# library("energy")
# mvnorm.etest(Le)
x  <- hkdbh$x_lccwgs84
y  <- hkdbh$y_lccwgs84
z  <- hkdbh$Depths
t  <- hkdbh$Temperature
xyzv  <- as.data.frame(cbind(x,y,z,t))
class(xyzv)
df  <- na.omit(xyzv)
summary(df)
spdf  <- df
coordinates(spdf) <- ~x+y+z
proj4string(spdf)  <- CRS(lccWgs84)
zerodist(spdf)
spdf0 <- remove.duplicates(spdf)
spdf <- spdf0
zerodist(spdf)
### grid
grid  <- hkd3dgrid
coordinates(grid) <- ~x+y+z
proj4string(grid)  <- CRS(lccWgs84)
gridded(grid) <- TRUE
head(grid)
## 3D IDW
IDW1 <- idw(t ~ 1,spdf,grid, idp = 1)
IDW2 <- idw(t ~ 1,spdf,grid, idp = 2)
IDWz <- idw(t ~ x+y+z,spdf,grid, idp = 2)
summary(IDWz)
IDWz2 <- idw(t ~ x^2+y^2+z^3,spdf,grid, idp = 2)
class(IDW2)
sgdf  <- as(IDW2, "SpatialGridDataFrame")
sgdfi  <- as.image.SpatialGridDataFrame(sgdf)

summary(sgdf)
# plot3D(raster(spdf))
# r  <- stack(sgdfi)
# r  <- brick(sgdf)
r  <- raster(sgdfi)
str(r)
plot(r)
class(r)
str(r)
plot3D(r)
decorate3d(r)
snapshot3d("hkd3d.png")

getwd()
library(lattice)
IDW1.df  <- as.data.frame(IDW1)
IDW2.df  <- as.data.frame(IDW2)

IDWz.df  <- as.data.frame(IDWz)
IDWz2.df  <- as.data.frame(IDWz2)
cols = oceColorsJet(25)
#cols = bpy.colors(255)
brks = seq(10,250,10)
labs = seq(10,250,20)
levelplot(var1.pred ~ x+y | z, IDW1.df, col.regions = cols)
levelplot(var1.pred ~ x+y | z, IDW2.df, col.regions = cols)
levelplot(var1.pred ~ x+y | z, IDWz.df,col.regions = cols)
levelplot(var1.pred ~ x+y | z, IDWz2.df,col.regions = cols)

class(IDW2.df)
gg0  <- ggplot(IDW2.df ,aes(x, y, col = var1.pred))+
        geom_point() +
        scale_x_continuous(label = function(x) x/1000) +
        scale_y_continuous(label = function(x) x/1000) +
        xlab("x (km)") + ylab("y (km)") +
        coord_equal() + theme_bw() +
        facet_wrap( ~ z, ncol = 5)
gg1  <- gg0 + scale_color_gradientn(colours = cols, breaks = brks, name = expression(Temperature~(degree*C)))

###
library(rgl)
IDW2.df[,1:3]
plot3d(IDW2.df[,1:3])
persp3d(IDW2.df[,1:3])
wireframe(z ~ x * y, data=IDW2.df)
surface3d

###
library(plot3D)
head(IDW2.df)
x  <- IDW2.df[1]
y  <- IDW2.df[2]
z  <- IDW2.df[3]

V  <- IDW2.df[4]
M  <- mesh(x,y,z)
colvar <- with(M, x*exp(-x^2 - y^2 - z^2))
scatter2D(M$x, M$y, M$z, V, pch = 15, cex = 5)
slice3D (x,y,z,colvar,theta = 60)
x <- y <- z <- seq(-1, 1, by = 0.1)
grid   <- mesh(x, y, z)
colvar <- with(grid, x*exp(-x^2 - y^2 - z^2))
x
y
z
str(colvar)
example(points3D)

source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd//hkd_profiles_140806_164333.Rds")
hkd3dgrid  <- readRDS("~/Dropbox/2data//dataProduct/hkd/hkd15hgrid_141201_111200.Rds")

## simple

hkdbh  <- hkdbhs[hkdbhs$Depths >=100 & hkdbhs$Depths <=1500,]
### Normality check
hist(log(hkdbh$Temperature))
qqnorm(log(hkdbh$Temperature))
qqline(log(hkdbh$Temperature), col = "red")
x  <- hkdbh$x_lccwgs84
y  <- hkdbh$y_lccwgs84
z  <- hkdbh$Depths
t  <- hkdbh$Temperature
Tlog  <- log(hkdbh$Temperature)
xyzv  <- as.data.frame(cbind(x,y,z,t,Tlog))
###  Trend Surface Analysis
plot(xyzv)
# pairs(xyzv)
# class(xyzv)
df  <- na.omit(xyzv)
### remove duplicated spatial point
summary(df)
spdf  <- df
coordinates(spdf) <- ~x+y+z
proj4string(spdf)  <- CRS(lccWgs84)
zerodist(spdf)
spdf0 <- remove.duplicates(spdf)
spdf <- spdf0
zerodist(spdf)
### grid
grid  <- hkd3dgrid
coordinates(grid) <- ~x+y+z
proj4string(grid)  <- CRS(lccWgs84)
gridded(grid) <- TRUE

grid.sp  <- as(grid, "SpatialPoints")
grid.df  <- data.frame(as.factor(1:length(grid.sp)))
grid.spdf <- sp::SpatialPointsDataFrame(grid.sp, grid.df)
hkd15hgrid  <-grid.spdf
ge.sp2shpPrj(hkd15hgrid)
ge.sp2shpPrj(hkdbh)
getwd()
Tlog.trend  <-  krige(Tlog ~ x + y + z , spdf, grid)
Tlog.trend.df  <- as.data.frame(Tlog.trend)
levelplot(data = Tlog.trend.df, exp(var1.pred) ~ x*y| z,  contour=TRUE)
#rgl::surface3d(Tlog.trend.df$x, Tlog.trend.df$y, Tlog.trend.df$z, col = rainbow(10))
###
Tlog.trend.lm <- lm(Tlog ~   z, spdf)
summary(Tlog.trend.lm)

plot(Tlog.trend.lm)



head(grid)
### log plot
vgmTlog <- variogram(Tlog ~ 1, spdf, cutoff = 100000)
vgmTlog
vgmTlog <- variogram(Tlog ~ 1, spdf,
                     boundaries = c(seq(100,1000,100), seq(2000,10000,1000)))
vgmTlog <- variogram(Tlog ~ z, spdf,
                     boundaries = c(seq(100,1000,100), seq(2000,45000,5000)))
plot(vgmTlog)
## Universal Kriging
#vgmTlog <- variogram(Tlog ~ z^3, spdf, cutoff = 50000,cloud = T)
#vgmTlog <- variogram(logT ~ z, spdf, cutoff = 2500, alpha = c(0,45,90,135), beta = c(0,45,90,135))
# Trend
#trend  <- gstat(NULL, "trend", logT ~ z^2, data = spdf)
#vgmTlog  <- variogram(trend, cutoff = 2500)
show.vgms()


v.eye  <- vgm(0.28, "Pen", 35000, 0)
v.eye1  <- vgm(psill = 0.16,  model = "Gau",  range=700,  nugget=0)
v.eye   <- vgm(psill = 0.13,  model = "Sph",  range=35000,  nugget=0,  add.to=v.eye1)
v.eye
plot(vgmTlog, v.eye)

vgmGauTlog <- fit.variogram(vgmTlog, v.eye)
vgmGauTlog
plot(vgmTlog, vgmGauTlog)
plot(vgmTlog, v.eye)
attr(vgmGauTlog, "SSErr")
grid$TlogUK.eye <- krige(Tlog~z, spdf, grid, model = v.eye)
show.vgms

### post-krige
log(exp(3))
exp(1)
uk.df  <- as.data.frame(Tlog_uk)
uk.df$Tuk  <- 10^uk.df$var1.pred
uk.df$TukCol  <- as.factor(round(uk.df$Tuk ))
head(uk.df)
summary(uk.df)
#ge.df2csv(uk.df)
#### 3D
if(require(scatterplot3d)){
        scatterplot3d(uk.df[,c(1,2,3)])
}


getwd()
library(rgl)





str(Tlog_uk)

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



## 3D IDW

head(vgm())

## 3D variogram
vgmT <- variogram(t ~ x+y+z, spdf, cutoff = 2500)
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
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd/hkdbh_141201_110703.Rds")
hkd3dgrid  <- readRDS("~/Dropbox/2data//dataProduct/hkd/hkd15hgrid_141201_111200.Rds")
spdf  <- hkdbhs
grid  <- hkd3dgrid
vgmTlog <- variogram(Tlog ~ z, spdf,
                     boundaries = c(seq(100,1000,100), seq(2000,45000,5000)))
plot(vgmTlog)
v.eye1  <- vgm(psill = 0.155,  model = "Gau",  range=700,  nugget=0)
v.eye   <- vgm(psill = 0.125,  model = "Sph",  range=35000,  nugget=0,  add.to=v.eye1)
v.eye
fitLine.df  <- variogramLine(v.eye, maxdist = 45000)
summary(v.eye)
# plot.new()
# plot(vgmTlog, v.eye, col = "red", lwd = 3, cex.lab=1.2,
#      xlab = 'Distance [m]', xlim = c(0,40000),
#      ylab = expression("Semivariance [ (ln"~degree*C~")"^2~"]"), ylim = c(0, 0.3)
#      )
vplot  <- ggplot() +
    geom_line(data = fitLine.df, aes(x = dist, y = gamma), size = 1, color = "red") +
    geom_point(data = vgmTlog, aes(x = dist, y = gamma), color = "blue")
    #geom_text(aes(label=np))
vhline  <- vplot + geom_hline(yintercept = c(0.125, 0.28), linetype = 2, color = "green") +
        geom_vline(xintercept = c(700, 35000), linetype = 4, color = "orange" )
limitsX  <- c(0,45000)
breaksX  <- seq(limitsX[1], limitsX[2], 5000)
labelsX=c(breaksX/1000)
##limitsY  <- c(41,47)
limitsY  <- c(0,0.3)
breaksY  <- seq(limitsY[1],limitsY[2], 0.05)
labelsY=as.character(breaksY)

vbase  <- vhline +
xlab("Distance [km]") +
  scale_x_continuous(breaks=breaksX,
                     labels=labelsX,
                     limits=limitsX) +
ylab(expression("Semivariance [ (ln"~degree*C~")"^2~"]")) +
  scale_y_continuous(breaks=breaksY,
                     labels=labelsY,
                     limits=limitsY)

vtext  <-
  vbase +
  geom_text(aes(x = 30000, y = 0.29), label = "sill = 0.155", angle = 0,
            family = "Times") +
  geom_text(aes(x = 6000, y = 0.11), label = "sill = 0.125", angle = 0,
            family = "Times") +
  geom_text(aes(x = 44000, y = 0.06), label = "Gaussian model \n (Vertical)", angle = 90,
            family = "Times") +
  geom_text(aes(x = 44000, y = 0.21), label = "Spherical model \n (Lateral)", angle = 90,
            family = "Times") +
  geom_text(aes(x = 6000, y = 0.02), label = "Range = 700 m", family = "Times") +
  geom_text(aes(x = 30000, y = 0.18), label = "Range = 35 km", family = "Times")
# sd  <- sd(vgmTlog$gamma)
# vtext + geom_ribbon(data = vgmTlog, aes(x = dist, ymin=gamma -2*sd, ymax=gamma+2*sd),alpha=1)
hkdVariogram  <- vtext +
        theme_bw(base_size = 12, base_family = "Times") +
        theme(axis.title.x=element_text(vjust = -0.5))
hkdVariogram
ggsave(plot =hkdVariogram, "hkdVariogram.pdf", width = 7, height = 5)
# #ge.ggsave(hkdVariogram)
 getwd()
# ## grid$TlogUK.eye <- krige(Tlog~z, spdf, grid, model = v.eye)
# ##TlogUK <- krige(Tlog~z, spdf, grid, model = v.eye, nmin =2, nmax = 6)
# TlogUK <- krige(Tlog~1, spdf, grid, model = v.eye, nmin =6, nmax = 12)
# TlogUK
# grid.df  <- as.data.frame(TlogUK)
# grid.df$Tpred0  <- exp(grid.df$var1.pred)
# grid.df$Tpred1  <- exp(grid.df$var1.pred + 0.5*grid.df$var1.var)
# head(grid.df)
# hist(grid.df$var1.pred)
# summary(grid.df)
# ggplot(grid.df) +
#         geom_raster(aes(x = x, y =y, fill = Tpred1)) +
#         facet_wrap(~z)
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd/hkdbh_141201_110703.Rds")
hkd3dgrid  <- readRDS("~/Dropbox/2data//dataProduct/hkd/hkd15hgrid_141201_111200.Rds")
spdf  <- hkdbhs
grid  <- hkd3dgrid
### research logt
spdf$logt  <- log(spdf$t)
summary(spdf$t)
summary(exp(spdf$logt))
####
ok.vgm <- variogram(logt ~ 1, spdf,
                     boundaries = c(seq(100,1000,100), seq(2000,45000,5000)))
plot(ok.vgm)
###
uk.vgm <- variogram(logt ~ z, spdf,
                     boundaries = c(seq(100,1000,100), seq(2000,40000,5000)))
plot(uk.vgm)

show.vgms()
uk.eye1  <- vgm(psill = 0.125,  model = "Gau",  range=700,  nugget=0.002)
uk.eye   <- vgm(psill = 0.155,  model = "Sph",  range=30000,  add.to=uk.eye1)
uk.eye

plot(uk.vgm, model = uk.eye, plot.numbers = TRUE)
uk.fit  <- fit.variogram(uk.vgm, uk.eye, fit.sills = TRUE, fit.ranges = TRUE,
              fit.method = 7, debug.level = 1, warn.if.neg = FALSE)
uk.fit
plot(uk.vgm, uk.fit)
#g.trend  <- gstat(formula = logt ~ z, data = spdf, model = uk.eye)
# uk1  <- predict(g.trend, newdata = grid, debug.levle = -1, nmax = 20) # using universal kriging
# gls1   <-  predict(g.trend, newdata = grid, BLUE = TRUE, debug.levle = -1) # generalized least squares trend estimation
### UK
summary(spdf)
summary(grid)
logt.uk <- krige(log(t)~z, spdf, grid, model = uk.fit, nmax = 20)
summary((spdf$logt))
summary((logt.uk$var1.pred))
bubble(logt.uk, "var1.pred")
logt.uk.df  <- as.data.frame(logt.uk)
logt.uk.df$tback  <-  exp(logt.uk.df$var1.pred + 0.5*logt.uk.df$var1.var)
summary(logt.uk.df)
hist(logt.uk.df$tback)
ggplot(logt.uk.df]) +
        geom_raster(aes(x = x, y =y, fill = exp(var1.pred + 0.5*var1.var))) +
        facet_wrap(~z)
### Corss validataion
logt.uk.cv  <- krige.cv(logt ~ z, spdf, grid, model = uk.eye, nfold = 10)
summary(logt.uk.cv)
spplot(logt.uk.cv~, "zscore")
### Check CrossValidation
ge.cv <- function(cv, response){
        ### mean error, ideally should be 0
        me0  <- mean(cv$residual)
        me00  <- mean(cv$residual^2)

        rmse0  <- sqrt(mean(cv$residual^2))
        #rmsesd  <- rmse0/sd(response)
        ### corrlation observed and predicted, ideally 1
        #me1  <- mean(cv$residual^2/cv$var1.var)
        cor1  <- cor(cv$observed, cv$observed - cv$residual)
        ### corrlation predicted data and residual
        cor0  <- cor(cv$observed - cv$residual, cv$residual)
        ### Results
        results  <- c(me0, me00, rmse0, rmsesd,cor1,cor0)
        names(results)  <- c("Mean error=0", "Mean suqred error=0", "rmse=0","rmsesd=0",
                             "correlation=1", "correlation=0")
        return(results)
}

ge.cv(logt.uk.cv, spdf$tlog)
sd(spdf@data$tlog)
### UK plot

uk.df  <- as.data.frame(logt.uk)
summary((spdf$logt))
summary((logt.uk$var1.pred))
plot(spdf$logt, logt.uk$var1.pred)
hist(uk.df$var1.pred)
hist(as.numeric(spdf@data$tlog))
uk.df$Tpred0  <- exp(grid.df$var1.pred)
uk.df$Tpred1  <- exp(grid.df$var1.pred + 0.5*grid.df$var1.var)
head(grid.df)
hist(grid.df$var1.pred)
summary(grid.df)
ggplot(grid.df) +
        geom_raster(aes(x = x, y =y, fill = Tpred1)) +
        facet_wrap(~z)
logt.uk.cv  <- krige.cv(logt ~ z, spdf, grid, model = uk.eye, nfold = 10)


uk.g  <- gstat(id = "tlog", formula = logt ~ z, data = spdf, model = uk.eye)

blue0  <- predict(uk.g, newdata = spdf, BLUE =TRUE, debug.level =1 )
## grid$TlogUK.eye <- krige(Tlog~z, spdf, grid, model = v.eye)
##TlogUK <- krige(Tlog~z, spdf, grid, model = v.eye, nmin =2, nmax = 6)
TlogUK <- krige(Tlog~1, spdf, grid, model = uk.eye, nmin =6, nmax = 12)
TlogUK
exp(log(Tlog$t))
grid.df  <- as.data.frame(TlogUK)
grid.df$Tpred0  <- exp(grid.df$var1.pred)
grid.df$Tpred1  <- exp(grid.df$var1.pred + 0.5*grid.df$var1.var)
head(grid.df)
hist(grid.df$var1.pred)
summary(grid.df)
ggplot(grid.df) +
        geom_raster(aes(x = x, y =y, fill = Tpred1)) +
        facet_wrap(~z)

### Trend Residauls and OK
# trend.lm  <- lm(log(t) ~ z, spdf)
# summary(trend.lm) ## how well does the model explain the values
# round(summary(trend.lm$residuals),3)
# summary(log(spdf$t))
# plot(vgmTuk)
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
uk  <- readRDS("~/Share500sda/result/uk.df_141010_111357.Rds")
head(uk)
c <- cut(uk$Tuk,breaks=255)
cols <- rainbow(255)[as.numeric(c)]

library(ggplot2)
library(lattice)
levelplot(var1.pred ~ x+y | z,uk)
library(rgl)
open3d()
plot3d(uk$x, uk$y, uk$z, col=cols,type="p",size=1)
surface3d(uk$x, uk$y, uk$z, col=cols,type="p",size=1)
library(plot3D)
plot3D()
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
uk  <- readRDS("~/Share500sda/result/uk.df_141010_111357.Rds")
head(uk)
c <- cut(uk$Tuk,breaks=255)
cols <- rainbow(255)[as.numeric(c)]

library(ggplot2)
library(lattice)
levelplot(var1.pred ~ x+y | z,uk)
library(rgl)
open3d()
plot3d(uk$x, uk$y, uk$z, col=cols,type="p",size=1)
surface3d(uk$x, uk$y, uk$z, col=cols,type="p",size=1)
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


library(geoR)
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd/hkdbh_141201_110703.Rds")
hkd3dgrid  <- readRDS("~/Dropbox/2data//dataProduct/hkd/hkd15hgrid_141201_111200.Rds")
spdf  <- hkdbhs
grid  <- hkd3dgrid
geodata  <- as.geodata(spdf)
### research logt
spdf$logt  <- log(spdf$t)
summary(spdf$t)
summary(exp(spdf$logt))
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
bhdir  <- "~/Dropbox/2data/dataRaw/boreholes/" #1250
setwd(bhdir)
# bh  <- as.data.frame(readRDS("jpBoreholeOK_141014_154924.Rds"))
# bh2 <- ge.crsTransform(bh, x, y, xlcc, ylcc, wgs84GRS, lccWgs84)
# coordinates(bh)  <- ~x+y
# proj4string(bh)  <- CRS(wgs84GRS)
#
# spdf <- spTransform(bh,CRS(lccWgs84))
# df  <- as.data.frame(spdf)
# jpBoreholeOK  <- df[,c(1:4, 6:33, 35,36)]
# ge.df2csv(jpBoreholeOK)
# plot(spdf)
bh.df0 <- as.data.frame(readRDS("jpBoreholeOK_141015_152340.Rds")) #85545

names(bh.df0)
## simple

d0  <- as.numeric(bh.df0$Depths)
bh.df  <- bh.df0[d0 >=100 & d0 <=1500,] #74804
x  <- as.numeric(bh.df$x)
y  <- as.numeric(bh.df$y)
z <- as.numeric(bh.df$Depths)
t  <- as.numeric(bh.df$Temperature)
tlog  <- as.numeric(log(t))
### Normality check

plot(t,z)

xyzv  <- as.data.frame(cbind(x,y,z,t,tlog))
###  Trend Surface Analysis
# plot(xyzv)
# pairs(xyzv)
# class(xyzv)
df  <- na.omit(xyzv)
### remove duplicated spatial point
summary(df)
spdf  <- df
coordinates(spdf) <- ~x+y+z
proj4string(spdf)  <- CRS(lccWgs84)
zerodist(spdf)
spdf0 <- remove.duplicates(spdf)
spdf <- spdf0
zerodist(spdf) ##check again
vgmTlog <- variogram(tlog ~ z, spdf,
                     boundaries = c(seq(100,1000,100), seq(10000,45000,5000)))
plot(vgmTlog)
show.vgms()
v.eye1  <- vgm(psill = 0.155,  model = "Gau",  range=700,  nugget=0)
v.eye   <- vgm(psill = 0.4,  model = "Exc",  range=30000,  nugget=0,  add.to=v.eye1)
v.eye
plot(vgmTlog, v.eye)
fitLine.df  <- variogramLine(v.eye, maxdist = 45000)
summary(v.eye)
# plot.new()
# plot(vgmTlog, v.eye, col = "red", lwd = 3, cex.lab=1.2,
#      xlab = 'Distance [m]', xlim = c(0,40000),
#      ylab = expression("Semivariance [ (ln"~degree*C~")"^2~"]"), ylim = c(0, 0.3)
#      )
vplot  <- ggplot() +
        geom_line(data = fitLine.df, aes(x = dist, y = gamma), size = 1, color = "red") +
        geom_point(data = vgmTlog, aes(x = dist, y = gamma), color = "blue")
vplot
#geom_text(aes(label=np))
vhline  <- vplot + geom_hline(yintercept = c(0.155, 0.4), linetype = 2, color = "green") +
        geom_vline(xintercept = c(700, 30000), linetype = 4, color = "orange" )
limitsX  <- c(0,45000)
breaksX  <- seq(limitsX[1], limitsX[2], 5000)
labelsX=c(breaksX/1000)
##limitsY  <- c(41,47)
limitsY  <- c(0,0.5)
breaksY  <- seq(limitsY[1],limitsY[2], 0.05)
labelsY=as.character(breaksY)

vbase  <- vhline +
        xlab("Distance [km]") +
        scale_x_continuous(breaks=breaksX,
                           labels=labelsX,
                           limits=limitsX) +
        ylab(expression("Semivariance [ (ln"~degree*C~")"^2~"]")) +
        scale_y_continuous(breaks=breaksY,
                           labels=labelsY,
                           limits=limitsY)

vtext  <-
        vbase +
        geom_text(aes(x = 20000, y = 0.42), label = "sill = 0.275", angle = 0,
                  family = "Times") +
        geom_text(aes(x = 6000, y = 0.14), label = "sill = 0.155", angle = 0,
                  family = "Times") +
        geom_text(aes(x = 46000, y = 0.07), label = "Gaussian model \n (Vertical)", angle = 90,
                  family = "Times") +
        geom_text(aes(x = 46000, y = 0.28), label = "Exponential class model \n (Lateral)", angle = 90,
                  family = "Times") +
        geom_text(aes(x = 6000, y = 0.02), label = "Range = 700 m", family = "Times") +
        geom_text(aes(x = 25000, y = 0.276), label = "Range = 30 km", family = "Times")
# sd  <- sd(vgmTlog$gamma)
# vtext + geom_ribbon(data = vgmTlog, aes(x = dist, ymin=gamma -2*sd, ymax=gamma+2*sd),alpha=1)
jp3dVariogram  <- vtext +
        theme_bw(base_size = 12, base_family = "Times") +
        theme(axis.title.x=element_text(vjust = -0.5))
jp3dVariogram
ggsave(plot =jp3dVariogram, "jp3DVariogram.pdf", width = 7, height = 5)
# # #ge.ggsave(hkdVariogram)
# getwd()
# ## grid$TlogUK.eye <- krige(Tlog~z, spdf, grid, model = v.eye)
# ##TlogUK <- krige(Tlog~z, spdf, grid, model = v.eye, nmin =2, nmax = 6)
# TlogUK <- krige(Tlog~1, spdf, grid, model = v.eye, nmin =6, nmax = 12)
# TlogUK
# grid.df  <- as.data.frame(TlogUK)
# grid.df$Tpred0  <- exp(grid.df$var1.pred)
# grid.df$Tpred1  <- exp(grid.df$var1.pred + 0.5*grid.df$var1.var)
# head(grid.df)
# hist(grid.df$var1.pred)
# summary(grid.df)
# ggplot(grid.df) +
#         geom_raster(aes(x = x, y =y, fill = Tpred1)) +
#         facet_wrap(~z)


# fit.variogram(vgmTlog,v.eye)
### grid
# grid  <- hkd3dgrid
# coordinates(grid) <- ~x+y+z
# proj4string(grid)  <- CRS(lccWgs84)
# gridded(grid) <- TRUE
#
# grid.sp  <- as(grid, "SpatialPoints")
# grid.df  <- data.frame(as.factor(1:length(grid.sp)))
# grid.spdf <- sp::SpatialPointsDataFrame(grid.sp, grid.df)
# hkd15hgrid  <-grid.spdf
# ge.sp2shpPrj(hkd15hgrid)
# ge.sp2shpPrj(bh.df)
# getwd()
# Tlog.trend  <-  krige(Tlog ~ x + y + z , spdf, grid)
# Tlog.trend.df  <- as.data.frame(Tlog.trend)
# levelplot(data = Tlog.trend.df, exp(var1.pred) ~ x*y| z,  contour=TRUE)
# #rgl::surface3d(Tlog.trend.df$x, Tlog.trend.df$y, Tlog.trend.df$z, col = rainbow(10))
# ###
# Tlog.trend.lm <- lm(Tlog ~   z, spdf)
# summary(Tlog.trend.lm)
#
# plot(Tlog.trend.lm)



# head(grid)
# ### log plot
# vgmTlog <- variogram(tlog ~ 1, spdf, cutoff = 100000)
# vgmTlog
# vgmTlog <- variogram(Tlog ~ 1, spdf,
#                      boundaries = c(seq(100,1000,100), seq(2000,10000,1000)))
# vgmTlog <- variogram(Tlog ~ z, spdf,
#                      boundaries = c(seq(100,1000,100), seq(2000,45000,5000)))
# plot(vgmTlog)
# ## Universal Kriging
# #vgmTlog <- variogram(Tlog ~ z^3, spdf, cutoff = 50000,cloud = T)
# #vgmTlog <- variogram(logT ~ z, spdf, cutoff = 2500, alpha = c(0,45,90,135), beta = c(0,45,90,135))
# # Trend
# #trend  <- gstat(NULL, "trend", logT ~ z^2, data = spdf)
# #vgmTlog  <- variogram(trend, cutoff = 2500)
# show.vgms()
#
#
# v.eye  <- vgm(0.28, "Pen", 35000, 0)
# v.eye1  <- vgm(psill = 0.16,  model = "Gau",  range=700,  nugget=0)
# v.eye   <- vgm(psill = 0.13,  model = "Sph",  range=35000,  nugget=0,  add.to=v.eye1)
# v.eye
# plot(vgmTlog, v.eye)
#
# vgmGauTlog <- fit.variogram(vgmTlog, v.eye)
# vgmGauTlog
# plot(vgmTlog, vgmGauTlog)
# plot(vgmTlog, v.eye)
# attr(vgmGauTlog, "SSErr")
# grid$TlogUK.eye <- krige(Tlog~z, spdf, grid, model = v.eye)
# show.vgms
#
# ### post-krige
# log(exp(3))
# exp(1)
# uk.df  <- as.data.frame(Tlog_uk)
# uk.df$Tuk  <- 10^uk.df$var1.pred
# uk.df$TukCol  <- as.factor(round(uk.df$Tuk ))
# head(uk.df)
# summary(uk.df)
# #ge.df2csv(uk.df)
# #### 3D
# if(require(scatterplot3d)){
#         scatterplot3d(uk.df[,c(1,2,3)])
# }
#
#
# getwd()
# library(rgl)
#
#
#
#
#
# str(Tlog_uk)
#
# vgmSphTlog <- fit.variogram(vgmTlog, vgm(0.08, "Sph", 2500, 0))
# vgmSphTlog
# plot(vgmTlog, vgmSphTlog)
# attr(vgmSphTlog, "SSErr")
#
# vgmExpTlog <- fit.variogram(vgmTlog, vgm(0.08, "Exp", 2500, 0))
# vgmExpTlog
# plot(vgmTlog, vgmExpTlog)
# attr(vgmExpTlog, "SSErr")
# vgmLinTlog <- fit.variogram(vgmTlog, vgm(0.08, "Lin", 2500, 0))
# vgmLinTlog
# plot(vgmTlog, vgmLinTlog)
# attr(vgmLinTlog, "SSErr")
# vgmMatTlog <- fit.variogram(vgmTlog, vgm(0.08, "Mat", 2500, 0, kappa=3))
# vgmMatTlog
# plot(vgmTlog, vgmMatTlog)
# attr(vgmMatTlog, "SSErr")
#
#
# vgmTlogok  <- fit.variogram(vgmTlog, vgmSphTlog)
# vgmTlogok
# plot(vgmTlog, vgmTlogok)
#
#
#
# ## 3D IDW
#
# head(vgm())
#
# ## 3D variogram
# vgmT <- variogram(t ~ x+y+z, spdf, cutoff = 2500)
# vgmT
# plot(vgmT)
#
# vgmSphT <- fit.variogram(vgmT, vgm(3000, "Sph", 2500, 0))
# vgmSphT
# plot(vgmT, vgmSphT)
# attr(vgmSphT, "SSErr")
# vgmExpT <- fit.variogram(vgmT, vgm(3000, "Exp", 2500, 0))
# vgmExpT
# plot(vgmT, vgmExpT)
# attr(vgmExpT, "SSErr")
# vgmLinT <- fit.variogram(vgmT, vgm(3000, "Lin", 2500, 0))
# vgmLinT
# plot(vgmT, vgmLinT)
# attr(vgmLinT, "SSErr")
# vgmMatT <- fit.variogram(vgmT, vgm(3000, "Mat", 2500, 0, kappa=3))
# vgmMatT
# plot(vgmT, vgmMatT)
# attr(vgmMatT, "SSErr")
#
# vgmTok  <- fit.variogram(vgmT, vgmSphT)
# vgmTok
# plot(vgmT, vgmTok)
wireframe(volcano, drape = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10,0,10),
          col.regions = colorRampPalette(c("blue", "pink"))(100))
class(volcano)
library(GISTools)
data(newhaven)
# Do the KDE
breach.dens = kde.points(breach,lims=tracts)
# Plot the result
level.plot(breach.dens)
# Block out the part outside the study area
masker = poly.outer(breach.dens,tracts,extend=100); add.masking(masker)
# Plot census tract boundaries
plot(tracts,add=TRUE)
x <- c(0, 10, 0, 0)
y <- c(0, 0, 100, 0)
z <- c(0, 0, 0, 1)
i <- c(1,2,1,3,1,4)
labels <- c("Origin", "X", "Y", "Z")
text3d(x,y,z,labels)
segments3d(x[i],y[i],z[i])
dfr <- data.frame(x = 1:10, y = (1:10)^2, z = runif(10), col = rainbow(10))
with(dfr, points3d(x, y, z, col = col))
library(rgl)
library(mapdata)
library(colorspace)
# mapdataの図形を読み込んでXYZ座標系に変換
d <- map("world",plot=F)
head(d)
xyz <- mapply(XYZ,d$x,d$y)
# 描画
open3d()
plot3d(xyz[1,],xyz[2,],xyz[3,],t="l",
       col="blue",axes=F,add=T)
