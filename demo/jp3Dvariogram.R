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
                     boundaries = c(seq(100,1000,100), seq(5000,50000,5000)))
plot(vgmTlog)
show.vgms()
v.eye1  <- vgm(psill = 0.155,  model = "Gau",  range=700,  nugget=0)
v.eye   <- vgm(psill = 0.38,  model = "Exc",  range=35000,  nugget=0,  add.to=v.eye1)
v.eye
plot(vgmTlog, v.eye)
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



head(grid)
### log plot
vgmTlog <- variogram(tlog ~ 1, spdf, cutoff = 100000)
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
