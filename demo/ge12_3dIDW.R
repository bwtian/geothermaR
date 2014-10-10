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

