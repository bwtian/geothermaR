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
### grid
grid  <- hkd3dgrid
coordinates(grid) <- ~x+y+z
proj4string(grid)  <- CRS(lccWgs84)
gridded(grid) <- TRUE
head(grid)
## 3D IDW
IDW1 <- idw(t ~ 1,spdf,grid, idp = 1)
IDW2 <- idw(t ~ 1,spdf,grid, idp = 2)
