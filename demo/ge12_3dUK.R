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
                     boundaries = c(seq(100,1000,100), seq(2000,45000,5000)))
plot(uk.vgm)


uk.eye1  <- vgm(psill = 0.155,  model = "Gau",  range=700,  nugget=0)
uk.eye   <- vgm(psill = 0.125,  model = "Sph",  range=35000,  nugget=0,  add.to=uk.eye1)
uk.eye
plot(uk.vgm, model = uk.eye, plot.numbers = TRUE)
g.trend  <- gstat(formula = logt ~ z, data = spdf, model = uk.eye)
# uk1  <- predict(g.trend, newdata = grid, debug.levle = -1, nmax = 20) # using universal kriging
# gls1   <-  predict(g.trend, newdata = grid, BLUE = TRUE, debug.levle = -1) # generalized least squares trend estimation
### UK
logt.uk <- krige(log(t)~z,  data = spdf@data, newdata = grid, model = uk.eye, nmax = 20)

### UK plot
uk.df  <- as.data.frame(logt.uk)
summary(exp(spdf$logt))
summary(exp(logt.uk$var1.pred))
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
