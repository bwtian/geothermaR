source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd/hkdbh_141201_110703.Rds")
hkd3dgrid  <- readRDS("~/Dropbox/2data//dataProduct/hkd/hkd15hgrid_141201_111200.Rds")
summary(hkdbhs)
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

show.vgms()
uk.eye1  <- vgm(psill = 0.155,  model = "Gau",  range=700,  nugget=0.001)
uk.eye   <- vgm(psill = 0.125,  model = "Sph",  range=35000,  nugget=0,  add.to=uk.eye1)
uk.eye
plot(uk.vgm, model = uk.eye, plot.numbers = TRUE)
#g.trend  <- gstat(formula = logt ~ z, data = spdf, model = uk.eye)
# uk1  <- predict(g.trend, newdata = grid, debug.levle = -1, nmax = 20) # using universal kriging
# gls1   <-  predict(g.trend, newdata = grid, BLUE = TRUE, debug.levle = -1) # generalized least squares trend estimation
### UK and Cross validataion
summary(spdf)
summary(grid)
logt.uk <- krige(log(t)~z, spdf, grid, model = uk.eye, nmax = 20)
summary((spdf$logt))
summary((logt.uk$var1.pred))
logt.uk.cv  <- krige.cv(logt ~ z, spdf, grid, model = uk.eye, nfold = 10)
summary(logt.uk.cv)
### Check CrossValidation
ge.cv <- function(cv, response){
        ### mean error, ideally should be 0
        me  <- mean(cv$residual)
        rmse  <- sqrt(mean(cv$residual^2))
        rmsesd  <- rmse/sd(response)
        ### corrlation observed and predicted, ideally 1
        ### Results
        results  <- c(me,rmse, rmsesd)
        names(results)  <- c("mean error","rmse","rmsesd")
        return(results)
}

ge.cv(logt.uk.cv, spdf$tlog)
sd(spdf$tlog)
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
