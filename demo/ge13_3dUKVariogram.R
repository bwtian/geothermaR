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
vhline  <- vplot + geom_hline(yintercept = c(0.155, 0.28), linetype = 2, color = "green") +
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
  geom_text(aes(x = 6000, y = 0.14), label = "sill = 0.125", angle = 0,
            family = "Times") +
  geom_text(aes(x = 44000, y = 0.08), label = "Gaussian model \n (Vertical)", angle = 90,
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
