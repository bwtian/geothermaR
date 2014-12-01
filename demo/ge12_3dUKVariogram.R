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
vhline  <- vplot + geom_hline(yintercept = c(0,0.155, 0.28), linetype = 2) +
        geom_vline(xintercept = c(700, 35000), linetype = 3) 
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
  geom_text(aes(x = 36000, y = 0.08), label = "sill = 0.155", angle = 90, 
            family = "Times") +
  geom_text(aes(x = 36000, y = 0.21), label = "sill = 0.125", angle = 90, 
            family = "Times")

arrows(10000,0,10000,pGau,code=3,col="green") 
text(2,,pGau/2,paste("Nugget variance =",round(pGau,2)),cex=0.9,pos=4) 

abline(h=pGau,col=gray(0.6),lty=2) 

arrows(300,n,300,n+s,length=0.08,code=3,col=gray(0.6)) 
text(298,n+s/2,paste("Sill =",round(s,2)),cex=0.9,pos=2) 

arrows(0,1.3,r,1.3,length=0.08,code=3,col=gray(0.6)) 
text(r/2,1.3,paste("Range =",round(r,0)),cex=0.9,pos=3) 
dev.off() 

vgm  <- vgmTlog
ge.ggVariogram <- function(vgm){
        ggplot(vgm, aes(x=dist, y=gamma)) + 
          geom_point(color='blue')
        + 
          geom_text(aes(label=np)) +
        geom_smooth(fill='white', alpha=0) + 
        geom_ribbon(aes(ymin=gamma -2*sd,ymax=gamma+2*sd),alpha=0.2) +
        xlab('Distance') + ylab('Semivariogram') + 
        theme_bw()
}
ge.ggVariogram(v.eye)

## grid$TlogUK.eye <- krige(Tlog~z, spdf, grid, model = v.eye)
