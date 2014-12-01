source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# load data and grid
hkdbhs  <- readRDS("~/Dropbox//2data//dataProduct//hkd/hkdbh_141201_110703.Rds")
hkd3dgrid  <- readRDS("~/Dropbox/2data//dataProduct/hkd/hkd15hgrid_141201_111200.Rds")
spdf  <- hkdbhs
grid  <- hkd3dgrid
vgmTlog <- variogram(Tlog ~ z, spdf,
                     boundaries = c(seq(100,1000,100), seq(2000,45000,5000)))
plot(vgmTlog)
v.eye1  <- vgm(psill = 0.16,  model = "Gau",  range=700,  nugget=0)
v.eye   <- vgm(psill = 0.13,  model = "Sph",  range=35000,  nugget=0,  add.to=v.eye1)
v.eye
summary(v.eye)
plot(vgmTlog, v.eye, color = "blue", 
     xlab = 'Distance [m]', 
     ylab = expression("Semivariance [(ln"~degree*C~")"^2~"]"),
     )

vgm  <- v.eye
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

grid$TlogUK.eye <- krige(Tlog~z, spdf, grid, model = v.eye)
