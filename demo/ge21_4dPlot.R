source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
uk  <- readRDS("~/Share500sda/result/uk.df_141010_111357.Rds")
library(rgl)
c <- cut(uk$Tuk,breaks=255)
cols <- rainbow(255)[as.numeric(c)]
open3d()
plot3d(uk$x, uk$y, uk$z, col=cols,type="s",size=1)
