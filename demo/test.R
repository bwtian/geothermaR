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
