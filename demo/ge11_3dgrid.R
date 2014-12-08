source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
# raster2d <- readRDS("~/Dropbox/2data/hkd//hkdmskbig_grdi2d1h.Rds")
# point2d  <- rasterToPoints(raster2d)
# xy <- point@coords
# xyz <- list()
# for (i in 1:10) {
#         xyz[[i]] <- cbind(xy,rep(i*100, nrow(xy), simplify=F))
#
# }
# grid3d <- as.data.frame(do.call(rbind, xyz))
# colnames(grid3d) <- c("x","y","z")
# grid3d$value <- 10
# plot(grid3d)
# write.csv(grid3d, "grid3d.csv",quote = F, row.names = F) ## save to csv
# #' Make 2D 100m * 100m Grid with lccWgs84 CRS
#'
## BBOX Raster
P  <- Polygon(cbind(c(1152500, 1152500, 1673500, 1673500, 1152500),
                     c(1372500, 1843500, 1843500, 1372500, 1372500)))
# P  <- Polygon(cbind(c(1152550, 1152550, 1673550, 1673550, 1152550),
#                     c(1372550, 1843550, 1843550, 1372550, 1372550)))
Ps  <- Polygons(list(P), "P")
SpP  <- SpatialPolygons(list(Ps))
grid2d1k  <- spsample(SpP,type = "regular", cellsize = c(1000,1000),
                      offset = c(0.5, 0.5))
proj4string(grid2d1k) <- CRS(lccWgs84)
# bbraster1h  <- rasterFromXYZ(bbgrid1h)
# bbraster1h[]  <- 1: ncell(bbraster1h)
# proj4string(bbraster1h) <- CRS(lccWgs84)
# proj4string(bbraster1h)

# plot(bbraster1h)
# saveRDS(bbraster1h, file = "~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
# hkd1h  <- readRDS("~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
# proj4string(hkd1h)
## Hokkaido Shape Raster

dsn  <- "~/Dropbox/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"
hkdshp  <- readShapePoly(dsn)
proj4string(hkdshp) <- CRS(lccWgs84)
hkdLand <- ge.LargestPolys(hkdshp, Polygon = T)
plot(hkdLand)
proj4string(hkdLand)  <- CRS(lccWgs84)
hkdLandPoint1k  <-gIntersection(grid2d1k, hkdLand)
proj4string(hkdLandPoint1k)  <- CRS(lccWgs84)
# saveRDS(hkdmaskb, file = "~/Dropbox/2data/hkd/hkf1kpoint2d.Rds")
# head(hkdmaskb)
# hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
plot(hkdLandPoint1k)
plot(hkdLand, add = T)
hkdPoint2d1k.df  <- as.data.frame(hkdLandPoint1k)

l  <- list()
for (i in 1:15) {
        l[[i]] <- cbind(hkdPoint2d1k.df, z = rep(i*100, nrow(hkdPoint2d1k.df), simplify=F))

}
d  <- do.call(rbind, l)
names(d)  <- c("x","y","z")
saveRDS(d, file = "~/Dropbox/2data/hkd/hkd1k3d15h.df.Rds")
