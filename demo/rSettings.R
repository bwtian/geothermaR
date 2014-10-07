library(sp)
library(rgdal)
library(gdalUtils)
library(maptools)
library(raster)
library(rasterVis)
library(plotKML)
library(gstat)
library(plyr)
library(rgeos)
library(ggplot2)
library(ggmap)

library(lattice)
library(latticeExtra)
# Extent of Study area of Japan in Phd Thesis
xlimJP <- c(128.5, 146.5)
ylimJP <- c(30.2, 45.8)
certerJp <- c(137.5, 38)
phd.rainbow <- grDevices::colorRampPalette(c("purple","blue","cyan","green","yellow", "orange","red"))
# Coordinate Reference Systems of Japan
## Geographic Coordinate Reference Systems
#tokyoGRS <- "+init=epsg:4301"
tokyoGRS  <- "+proj=longlat +ellps=bessel +no_defs"
#wgs84GRS <- "+init=epsg:4326"
wgs84GRS  <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
#jgd2000GRS <- "+init=epsg:4612"
jgd2000GRS  <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

## Projected Coordinate Reference Systems
lccBessel <- "+proj=lcc +lat_1=32.8 +lat_2=43.2 +lat_0=38 +lon_0=137.5 +x_0=1000000 +y_0#=1000000 +ellps=bessel +towgs84=-146.414,507.337,680.507,0,0,0,0 +units=m +no_defs"
## change the datum to WGS84 20140508
lccWgs84 <- "+proj=lcc +lat_1=32.8 +lat_2=43.2 +lat_0=38 +lon_0=137.5 +x_0=1000000 +y_0=1000000 +datum=WGS84 +units=m +no_defs"
#  +ellps=WGS84 +towgs84=0,0,0
codeDir <- "~/Dropbox/1code"
dataDir <- "~/Dropbox/2data"
dataData <- "~/Dropbox/2data/data"
dataRaw <- "~/Dropbox/2data/dataRaw"
dataPro <- "~/Dropbox/2data/dataProduct"
figsDir <- "~/Dropbox/3figs"
