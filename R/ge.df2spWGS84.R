

ge.df2spwgs84 <- function(df, x, y) {
         # spatialize a data frame to spdf
         d  <- df
         d$x  <- d[,which(colnames(d) == as.character(substitute(x)))]
         d$y  <- d[,which(colnames(d) == as.character(substitute(y)))]
         wgs84GRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
         coords  <- d[, c("x","y")]
         m  <- as.matrix(coords) #sp need numeric matrix
         mode(m)  <- "numeric"
         sp  <- sp::SpatialPoints(m, proj4string = sp::CRS(wgs84GRS))
         spdf <- sp::SpatialPointsDataFrame(m, data = d, proj4string=sp::CRS(wgs84GRS))
         return(spdf)
  }