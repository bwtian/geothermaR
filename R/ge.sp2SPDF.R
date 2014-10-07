ge.sp2SPDF  <- function(sp) {
        df  <- data.frame(as.factor(1:length(sp)))
        spdf <- sp::SpatialPointsDataFrame(sp, df)
        grd  <- spdf
        sp::gridded(grd)  <- TRUE
        grd_SPDF  <- as(grd, "SpatialPolygonsDataFrame")
        return(grd_SPDF)
}
