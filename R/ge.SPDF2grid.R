ge.SPDF2grid  <- function(SPDF,x, y = x, type = "regular"){
        sp  <- sp::spsample(SPDF,type = type, cellsize = c(x,y), offset =c(0.5, 0.5))
        df  <- data.frame(id = as.factor(1:length(sp)))
        spdf  <- sp::SpatialPointsDataFrame(sp, data = df)
}

