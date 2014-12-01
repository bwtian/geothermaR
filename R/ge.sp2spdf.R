ge.sp2spdf  <- function(sp){
  sp  <- as(grid, "SpatialPoints")
  df  <- data.frame(as.factor(1:length(sp)))
  spdf <- sp::SpatialPointsDataFrame(sp, df)
}
