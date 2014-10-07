ge.xy2fishnet  <- function(x1,x2,y1,y2,rx=1000,ry=rx) {
        x.range.v <- seq(as.numeric(x1),as.numeric(x2),by = rx)
        y.range.v <- seq(as.numeric(y1),as.numeric(y2),by = ry)
        grid.m    <- outer(x.range.v, y.range.v, paste, sep = ",")
        grid.v    <- as.vector(grid.m)
        grid.d    <- as.data.frame(grid.v)
        fishnet.d <- data.frame(do.call('rbind', strsplit(as.character(grid.d[,1]), ',', fixed=TRUE)))
        colnames(fishnet.d)  <- c("x","y")
}
