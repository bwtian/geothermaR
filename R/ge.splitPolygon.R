ge.splitPoly  <- function(line,poly){
        x  <- gIntersection(line, poly)
        bx <- gBuffer(x, width = 0.000001)
        dx <- gDifference(poly, bx)
}