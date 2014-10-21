ge.xy2bboxSPDF <- function(xmin,xmax,ymin,ymax,crs = NA){
        ### Make a bbox rect SPDF from LL and TR point.
        xmin  <- as.numeric(xmin)
        xmax <- as.numeric(xmax)
        ymin  <- as.numeric(ymin)
        ymax  <- as.numeric(ymax)
        Polygon  <- sp::Polygon(cbind(c(xmin, xmin, xmax, xmax, xmin),
                           c(ymin, ymax, ymax, ymin, ymin)))
        Polygons  <- sp::Polygons(list(Polygon), 1) #ID for row.names = 1
        SP  <- sp::SpatialPolygons(list(Polygons))
        data_d  <- data.frame(name = "bbox", row.names= row.names(Polygons))
        SPDF  <- sp::SpatialPolygonsDataFrame(SP, data = data_d)
        sp::proj4string(SPDF)  <- crs
        return(SPDF)
}
