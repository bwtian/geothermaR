ge.conAreaSPDF  <- function(SPDF, area) {
        ### Extract Polygons(Layer) by Area to SPDF1
        crs  <- sp::proj4string(SPDF)
        areas_Polygons  <-  sapply(SPDF@polygons, function(Polygons) Polygons@area)
        ids_polygons  <- which(areas_Polygons >= area)
        SPDF1  <- SPDF[ids_polygons,]
}
##sp::plot(phd.spAreaSPDF(SPDF,1))
