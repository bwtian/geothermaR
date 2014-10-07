ge.LargestPolys  <- function(SPDF, Polygon = FALSE, Polygons = FALSE) {
        ### TODO: lvl = match.type("Polygons", "Polygon", "polygons")
        ### Extract Largest Polygons(Layer) from SPDF to SPDF1 08-14
        if (class(SPDF)[1] != "SpatialPolygonsDataFrame")
        stop("Must be a SpatialPolygonsDataFrame object")
        crs  <- sp::proj4string(SPDF)
        areas_Polygons  <-  sapply(SPDF@polygons, function(Polygons) Polygons@area)
        id_Polygons  <- which.max(areas_Polygons)
        SPDF1  <- SPDF[id_Polygons,]
        ### Extract Largest Polygon to SPDF2
        area_fun  <- function(polygons) {
                sapply(polygons@Polygons, function(Polygon) Polygon@area)
                }
        areas_Polygon <- lapply(SPDF@polygons, area_fun)
        mx_Polygons <- areas_Polygon[[id_Polygons]]
        id_Polygon  <- which.max(mx_Polygons)
        mx_Polygon  <- SPDF@polygons[[id_Polygons]]@Polygons[[id_Polygon]]
        toPolygons  <- sp::Polygons(list(mx_Polygon), id_Polygon) #ID for row.names = 1
        toSP  <- sp::SpatialPolygons(list(toPolygons))
        SPDF2  <- as(toSP, "SpatialPolygonsDataFrame")
        ### Extract Largest Polygon in each Polygons to SPDF3
        ids_mxpolys <- lapply(areas_Polygon, function(x) which.max(x))
        Polygons_l  <- list()
        for (i in 1:length(ids_mxpolys)) {
             Polygons_l[[i]] <- SPDF@polygons[[i]]@Polygons[[ids_mxpolys[[i]]]]
        }
        toPolygons  <- sp::Polygons(Polygons_l, 1) #ID for row.names = 1
        toSP  <- sp::SpatialPolygons(list(toPolygons))
        SPDF3  <- as(toSP, "SpatialPolygonsDataFrame")
        sp::proj4string(SPDF1)  <- crs
        sp::proj4string(SPDF2)  <- crs
        sp::proj4string(SPDF3)  <- crs
        if (Polygons) {
                return(SPDF3)
        } else if (Polygon) {
                return(SPDF2)
        } else {
                return(SPDF1)
        }

}
