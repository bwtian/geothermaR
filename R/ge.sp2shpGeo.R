ge.sp2shpGeo <- function(obj) {
        ## write sp object as shp, kml rds and csv file
        now <- format(Sys.time(), "_%y%m%d_%H%M%S")
        csvName  <- paste0(deparse(substitute(obj)), now, ".csv")
        rdsName  <- paste0(deparse(substitute(obj)), now, ".Rds")
        pngName  <- paste0(deparse(substitute(obj)), now, ".png")
        shpName  <- paste0(deparse(substitute(obj)), now)
        kmlName  <- paste0(deparse(substitute(obj)), now)
        kmlDsn  <-  paste0("./", kmlName, ".kml")
        write.table(as.data.frame(obj), csvName, sep = ",", quote = F, row.names = F)
        saveRDS(obj, rdsName)
        png(pngName); plot(obj);dev.off()
        if(require(rgdal)){
                writeOGR(obj, dsn = '.', layer = shpName, driver = "ESRI Shapefile")
                writeOGR(obj, dsn = kmlDsn, layer = kmlName, driver = "KML")
        }
}