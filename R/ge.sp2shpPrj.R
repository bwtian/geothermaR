ge.sp2shpPrj <- function(obj) {
        ## write sp objects as shp, rds and csv file
        now <- format(Sys.time(), "_%y%m%d_%H%M%S")
        csvName  <- paste(deparse(substitute(obj)), now, ".csv", sep = "")
        rdsName  <- paste(deparse(substitute(obj)), now, ".Rds", sep = "")
        shpName  <- paste(deparse(substitute(obj)), now, sep = "")
        write.table(as.data.frame(obj), csvName, sep = ",", quote = F, row.names = F)
        saveRDS(obj, rdsName)
        if(require(rgdal)){
                writeOGR(obj, dsn = '.', layer = shpName, driver = "ESRI Shapefile")
        }
}