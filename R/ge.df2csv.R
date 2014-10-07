ge.df2csv <- function(df) {
        ## write data.frame as csv and rds file into data folder using df name itselt
        now <- format(Sys.time(), "_%y%m%d_%H%M%S")
        csvName  <- paste(deparse(substitute(df)), now, ".csv", sep = "")
        rdsName  <- paste(deparse(substitute(df)), now, ".Rds", sep = "")
        write.table(df, csvName, sep = ",", quote = F, row.names = F)
        saveRDS(df, rdsName)
}
