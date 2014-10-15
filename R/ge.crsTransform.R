ge.crsTransform  <- function(df, x, y, xName, yName, fromCRS, toCRS) {
      ### 140510 transfer CRS in a dataframe fromat, x, y will be repaced in data.frame
    df$x  <- as.numeric(df[,which(colnames(df) == as.character(substitute(x)))])
    df$y  <- as.numeric(df[,which(colnames(df) == as.character(substitute(y)))])
    require(sp)
    require(rgdal)
    coordinates(df)  <- c("x", "y")
    proj4string(df)  <- CRS(fromCRS)
    df <- spTransform(df,CRS(toCRS))
    df <- as.data.frame(df)
    names(df)[which(names(df) == "x")] <- as.character(substitute(xName))
    names(df)[which(names(df) == "y")] <- as.character(substitute(yName))
    return(df)
}
