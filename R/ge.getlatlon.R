ge.geoCode <- function(nameLst) {
          # get longitude, latitude, and Name of given place
          lonlata  <- ggmap::geocode(nameLst, output = "latlona")
          names <- do.call("rbind", strsplit(lonlata[,3], ","))
          name  <- Hmisc::capitalize(names[,1])
          lonlatn  <- cbind(lonlata[,c(1,2)], name)
          return(lonlatn)
}
