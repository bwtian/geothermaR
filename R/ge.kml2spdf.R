#' Extracting Point Information from Kml to Dataframe

#' @export
#' @import sp
#' @author Bingwei Tian <bwtian@gmail.com>
ge.kml2spdf  <- function(kml, name = FALSE){
  ### Extracting Information from Kml to Dataframe [2014-06-26 Thu]
        cha <- readLines(kml)
        doc <- XML::htmlParse(cha)   # xmlParse not work here!!!
        nod <- XML::xpathApply(doc, "//description")
        des_d  <- as.data.frame(t(sapply(nod, function(x)unname(XML::xmlSApply(x, XML::xmlValue)))))
        xy_l  <- XML::xpathSApply(doc,  "//coordinates", XML::xmlValue)
        xy_d  <- as.data.frame(do.call("rbind", strsplit(xy_l,",")))
        names(xy_d)  <- c("lon","lat")
        if (name) {
                ### for same bad structure of KML
                name_d  <- as.data.frame(XML::xpathSApply(doc,  "//name", XML::xmlValue))
                names(name_d)  <- "Name"
                kml_d  <- cbind(name_d, xy_d, des_d)}
        else
                kml_d  <- cbind(xy_d, des_d)
        kml_d  <- kml_d[, colSums(kml_d != "") != 0] ## Clear Empty Column
        coords  <- kml_d[, c("lon","lat")]
        coords_m  <- as.matrix(coords) #sp need numeric matrix
        mode(coords_m)  <- "numeric"
        options(digits=15)  #based on orgin data
        kml_sp  <- sp::SpatialPoints(coords_m, proj4string = sp::CRS(wgs84GRS))
        kml_spdf <- sp::SpatialPointsDataFrame(coords_m, data =kml_d, proj4string=sp::CRS(wgs84GRS))
        return(kml_spdf)
        #phd.saveshp.geo(kml_spdf)
}
