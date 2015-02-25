ge.cleaupDir  <- function(dir=getwd()){
        setwd(dir)
        files  <- list.files()
        exts  <- tools::file_ext(file)
        extsu  <- unique(exts)
        for (i in extsu){
                 if (i != ""){
                        dir.create(i)        
                }
                for (j in files) {
                        if(tools::file_ext(j) == i){
                                file.copy(j, i)
                                file.remove(j)
                        }    
                }
        }
}
ge.crsTransform  <- function(df, x, y, xName, yName, fromCRS, toCRS) {
      ### 140510 transfer CRS in a dataframe fromat, x, y will be repaced in data.frame
       df$x  <- df[,which(colnames(df) == as.character(substitute(x)))]
       df$y  <- df[,which(colnames(df) == as.character(substitute(y)))]
      library(sp)
      library(rgdal)
      coordinates(df)  <- c("x", "y")
      proj4string(df)  <- CRS(fromCRS)
      df <- spTransform(df,CRS(toCRS))
      df <- as.data.frame(df)
      names(df)[which(names(df) == "x")] <- as.character(substitute(xName))
      names(df)[which(names(df) == "y")] <- as.character(substitute(yName))
      return(df)
    }
ge.df2collars  <- function(ID, Easting, Northing, Elevation, Azimuth = 0, Dip = 0, Depth) {
      ID  <- ID
      well  <- as.data.frame(ID)
      well$Easting  <- Easting
      well$Northing  <- Northing
      well$Elevation  <- Elevation
      well$Azimuth  <- Azimuth
      well$Dip  <- Dip
      well$Depth  <- Depth
      well[sort(well$ID),]
      return(well)
    }ge.df2csv <- function(df) {
        ## write data.frame as csv and rds file into data folder using df name itselt
        now <- format(Sys.time(), "_%y%m%d_%H%M%S")
        csvName  <- paste(deparse(substitute(df)), now, ".csv", sep = "")
        rdsName  <- paste(deparse(substitute(df)), now, ".Rds", sep = "")
        write.table(df, csvName, sep = ",", quote = F, row.names = F)
        saveRDS(df, rdsName)
}


ge.df2spwgs84 <- function(df, x, y) {
         # spatialize a data frame to spdf
         d  <- df
         d$x  <- d[,which(colnames(d) == as.character(substitute(x)))]
         d$y  <- d[,which(colnames(d) == as.character(substitute(y)))]
         wgs84GRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
         coords  <- d[, c("x","y")]
         m  <- as.matrix(coords) #sp need numeric matrix
         mode(m)  <- "numeric"
         sp  <- sp::SpatialPoints(m, proj4string = sp::CRS(wgs84GRS))
         spdf <- sp::SpatialPointsDataFrame(m, data = d, proj4string=sp::CRS(wgs84GRS))
         return(spdf)
  }ge.df2trajectories  <- function(ID, MD, Azimuth = 0, Inclination = 0) {
      ID  <- ID
      well  <- as.data.frame(ID)
      well$MD  <- MD
      well$Azimuth  <- Azimuth
      well$Inclination  <- Inclination
      well[sort(well$ID),]
      return(well)
    }
ge.df2voxlerSamples  <- function(ID, From, To, V1, ...) {
      ID  <- ID
      well  <- as.data.frame(ID)
      ## From to can get from phd.slice100m
      well$From  <- From
      well$To  <- To
      well$V1  <- V1
      well[sort(well$ID),]
      return(well)
    }
    ## Add linear function and R2 on the line in ggplot
    #http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph/7549819#7549819
ge.conAreaSPDF  <- function(SPDF, area) {
        ### Extract Polygons(Layer) by Area to SPDF1
        crs  <- sp::proj4string(SPDF)
        areas_Polygons  <-  sapply(SPDF@polygons, function(Polygons) Polygons@area)
        ids_polygons  <- which(areas_Polygons >= area)
        SPDF1  <- SPDF[ids_polygons,]
}
##sp::plot(phd.spAreaSPDF(SPDF,1))
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
ge.geoCode <- function(nameLst) {
          # get longitude, latitude, and Name of given place
          lonlata  <- ggmap::geocode(nameLst, output = "latlona")
          names <- do.call("rbind", strsplit(lonlata[,3], ","))
          name  <- Hmisc::capitalize(names[,1])
          lonlatn  <- cbind(lonlata[,c(1,2)], name)
          return(lonlatn)
}#' @usage ge.getGoogleMap(142.5, 43.5, 7, "hkd")
#'
ge.getGoogleMap <- function(lon, lat, zoom, prefix = "google"){
        ### ggmap 4 type of google map and save to Rds 08-15
        require(ggmap)
        x  <- deparse(substitute(lon))
        y  <- deparse(substitute(lat))
        z  <- deparse(substitute(zoom))
        now <- format(Sys.time(), "_%y%m%d_%H%M")
        for (i in c("terrain", "satellite", "roadmap", "hybrid")){
                fileName  <-  paste0(prefix,"_google_", i,"_",x,"_",y,"_zoom", z, now, ".Rds")
                file  <- get_googlemap(center = c(lon = lon, lat = lat), zoom = zoom,
                             maptype = i, filename = fileName)
                saveRDS(file, file = fileName)
         }
}
lm_eqn = function(m) {

        l <- list(a = format(coef(m)[1], digits = 2),
                  b = format(abs(coef(m)[2]), digits = 2),
                  r2 = format(summary(m)$r.squared, digits = 3));

        if (coef(m)[2] >= 0)  {
            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
        } else {
            eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
        }

        as.character(as.expression(eq));
    }
ge.ggsave <- function(gg, scale = 1, dpi = 300, ...) {
now <- format(Sys.time(), "_%y%m%d_%H%M%S")
## eps/ps, tex (pictex), pdf, jpeg, tiff, png, bmp, svg and wmf (windows only).
#pngName  <- paste0(deparse(substitute(name)), now, ".png")
# unit is inch
# if (paper == 1) {
#         width = 6.83
# } else if(paper == 2){
#         width = 3.27
# } else if (paper  == 3 ){
#         width = 2.09
# }
fileName  <- deparse(substitute(gg))
pngName  <- paste0(fileName, now, ".png")
tifName  <- paste0(fileName, now, ".tiff")
pdfName  <- paste0(fileName, now, ".pdf")
ggsave(pdfName, plot = gg, scale = scale, dpi = dpi)
ggsave(pngName, plot = gg, scale = scale, dpi = dpi)
ggsave(tifName, plot = gg, scale = scale, dpi = dpi)
}
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
ge.lm2df <- function(lm) {
      #wrap a lm resuts to data frame format
      cf <- coef(lm)
      tinfo <- summary(lm)$coefficients[2, c(2, 4)]
      r2 <- summary(lm)$r.squared
      data.frame(intercept = cf[1], slope = cf[2], n = length(resid(lm)),
                 slope.se = tinfo[1], pval = tinfo[2], Rsq = r2)
    }
ge.nearest  <- function(df, x, y, i = 100){
        if(require(plyr)){
                nearest <- function(x,y = 100) {
                        x  <- as.numeric(x)
                        if(require(plyr)){
                                x[which.min(abs(x - round_any(x,y)))]
                        }
                }
                df$y  <-
                        by  <- by(df$x, df$y, nearest)
                suber <- as.data.frame(cbind(by))
                names(suber)  <- as.character(y)
                mode(df$x)  <- mode(suber$x)
                out  <- match_df(df, suber)
                return(out)
        }
}
ge.slice100m  <- function(df, toslice, na.omit = FALSE) {
        ## creat slice factors
        intervals  <- as.numeric()
        ## TODO power     <- log(10,int)
        for (i in 1:length(toslice)) {
                if(round(toslice[i],-2) == round(toslice[i],-1)) {
                        ##intervals[i]  <- round(toslice[i],-2)%/%int
                        intervals[i]  <- round(toslice[i],-2)
                } else {
                        intervals[i] <- NA
                }
        }
        df$slice  <- intervals
        if(na.omit){
                data  <- na.omit(df)
        }
        ## Delete very rared duplicated case in one slice

#         undup  <- function(df){
#                 df[!duplicated(df[,1]),] # Careful for ID
#         }
#         data.l  <- by(data, data$slice, undup)
#         if (require(plyr)){
#                 data.d  <- rbind.fill(data.l)
#         }
### for voxler soft export
        #data.d$from <- data.d$slice - 50
        #data.d$to <- data.d$slice + 50
        #return(data.d)
        return(df)
}
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
}ge.sp2shpPrj <- function(obj) {
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
}ge.sp2spdf  <- function(sp){
  sp  <- as(grid, "SpatialPoints")
  df  <- data.frame(as.factor(1:length(sp)))
  spdf <- sp::SpatialPointsDataFrame(sp, df)
}
ge.sp2spdf  <- function(sp){
  sp  <- as(grid, "SpatialPoints")
  df  <- data.frame(as.factor(1:length(sp)))
  spdf <- sp::SpatialPointsDataFrame(sp, df)
}
ge.SPDF2grid  <- function(SPDF,x, y = x, type = "regular"){
        sp  <- sp::spsample(SPDF,type = type, cellsize = c(x,y), offset =c(0.5, 0.5))
        df  <- data.frame(id = as.factor(1:length(sp)))
        spdf  <- sp::SpatialPointsDataFrame(sp, data = df)
}

ge.spdf2SPDF  <- function(spdf) {
        grd  <- spdf
        sp::gridded(grd)  <- TRUE
        grd_SPDF  <- as(grd, "SpatialPolygonsDataFrame")
        return(grd_SPDF)
}

ge.splitPoly  <- function(line,poly){
        x  <- gIntersection(line, poly)
        bx <- gBuffer(x, width = 0.000001)
        dx <- gDifference(poly, bx)
}ge.urlTable  <- function(url, table = 1 ) {
        ## scrape the table to dataframe from a url website
      if(!require(XML)){
              installed.packages("XML")
      } else {
              url.doc <- htmlParse(url)
              url.l <- readHTMLTable(url.doc)
              url.d <- url.l[[table]]
      }
      return(url.d)
}
##  # url <- "http://ja.wikipedia.org/wiki/%E5%9C%B0%E7%86%B1%E7%99%BA%E9%9B%BB"
### ge.urlTable(url,3)
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
ge.xy2fishnet  <- function(x1,x2,y1,y2,rx=1000,ry=rx) {
        x.range.v <- seq(as.numeric(x1),as.numeric(x2),by = rx)
        y.range.v <- seq(as.numeric(y1),as.numeric(y2),by = ry)
        grid.m    <- outer(x.range.v, y.range.v, paste, sep = ",")
        grid.v    <- as.vector(grid.m)
        grid.d    <- as.data.frame(grid.v)
        fishnet.d <- data.frame(do.call('rbind', strsplit(as.character(grid.d[,1]), ',', fixed=TRUE)))
        colnames(fishnet.d)  <- c("x","y")
}
