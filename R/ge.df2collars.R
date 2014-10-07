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
    }