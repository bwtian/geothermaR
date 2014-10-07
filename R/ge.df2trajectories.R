ge.df2trajectories  <- function(ID, MD, Azimuth = 0, Inclination = 0) {
      ID  <- ID
      well  <- as.data.frame(ID)
      well$MD  <- MD
      well$Azimuth  <- Azimuth
      well$Inclination  <- Inclination
      well[sort(well$ID),]
      return(well)
    }
