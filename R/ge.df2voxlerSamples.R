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
