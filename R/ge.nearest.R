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
