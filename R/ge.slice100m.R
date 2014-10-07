ge.slice100m  <- function(df,v,int=100) {
        ## creat slice factors
        intervals  <- as.numeric()
        ## TODO power     <- log(10,int)
        for (i in 1:length(v)) {
                if(round(v[i],-2) == round(v[i],-1)) {
                        ##intervals[i]  <- round(v[i],-2)%/%int
                        intervals[i]  <- round(v[i],-2)
                } else {
                        intervals[i] <- NA
                }
        }
        df$slice  <- intervals
        data  <- na.omit(df)
        ## Delete very rared duplicated case in one slice
        undup  <- function(df){
                df[!duplicated(df[,1]),] # Careful for ID
        }
        data.l  <- by(data, data$slice, undup)
        if (require(plyr)){
                data.d  <- rbind.fill(data.l)
        }
### for voxler soft export
        data.d$from <- data.d$slice - 50
        data.d$to <- data.d$slice + 50
        return(data.d)
}
