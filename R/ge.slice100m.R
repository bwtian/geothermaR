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
