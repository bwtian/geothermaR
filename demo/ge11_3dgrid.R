## then sample use sp package to get a 2D grid, and you can choose the type of grid, here is a regular one
grid2d <- readRDS("~/Dropbox/2data/hkd//hkdmskbig_grdi2d1h.Rds")
sp::plot(grid2d)
## until now you get xy data,then we create 3d point grid with depth to 100m to 1000m as example
xy <- grid2d@coords
xyz <- list()
for (i in 1:10) {
        xyz[[i]] <- cbind(xy,rep(i*100, nrow(xy), simplify=F))

}
grid3d <- as.data.frame(do.call(rbind, xyz))
colnames(grid3d) <- c("x","y","z")
grid3d$value <- 10
plot(grid3d)
write.csv(grid3d, "grid3d.csv",quote = F, row.names = F) ## save to csv
