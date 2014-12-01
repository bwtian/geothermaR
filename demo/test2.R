wireframe(volcano, drape = TRUE,
          aspect = c(61/87, 0.4),
          light.source = c(10,0,10),
          col.regions = colorRampPalette(c("blue", "pink"))(100))
class(volcano)
library(GISTools)
data(newhaven)
# Do the KDE
breach.dens = kde.points(breach,lims=tracts)
# Plot the result
level.plot(breach.dens)
# Block out the part outside the study area
masker = poly.outer(breach.dens,tracts,extend=100); add.masking(masker)
# Plot census tract boundaries
plot(tracts,add=TRUE)
x <- c(0, 10, 0, 0)
y <- c(0, 0, 100, 0)
z <- c(0, 0, 0, 1)
i <- c(1,2,1,3,1,4)
labels <- c("Origin", "X", "Y", "Z")
text3d(x,y,z,labels)
segments3d(x[i],y[i],z[i])
dfr <- data.frame(x = 1:10, y = (1:10)^2, z = runif(10), col = rainbow(10))
with(dfr, points3d(x, y, z, col = col))
