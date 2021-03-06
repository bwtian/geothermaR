## Here I took a irregular polygon as example
P <- sp::Polygon(cbind(c(2500, 2500, 3500, 3500, 2500),
                       c(1500, 4500, 4500, 5500, 1500)))
Ps <- sp::Polygons(list(P), "P")
SP <- sp::SpatialPolygons(list(Ps))
sp::plot(SP)
shp <- SP
## then sample use sp package to get a 2D grid, and you can choose the type of grid, here is a regular one
grid2d <- sp::spsample(shp,type = "regular", cellsize = c(100,100), offset =c(0.5, 0.5))
sp::plot(grid2d)
## until now you get xy data,then we create 3d point grid with depth to 100m to 1000m as example

