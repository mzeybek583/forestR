
## Rasterize

library(lidR)
library(ggplot2)
library(future)
library(rlid)
library(spatialEco)

#Set threads for parallelism
get_lidr_threads()
set_lidr_threads(0)
plan(multisession, workers = 20L)

memory.size(max = TRUE)
memory.limit(size=56000)
# Timing ------------------------------------------------------------------

time <- proc.time()

# Load Data
str_name<-'CHM_B7_clip_test.tif' 
r=raster(str_name)

plot(r)

crowns = watershed(r, th = 3, tol = 1.1 , ext = 1)()
plot(crowns, col = pastel.colors(200))

contour = rasterToPolygons(crowns, dissolve = TRUE)

plot(contour)
## filter

cc <- remove.holes(contour)
cc <- remove.holes(cc)
cc <- remove.holes(cc)

plot(cc)
plot(contour)

# Report ------------------------------------------------------------------

crs(contour) <- CRS('+init=EPSG:5254')

shapefile(contour,"crowns_B7_test.shp",overwrite=TRUE)

sprintf("Processing time is %3.1f second",(proc.time() - time)[3])
