
## Rasterize

library(lidR)
library(ggplot2)
library(future)

#Set threads for parallelism
get_lidr_threads()
set_lidr_threads(0)
plan(multisession, workers = 20L)

memory.size(max = TRUE)
memory.limit(size=56000)
# Timing ------------------------------------------------------------------

time <- proc.time()

# Load Data
b7.las <- readLAS("../2016-02-11_16-28-48_100pct_height_world-B7-geo-3cm.laz")


# Denoise -----------------------------------------------------------------

#plot(b7.las)

b7.las <- classify_noise(b7.las, sor(6,3))
b7.las <- filter_poi(b7.las, Classification != LASNOISE)

#plot(b7.las)


las <- classify_ground(b7.las, algorithm = csf(class_threshold = 0.2))
p1 <- c(584298.204,4081061.001);p2 <- c(584325.885,4081064.807 )

plot_crossection <- function(las,
                             p1 = c(min(las@data$X), mean(las@data$Y)),
                             p2 = c(max(las@data$X), mean(las@data$Y)),
                             width = 1, colour_by = NULL)
{
  colour_by <- enquo(colour_by)
  data_clip <- clip_transect(las, p1, p2, width)
  p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
  
  if (!is.null(colour_by))
    p <- p + aes(color = !!colour_by) + labs(color = "")
  
  return(p)
}


#  Test of classification -------------------------------------------------


 #plot_crossection(las, p1 , p2, 0.5, colour_by = factor(Classification))


# Generate Terrain --------------------------------------------------------


dtm_tin <- grid_terrain(las, res = 0.5, algorithm = tin())
#plot_dtm3d(dtm_tin, bg = "white") 



# Point Cloud Normalization -----------------------------------------------

nlas <- normalize_height(las, knnidw())



# Rasterize ---------------------------------------------------------------

#chm <- grid_canopy(nlas, res = 0.05, p2r(0.05, na.fill = tin()))
col <- height.colors(50)
#plot(chm, col = col)

# Method2
chm <- grid_canopy(nlas, res = 0.05, pitfree(thresholds = c(0, 5, 10), max_edge = c(0, 0.2)))
#plot(chm, col = col)
fill.na <- function(x, i=5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else { return(x[i]) }}
w <- matrix(1, 3, 3)
chm <- focal(chm, w, fun = fill.na)
#plot(chm, col = col)


# Individual Trees --------------------------------------------------------
# variable windows size
adaptive_wind <- function(x) { x * 0.07 + 5}
ttops_chm_pf <- find_trees(chm, lmf(adaptive_wind, shape = "circular"))
#plot(chm, main = "CHM PITFREE 0.5m", col = col); plot(ttops_chm_pf, add = T)


# Individual Tree on Point Clouds -----------------------------------------

algo <- dalponte2016(chm, ttops_chm_pf)
las <- segment_trees(las, algo) # segment point cloud
#plot(las, bg = "white", size = 4, color = "treeID") # visualize trees



# Tree Crowns -------------------------------------------------------------

crowns <- delineate_crowns(las,"concave")
#par(mar=rep(0,4))
#plot(crowns)

#crowns_water = watershed(chm, th = 2)()
#plot(crowns, col = pastel.colors(200))

#contour = rasterToPolygons(crowns, dissolve = TRUE)



# Report ------------------------------------------------------------------
crs(chm) <- CRS('+init=EPSG:5254')
crs(ttops_chm_pf) <- CRS('+init=EPSG:5254')
crs(crowns) <- CRS('+init=EPSG:5254')

writeRaster(chm, "CHM_B7.tif",format="GTiff", overwrite=TRUE)
shapefile(ttops_chm_pf,"ttops_B7.shp",overwrite=TRUE)
shapefile(crowns,"crowns_B7.shp",overwrite=TRUE)

sprintf("Processing time is %3.1f second",(proc.time() - time)[3])
