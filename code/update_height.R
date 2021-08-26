

## Edit for Diameter and Height Values Manually
## TreeLS improvement

# -------------------------------------------------------------------------
rm(list = ls(globalenv()))
memory.size(max = TRUE)
memory.limit(size=56000)

# Load Library ------------------------------------------------------------
setwd("path_to_las/")


library(lidR)
library(TreeLS)
library(rgl)
library(dplyr)
library(MASS)
library(ggplot2)
library(prediction)

# File names --------------------------------------------------------------

las.file <- "6_trees.las"

las <- readLAS(files = las.file)

tree.id <- 3
s.tree <- filter_poi(las,las@data$TreeID==tree.id)
s.tree = stemPoints(s.tree, stm.hough(h_base = c(1,2), min_density = 0.4)) #min_density = 0.4
#plot(s.tree, color="Stem")
inv = tlsInventory(s.tree, d_method=shapeFit(shape = "cylinder", algorithm = "ransac", n = 20, n_best = 20));inv

#tls <- add_lasattribute(tls, 1*tls@data$Stem, "Stem", "Stem Points")

#writeLAS(tls,"tree-18.las")

ids <- rgl::plot3d(s.tree@data[,1:3], axis=TRUE,aspect=TRUE, bg="white")
aspect3d(1, 1, 10)

h.Z <- (s.tree@data$Z)
hist(h.Z)
boxplot(h.Z)

if (interactive()) {
  # Click near a point to select it and put a sphere there.
  # Press ESC to quit...
  
  # This version returns coordinates
  selectpoints3d(ids["data"], value=TRUE,
                 multiple = function(x) {
                   spheres3d(x, color = "red", alpha = 0.9, radius = 1)
                   TRUE
                 })
}
