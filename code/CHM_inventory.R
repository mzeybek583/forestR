
# Attach the 'ForestTools' and 'raster' libraries
library(ForestTools)
library(raster)

# Load sample canopy height model
data("kootenayCHM")

#View the CHM using the plot function. The cell values are equal to the canopy’s height above ground.

# Remove plot margins (optional)
par(mar = rep(0.5, 4))

# Plot CHM (extra optional arguments remove labels and tick marks from the plot)
plot(kootenayCHM, xlab = "", ylab = "", xaxt='n', yaxt = 'n')

lin <- function(x){x * 0.05 + 0.6}
ttops <- vwf(CHM = kootenayCHM, winFun = lin, minHeight = 2)
plot(kootenayCHM, xlab = "", ylab = "", xaxt='n', yaxt = 'n')
plot(ttops, col = "blue", pch = 3, cex = 0.5, add = TRUE)
mean(ttops$height)
crowns <- mcws(treetops = ttops, CHM = kootenayCHM, minHeight = 1.5, verbose = FALSE)
plot(crowns, col = sample(rainbow(50), length(unique(crowns[])), replace = TRUE), legend = FALSE, xlab = "", ylab = "", xaxt='n', yaxt = 'n')

# Create polygon crown map
crownsPoly <- mcws(treetops = ttops, CHM = kootenayCHM, format = "polygons", minHeight = 1.5, verbose = FALSE)

# Plot CHM
plot(kootenayCHM, xlab = "", ylab = "", xaxt='n', yaxt = 'n')

# Add crown outlines to the plot
plot(crownsPoly, border = "blue", lwd = 0.5, add = TRUE)

# Compute average crown diameter
crownsPoly[["crownDiameter"]] <- sqrt(crownsPoly[["crownArea"]]/ pi) * 2

# Mean crown diameter
mean(crownsPoly$crownDiameter)

sp_summarise(ttops)
sp_summarise(crownsPoly, variables = c("crownArea", "height"))
