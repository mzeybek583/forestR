# Point Cloud Clustering Meanshift and DBSCAN

## Mean shift Clustering ------------------------------------------------------------

rm(list = ls(globalenv()))
memory.size(max = TRUE)
memory.limit(size=56000)

# Load Library ------------------------------------------------------------

library(lidR)
library(TreeLS)
library(meanShiftR)

setwd(dir = "C:/Users/ASUS/Desktop/TestR")

las.file <- "slice.las"

las <- readLAS(files = las.file)
#las <- tlsSample(las, smp.voxelize(0.05))

tlsPlot(las)

df <- las@data[,1:2]
df.m <- as.matrix(df)
classification <- meanShift(df.m, nNeighbors = 50, algorithm = "KDTREE", iterations = 100, epsilonCluster = 0.1)

unique(classification$assignment)
plot( df$X, df$Y, col=classification[[1]],
      xlab="X", ylab="Y", main="Mean shift labels",
      cex=0.65, pch=16 )
export <- cbind(las@data[,1:3], classification$assignment)

write.csv(export, "export.csv")

# DBSCAN ------------------------------------------------------------

library(dbscan)
res <- dbscan(df, eps = .2, minPts = 20)

res
plot(df, col=res$cluster)
export.dbscan <- cbind(las@data[,1:3], res$cluster)
write.csv(export.dbscan, "export_dbscan.csv")

