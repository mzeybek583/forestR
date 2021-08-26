
## 
#Note : plotless!

# -------------------------------------------------------------------------
rm(list = ls(globalenv()))
memory.size(max = TRUE)
memory.limit(size=56000)

# Load Library ------------------------------------------------------------
setwd("path_to_las")
library(beepr)
library(stringr)
library(plotrix)
library(lidR)
library(TreeLS)
library(TreeLS)
library(dplyr)
library(MASS)
library(ggplot2)
library(prediction)

## For Windows

time <- proc.time()


# Parameters --------------------------------------------------------------

max.radius <- 0.7


# File names --------------------------------------------------------------

file3 <- "6.txt"
las.file <- "clipped.laz"

f.name <- tools::file_path_sans_ext(file3)

# Read from 3DReshaper ----------------------------------------------------
## 3D Reshaper Read Data #####

CylinderFit <- read.csv(file = file3, header = F, skip = 1)
class(CylinderFit)
colnames(CylinderFit) <- c("data")
CylinderFit <- str_split_fixed(CylinderFit$data, pattern = ":", 2)
CylinderFit <- as.data.frame(CylinderFit)
colnames(CylinderFit) <- c("Name", "Variable")

cy_fit <- CylinderFit[which(CylinderFit$Name == "Radius", arr.ind = TRUE),]
cy_fit$Variable<-as.numeric(as.character(cy_fit$Variable))
#cy_fit$Variable <- sort(cy_fit$Variable)

cy_fitxy <- CylinderFit[which(CylinderFit$Name == "Center", arr.ind = TRUE),]
cy_fitxy <- str_split_fixed(cy_fitxy$Variable, pattern = " ", 4)
cy_fitxy <- as.data.frame(cy_fitxy)
colnames(cy_fitxy) <- c("bos","x", "y","z")
cy_fitxy <- cy_fitxy[2:4]
cy_fitxy$x<-as.numeric(as.character(cy_fitxy$x))
cy_fitxy$y<-as.numeric(as.character(cy_fitxy$y))
cy_fitxy$z<- 2L 


Tree.count <- nrow(cy_fitxy)
TreeID <- seq(1:Tree.count)
Diameter <- 2*cy_fit$Variable
cy_fitxy <- cbind(TreeID, cy_fitxy, Diameter )

write.table(cy_fitxy, file = paste(f.name,"_result.csv", sep = ""), row.names = F,
            sep = ";")


# Height Functions --------------------------------------------------------

## Height Function
file = paste(f.name,"_result.csv", sep = "")

df <- read.csv(file = file, sep = ";")

## Center Point & Radius of Plot

c.x= 0
c.y= 0
R = 11.29



las <- readLAS(files = las.file)

las_normal = tlsNormalize(las, keep_ground = T)
las <- las_normal
las@header
las@header@PHB[["X scale factor"]] <- 0.01
las@header@PHB[["Y scale factor"]] <- 0.01
las@header@PHB[["Z scale factor"]] <- 0.01

# Create Inventory ####

inv <- data.frame()
out <- clip_circle(las, df$x[1], df$y[1], 0.5)
out <- add_lasattribute(out, 1, "TreeID", "Tree ID")
Z <- sort(out@data$Z, decreasing = TRUE)
Tree.Height <- median(head(Z,15))
inv <- Tree.Height
Tree.count<- nrow(df)

for (i in 2:Tree.count) {
  seg.tree <- clip_circle(las, df$x[i], df$y[i], 0.5)
  seg.tree <- add_lasattribute(seg.tree, i, "TreeID", "Tree ID")
  out <- rbind(seg.tree,out)
  Z <- sort(seg.tree@data$Z, decreasing = TRUE)
  Tree.Height <- median(head(Z,15))
  inv <- rbind(inv,Tree.Height)
}
rownames(inv) <- seq(1: Tree.count)
inv.man <- data.frame(df$TreeID ,df$x, df$y, inv, df$Diameter)
colnames(inv.man) <- c("Tree ID", "X", "Y", "Tree Height","Diameter")

col<-rgb(runif(Tree.count),runif(Tree.count),runif(Tree.count))

# Plot --------------------------------------------------------------------


#plot(out, color = "TreeID", colorPalette = col)


writeLAS(out, paste(f.name,"_trees.las", sep = ""))
### Plots #####

pdf(file = paste(f.name,"-PlotCenters.pdf", sep = ""))
plot(df$x, df$y ,asp=1, pch=3, cex=0.5, xlab="X", ylab="Y", ylim=c(-12,12),
     xlim=c(-12,12))
draw.circle(c.x,c.y, R, lty=2)
points(c.x,c.y, pch=24,col="black", bg="red")

text(df$x, df$y+0.3 ,labels=df$TreeID, col="black", cex=0.5)
text(df$x-0.3, df$y-0.3 ,labels=paste("H: ",round(inv.man$`Tree Height`, digits = 2), sep = ""), col="red",
     cex=0.3)
text(df$x+0.8, df$y-0.3 ,labels=paste("D: ",round(df$Diameter, digits = 2), sep = ""), col="green",
     cex=0.3)
dev.off()


pdf(file = paste(f.name,"-PlotDiameters.pdf", sep = ""))
plot(df$x, df$y,asp=1, pch=3, cex=0.1, 
     xlab="X", ylab="Y", ylim=c(-13,13),xlim=c(-13,13))
text(10,-11, labels="Scale:30 cm")
draw.circle(10, -12, 30/100, lty=1)
draw.circle(c.x, c.y, R, lty=2)
sp.df <- df[,1:2]
colnames(sp.df) <- c("x","y")
points(c.x,c.y, pch=24,col="black", bg="red")

for (i in 1:Tree.count) {
  draw.circle(df$x[i], df$y[i], (df$Diameter[i]), lty=1)
}
dev.off()


## Export Results #####
write.table(inv.man, paste(f.name,"-inventory_mz.csv", sep = ""), sep=";", row.names = FALSE, quote = FALSE)
m.height <- mean(inv.man$`Tree Height`)


#map = treeMap(out, map.hough(min_h= 2, h_step = 0.25, max_h = 3))
#tlsPlot(map, fast = TRUE)
#map.df <- as.data.frame(map@data)
#df3 <- transform(map.df, id=match(TreeID, unique(TreeID)))
#map@data$TreeID <- df3$id
#rm(df3,map.df)


# extract the tree map from a thinned point cloud
#
#thin = tlsSample(out, smp.voxelize(0.01))
#map = treeMap(thin, map.hough(min_density = 0.1), 0)
#x = plot(thin)

#add_treeMap(x, map, color='yellow', size=2)

# classify tree regions
#tls = treePoints(thin, map, trp.crop())
#add_treePoints(x, tls, size=4)
#add_treeIDs(x, tls, cex = 2, col='yellow')

# classify stem points

#x=plot(out)
#out <- tlsNormalize(out, keep_ground = F)
tls = stemPoints(out, stm.hough(h_base = c(1,2), min_density = 0.4))
#add_stemPoints(x, tls, color='red', size=8)

#plot(tls,color="Stem")

# make the plot's inventory
inv = tlsInventory(tls, d_method=shapeFit(shape = "cylinder", algorithm = "ransac", n = 20, n_best = 20))
#add_tlsInventory(x, inv)

# extract stem measures
seg = stemSegmentation(tls, sgt.ransac.cylinder(n=20))
#add_stemSegments(x, seg, color='white', fast=T)

# plot everything once
#tlsPlot(tls, inv, seg, fast=T)

# check out only one tree
#tlsPlot(tls, seg, tree_id = 1)
#tlsPlot(tls, seg, tree_id = 4)


# Calculate Volume --------------------------------------------------------


volumes <- data.frame()
n.max <- length(inv$TreeID)

## Tree trunk volume calculation

for (i in 1:n.max) {
  
  pred.height.seq <- 0.5
  tree.height <- inv$H[i] # get from inventory
  new.pred.height <- tree.height*2/5
  sections <- data.frame(height=seq(new.pred.height, tree.height, pred.height.seq))
  tree.id <- inv$TreeID
  
  radius<- seg$Radius[seg$TreeID==tree.id[i]]
  radius <- data.frame(radius=radius)
  height<- data.frame(height=seg$AvgHeight[seg$TreeID==tree.id[i]])
  
  
  ind <- seq(1,ceiling(nrow(radius)*2/5))
  df <- data.frame(height=height[ind,], radius=radius[ind,])
  #dt = data.table(height, val = height) # you'll see why val is needed in a sec
  #setattr(dt, "sorted", "height")  # let data.table know that w is sorted
  #setkey(dt, height) # sorts the data
  
  # binary search and "roll" to the nearest neighbour
  # In the final expression the val column will have the you're looking for.
  #ind.height <- dt[J(new.pred.height), .I, roll = "nearest",by = .EACHI]
  #ind.height <- ind.height$I
  
  #train.height <- data.frame(height=height[seq(1,ind.height),])
  pred.height <- data.frame(height=height[seq(max(ind)+1, nrow(height)),])
  pred.radius <-data.frame(radius=radius[seq(max(ind)+1,nrow(radius)),])
  df2 <- data.frame(height=pred.height, radius= pred.radius)
  
  V1 <- sum(pi*df$radius^2*0.5)
  
  #V<- (V+V2) # m^2
  #V
  
  
  ###
  
  #plot(df)
  
  model <- rlm(radius~ height, data = df,init = "ls", psi = psi.huber)
  
  #ggplot(df, aes(x = height, y = radius)) +
  # geom_point() 
  #    geom_line(slope = coef(model)[[2]], intercept = coef(model)[[1]], col="green",size=1.2)
  #stat_smooth(method = "lm", formula = radius ~ poly(height, 3), se = TRUE)
  
  summary(model)
  model$residuals
  #plot(model)
  pred.radius2 <- prediction(model = model, data = df2)
  pred.radius2
  
  #plot(df$height, df$radius)
  #lines(df$height,model$fitted.values, col="red")
  
  fitted.radius <- pred.radius2$fitted  
  fitted.radius.lim <- fitted.radius[fitted.radius>0.04]
  fitted.radius.lim
  V2 <- sum(pi*fitted.radius.lim^2*0.5)
  V <- V1+V2
  
  sprintf("Toplam hacim: %3.3f", V)
  volumes[i,1] <- tree.id[i]
  volumes[i,2] <- V
}

inv2 <- cbind(inv,volumes$V2)


# Volume from h and D -----------------------------------------------------

#getwd()

#df <- read.csv("201-inventory_mz.csv", header = T, sep = ";")
inv.man
colnames(inv.man) <- c("Tree ID", "X", "Y", "Tree Height","Diameter")

hacim.f <- function(d, H) (d*100)^2*(32506*10^-8 + 2453*10^-8*H)


V_Hd <- apply(inv.man[,c("Diameter","Tree Height")], 1, function(y) hacim.f(y["Diameter"],y["Tree Height"]))
inv2 <- cbind(inv2,V_Hd, inv.man$Diameter)
#cat(sprintf("Tek hacim %3.8f \n", V))

# Export report -----------------------------------------------------------


colnames(inv2) <- c("Tree ID", "X", "Y", "Radius", "Error", "H", "h_rad", "volume", "V_Hd","Manual.Diam")
inv2 <- inv2[inv2$Radius<max.radius,]
write.table(inv2, paste(f.name,"-inventory_treeLS_v2.csv", sep = ""), sep = ";", row.names = FALSE, quote = FALSE)

mescere <- data.frame("ortalama yükseklik"=mean(inv2$H), "Ortalama Çap"=mean(2*inv2$Radius), "Toplam Hacim"=sum(inv2$volume))
write.table(mescere, paste(f.name,"-mescerebilgisi.csv", sep = ""), sep = ";", row.names = FALSE, quote = FALSE)
beep(3)

proc.time() - time
