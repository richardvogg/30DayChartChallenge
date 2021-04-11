library(rgl)
library(Directional)
library(pracma) #function cart2sph


# Simulate von-Mises Fisher distributed data
m <- rnorm(3)
m <- m / sqrt(sum(m^2))
sample_data <- Directional::rvmf(100, m, 10)

  
# Build the sphere 
X <- seq(0, pi, length.out = 100) 
Y <- seq(0, 2*pi, length.out = 100)
colat <- matrix(data = X, nrow = length(X), ncol = length(Y))
longi <- matrix(data = Y, nrow = length(X), ncol = length(Y), byrow=TRUE)
  
# x, y and z grids -> spherical coordinates transformed to cartesian coordinates
r <- 1 # radius is 1 -> unit sphere
x <- r * sin(colat) * cos(longi)
y <- r * sin(colat) * sin(longi)
z <- r * cos(colat)
  
# f_mat caluclates the color for each point on the sphere depending
# on the number of points around
# -> red: high point density; blue: low point density


f_mat <- function(x, y, z, sample_data){ 
  # x,y,z are matrices with grid information (grid on sphere)
  # returns color with respect to point density
  # (calculates the amount "sample_data-Points" "near" to a grid point on sphere)
  
  f <- function(x, y, z){
    #x,y,z are here scalars!; 
    #some kind of kernel density between all sample data information and one grid point!
    exp(50 * cbind(x, y, z) %*%
          rbind(sample_data[,1], sample_data[,2], sample_data[,3])) %*%
      rep(1/length(sample_data[,1]), length(sample_data[,1])) # Kernel density estimate from Jürgen Kampf's code PlotDensitySphere.R (no origin of the kde give there)
  }
  
  return(matrix(mapply(f,x,y,z), nrow = dim(x)[1]))
}  


value_color <- f_mat(x, z, y, sample_data) 

rr <- range(value_color) 
svals <- (value_color - rr[1]) / diff(rr) 
f_col <- colorRamp(c("blue", "lightblue", "yellow", "orange", "red")) # generates color map
colors <- rgb(f_col(svals)/255)

#Plot 3D
# window is specified for nice snapshot of plot
# then we add things to this window before taking a snapshot in the end
par3d(windowRect = c(50, 50, 800, 800)) 
Sys.sleep(0.1)

rgl.surface(x, y, z, color = colors, back = "lines", lit=FALSE) # plot colored surface
rgl.spheres(x = sample_data, radius = 0.03, col= "black") # plots sample data

# coordinate system
axis_length <- 2
linetype <- "rotation"
arrow3d(p0 = c(0,0,0), p1 = c(0,0,axis_length), type = linetype, lwd=2, col="black",width=1/8)
arrow3d(p0 = c(0,0,0), p1 = c(0,-axis_length,0), type = linetype, lwd=2, col="black",width=1/8)
arrow3d(p0 = c(0,0,0), p1 = c(axis_length,0,0), type = linetype, lwd=2, col="black",width=1/8)
text3d(x = 0,  y = 0,  z = 1.1*axis_length,texts = "Z", col="black", fontweight = "bold", font =2, cex =1.3 )
text3d(x = 0,  y = -1.1*axis_length,z = 0  ,texts = "Y", col="black", fontweight = "bold", font =2, cex =1.3 )
text3d(x = 1.1*axis_length,y = 0,  z = 0  ,texts = "X", col="black", fontweight = "bold", font =2, cex =1.3 )

view3d(theta = 0, phi = 15, zoom = 0.8)

# Snapshot
snapshot3d(filename = "day11_circular/plot.png", fmt = "png")
rgl.close()

