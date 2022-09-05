### Step 1: Load and initialize the arcgisbinding
library(arcgisbinding)
arc.check_product()
### Step 2: Test if the sp and mclust packages are installed
if (!requireNamespace("sp", quietly = TRUE))
  install.packages("sp")
if (!requireNamespace("mclust", quietly = TRUE))
  install.packages("mclust")
### Step 3: Load the sp, and mclust into the R workspace
library(sp)
library(mclust)
### Step 4: Loads the billboard faces dataset
d <- arc.open(path = 'Y:\\GitHubTB\\GEOClass\\SUNY-Buffalo-GEO511-SDS\\Week2\\R-ArcGISBridge\\data.gdb\\Billboard_Faces')

### Step 5: Select a subset data from the loaded dataset
data <- arc.select(d, c('WIDTH','HEIGHT','OBJECTID'), where_clause = "WIDTH > 25 AND HEIGHT > 11")

### Step 6: Get the shape object from the selected subset data
data_shp <- arc.shape(data)

### Step 7: Pull the x and y coordinate values for each record and populate a dataframe
data.xy <- cbind(data_shp$x, data_shp$y)

### takes the data.xy dataframe and produces a cluster
patternBIC <- mclustBIC(data.xy)

### takes  the cluster results, and the data.xy dataframe and produces a summary
patternModel <- summary(patternBIC, data.xy)

### returns a list of probabilities
n <- patternModel$G
cond_probs <- lapply(1:n, function(i) patternModel$z[,i])

### determines the best model from clustering
bestModel <- mclustModel(data.xy, patternBIC)

### Calls the create.ellipse function
polygons <- create.ellipses(bestModel)

### pulls the mean value parameters from the bestmodel
mu <- bestModel$parameters$mean

### Step 8: Create a SpatialPolygonsDataFrame object to hold the polygon
sp.df <- SpatialPolygonsDataFrame(polygons, data=data.frame(clast_n=1:n, mu_x=mu[1,1:n], mu_y=mu[2,1:n]), match.ID = F)

### Step 9: Create a list object containing geometry type and spatial reference information for output
shape_info <- list(type="Polygon", WKT=arc.shapeinfo(data_shp)$WKT)

### Step 10: Write the output of the clustering to a new feature class
arc.write("Y:\\GitHubTB\\GEOClass\\SUNY-Buffalo-GEO511-SDS\\Week2\\R-ArcGISBridge\\data.gdb\\billboard_clusters", sp.df, shape_info = shape_info)

### utility functions for creating the ellipses
create.ellipses <- function(bestModel)
{
  n <- bestModel$G
  mu <- bestModel$parameters$mean
  sigma <- bestModel$parameters$variance$sigma
  cls.polygons <- lapply(1:n, function(i)
    {
    xy <- make.ellipse(mu = mu[,i], sigma = sigma[, , i])
    name <- paste0("ellipse", i)
    Polygons(list(Polygon(xy)), name)
  })
  SpatialPolygons(cls.polygons, 1:n, proj4string=sp::CRS())
}

make.ellipse <- function(mu, sigma, k = 60)
{
  p <- length(mu)
  if(p != 2)
    stop("only two-dimensional case is available")
  if(any(unique(dim(sigma)) != p))
    stop("mu and sigma are incompatible")
  ev <- eigen(sigma, symmetric = TRUE)
  s <- sqrt(rev(sort(ev$values)))
  v <- t(ev$vectors[, rev(order(ev$values))])
  theta <- (0:k) * (2*pi/k)
  x <- s[1] * cos(theta)
  y <- s[2] * sin(theta)
  xy <- cbind(x,y)
  xy <- xy %*% v
  cbind(xy[,1] + mu[1], xy[,2] + mu[2])
}

