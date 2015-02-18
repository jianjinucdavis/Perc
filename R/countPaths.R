
### countPaths() takes in the output of the IDpaths function
#   and returns a list of the same length of matrices.
#   These matrices contain the number of paths from i to j
#   for each order of dominance path.
###

countPaths = function(allPaths){
  nOrders = length(allPaths)
  pathList = list()
  N = max(sapply(allPaths, max))
  
  for(K in 1:nOrders){
    pathList[[K]] = as.conflictmat(allPaths[[K]][,c(1,(K+1))])
  }
  pathList
}

# to do: to test from the very beginning

