
### countPaths() takes in the output of the IDpaths function
#   and returns a list of the same length of matrices.
#   These matrices contain the number of paths from i to j
#   for each order of dominance path.
###
#
# countPaths = function(allPaths){
#   nOrders = length(allPaths)
#   pathList = list()
#   N = max(sapply(allPaths, max))  #! sapply needs to be fixed. #! N was not used.
#   
#   for(K in 1:nOrders){
#     pathList[[K]] = as.conflictmat(allPaths[[K]][,c(1,(K+1))]) #! return error:"Error in a[[1]][, c(1, (1 + 1))] : incorrect number of dimensions"
#   }
#   pathList
# }


countPaths = function(conf, maxLength = 2){
  allPaths <- findAllPaths(conf, maxLength)
  nOrders = length(allPaths)
  pathList = list()
  N = max(sapply(allPaths, max))  #! sapply needs to be fixed. #! N was not used.
  
  for(K in 1:nOrders){
    pathList[[K]] = as.conflictmat(allPaths[[K]][,c(1,(K+1))]) #! return error:"Error in a[[1]][, c(1, (1 + 1))] : incorrect number of dimensions"
  }
  pathList
}


# to do: 
#  -- input: the output from allpath? not IDpath?
