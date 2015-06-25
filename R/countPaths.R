#' count paths between all pairs
#' 
#' \code{countPaths} Identifies the number of paths of length 
#' less than or equal to \code{maxLength} between all pairs 
#' 
#' @param conf an N-by-N conflict matrix whose \code{(i,j)}th element is the number of times \code{i} defeated \code{j}
#' @param maxLength a positive numeric integer indicating the maximum length of paths to identify
#' @return A list in which elements are number of paths between all pairs of a given length.
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(sampleEdgelist)
#' # find number of paths of length 3 or less
#' npaths <- countPaths(confmatrix, 3)


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
  if (maxLength < 2) stop("len should be no smaller than 2.")
#  allPaths <- findAllPaths(conf, maxLength)
  allPaths <- allPaths(conf, maxLength)[[2]]
  nOrders = length(allPaths)
  pathList = list()
  N = max(sapply(allPaths, max))  #! sapply needs to be fixed. #! N was not used.
  
  for(K in 1:nOrders){
    pathList[[K]] = as.conflictmat(allPaths[[K]][,c(1,(K+1))]) #! return error:"Error in a[[1]][, c(1, (1 + 1))] : incorrect number of dimensions"
  }
   
  cpNames  <- dimnames(conf)[[1]][as.numeric(dimnames(pathList[[1]])[[1]])]
  
  for (i in 1:length(pathList)){
    dimnames(pathList[[i]])[[1]] <- cpNames
    dimnames(pathList[[i]])[[2]] <- cpNames
  }
  
  totalPathNum <- unname(unlist(lapply(pathList, sum)))
  names(pathList) <- 
    paste(
    "path length =", 
    c(2:maxLength),
    ";",
    "Total Number of Pathways:",
    totalPathNum
    )
  return(pathList)
}


# to do: 
#  -- input: the output from allpath? not IDpath?

