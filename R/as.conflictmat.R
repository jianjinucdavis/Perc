
### 
# as.conflictmat() takes in two arguments:
#   edgelist: A (K x 2) matrix of edges, with the dominant entity in the 1st column
#   swap.order: If the dominant entity is in the 2nd column, specify as TRUE.
# Outputs a matrix with [i,j]th entry equal to the number of times i dominates j.
# Note: edgelist values should be between 1 and N, where N is the total number of entities.
# Also works to convert a matrix to conf.mat class



as.conflictmat = function(edgelist, swap.order = FALSE){
  if(ncol(edgelist) == nrow(edgelist)){
    class(mat) = c("conf.mat", "matrix")
    return(mat)
  }
  
  if(ncol(edgelist) != 2){
    stop("Input a matrix with two columns.")
  }
  if(swap.order == TRUE){
    edgelist = edgelist[,2:1]
  }
  N = max(edgelist)
  if(N > 10000){
    stop("Convert edge IDs to integers starting at 1.")
  }
  mat = matrix(0, N, N)
  for(i in 1:nrow(edgelist)){
    mat[edgelist[i,1], edgelist[i,2]] = mat[edgelist[i,1], edgelist[i,2]] + 1
  }
  class(mat) = c("conf.mat", "matrix")
  return(mat)
}

