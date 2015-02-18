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
  # N = max(edgelist)
  #? I tested this function using the sample edgelist under data file, 
  #? it returns an error here. 
  #? When IDs in edgelist are integers, 
  #? max(edgelist) returns the largest number of ID rather than the number of IDs.
  subjects = sort(unique(c(edgelist[,1], edgelist[,2])))
  N = length(subjects)
  
  if(N > 10000){
    stop("Convert edge IDs to integers starting at 1.")
  }
  mat = matrix(0, N, N)
  #! error here. "Error in mat[edgelist[i, 1], edgelist[i, 2]] : subscript out of bounds"
# for(i in 1:nrow(edgelist)){
#   mat[edgelist[i,1], edgelist[i,2]] = mat[edgelist[i,1], edgelist[i,2]] + 1
# }
  #! mat[edgelist[i,1], edgelist[i,2]] refers to wrong row/column numbers.
  for(i in 1:nrow(edgelist)){
    subject1 = which(subjects == edgelist[i,1])
    subject2 = which(subjects == edgelist[i,2])
    mat[subject1, subject2] = mat[subject1, subject2] + 1
  }
  rownames(mat) = subjects
  colnames(mat) = subjects
  class(mat) = c("conf.mat", "matrix")
  return(mat)
}


# to do: add more sample edgelists and matrices to data folder. 
# test the codes using sample data
# test the codes using large data set