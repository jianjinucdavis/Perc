#' transform an edgelist into a matrix
#' 
#' @param edgelist a data frame. Edgelist. With the first column named "Initiator"; the second column named "Recipient". 
#' @param path.length an integer between 2 to 4, representing the length of indirect pathways used in finding dominance interactions.
#' @return a dataframe representing dominance certainty matrix.
#' @examples
#' PercOutput <- PercMatrix(SampleEdgelist, 2)


# as.conflictmat() takes in two arguments:
#   edgelist: A (K x 2) matrix of edges, with the dominant entity in the 1st column by default
#   swap.order: If the dominant entity is in the 2nd column, specify as TRUE.
# Outputs a matrix with [i,j]th entry equal to the number of times i dominates j.
# Note: edgelist values should be between 1 and N, where N is the total number of entities.
# Also works to convert a matrix to conf.mat class



edgelisttomatrix <- function(edgelist, swap.order = FALSE){
  if(ncol(edgelist) != 2){
    stop("Input a matrix with two columns.")
  }
  if(swap.order == TRUE){
    edgelist = edgelist[,2:1]
  }
  subjects = sort(unique(c(edgelist[,1], edgelist[,2])))
  N = length(subjects)
  
  if(N > 10000){
    stop("Convert edge IDs to integers starting at 1.") # ? what does this used for?
  }
  mat = matrix(0, N, N)
  
  for(i in 1:nrow(edgelist)){
    subject1 = which(subjects == edgelist[i,1])
    subject2 = which(subjects == edgelist[i,2])
    mat[subject1, subject2] = mat[subject1, subject2] + 1
  }
  rownames(mat) = subjects
  colnames(mat) = subjects
  
  return(mat)
}

#' convert to conflict matrix class
#' df: a dataframe, either edgelist or matrix

as.conflictmat = function(df, swap.order = FALSE){
  if(ncol(df) == nrow(df)){
    mat <- df
    class(mat) = c("conf.mat", "matrix")
    return(mat)
  } else {
    mat <- edgelisttomatrix(df, swap.order = FALSE)
    class(mat) = c("conf.mat", "matrix")
    return(mat) 
  }
}


# to do: add more sample edgelists and matrices to data folder. 
# to do: break this function into two functions. 1. edgelist --> matrix; 2. as.confmat
# test the codes using sample data
# test the codes using large data set
# documentation!!!