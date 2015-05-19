#' transform an edgelist into a matrix
#' 
#' @param edgelist a 2-column (or 3-column for weighted edgelist) dataframe/matrix of edges. The dominant entity is in the 1st column by default. For weighted edgelist, the third column should be the weight. 
#' @param weighted If the edgelist is a 3-column weighted edgelist, use \code{weighted = TRUE}. 
#' @param swap.order If the dominant entity is in the 2nd column, specify as TRUE.
#' @return a named matrix with [i,j]th entry equal to the number of times i dominates j. It is the matrix representation of the edgelist.
#' 
#' @examples
#' rawmatrix <- edgelisttomatrix(SampleEdgelist, swap.order = FALSE)
#' 
#' rawmatrix2 <- edgelisttomatrix(sampleWeightedEdgelist, weighted = TRUE, swap.order = FALSE)





edgelisttomatrix <- function(edgelist, weighted = FALSE, swap.order = FALSE) {
  
  if (swap.order == TRUE){
    edgelist[, 1:2] <- edgelist[, 2:1]
  }
  
  subjects = unique(sort(as.matrix(edgelist[,1:2]))) # work better for IDs of character
  # subjects = sort(unique(c(edgelist[,1], edgelist[,2])))
  N = length(subjects)
  if (N > 10000){
    stop("No more than 10000 unique subjects.")
  }
  
  mat = matrix(0, N, N)
  
  
  if (weighted == TRUE){
    
    if (ncol(edgelist) != 3){
      stop("Input a matrix or dataframe with three columns, with the third column being Frequency of the interaction")
    }
    
    if (nrow(edgelist[,1:2]) != nrow(unique(edgelist[,1:2]))) {
      stop("dyads in the edgelist are not unique; weighted edgelist should contain only unique dyads")
    }
    
    
    # transform the weighted edgelist into a matirx
    
    for(i in 1:nrow(edgelist)){
      subject1 = which(subjects == edgelist[i,1])
      subject2 = which(subjects == edgelist[i,2])
      mat[subject1, subject2] = edgelist[i, 3]
    }
    
  } else {
    
    if (ncol(edgelist) != 2){
      stop("edgelist should be a dataframe or matrix of two columns. If it is a weighted edgelist, it should be a matrix or dataframe of 3 columns and use the argument 'weighted = TRUE'")
    }
    
    for(i in 1:nrow(edgelist)){
      subject1 = which(subjects == edgelist[i,1])
      subject2 = which(subjects == edgelist[i,2])
      mat[subject1, subject2] = mat[subject1, subject2] + 1
    }
  }
  
  rownames(mat) = subjects
  colnames(mat) = subjects
  
  return(mat)
}


#' convert an edgelist or a dominance matrix to conf.mat class 
#' 
#' @param Data either an edgelist of 2 column dataframe with the dominant entity in the 1st column by default; or a dominance matrix. 
#' @param swap.order If the dominant entity is in the 2nd column, specify as TRUE.
#' @param weighted If the edgelist is a 3-column weighted edgelist, use \code{weighted = TRUE}. 
#' @return a named matrix with [i,j]th entry equal to the number of times i dominates j.
#' 
#' @examples
#' confmatrix <- as.conflictmat(SampleEdgelist, swap.order = FALSE)
#' confmatrix2 <- as.conflictmat(SampleRawMatrix, swap.order = FALSE)
#' confmatrix3 <- as.conflictmat(sampleWeightedEdgelist, weighted = TRUE, swap.order = FALSE)

as.conflictmat = function(Data, weighted = FALSE, swap.order = FALSE){
  if(ncol(Data) == nrow(Data)){
    mat <- as.matrix(Data)
    class(mat) = c("conf.mat", "matrix")
    return(mat)
  } else {
    
    mat <- edgelisttomatrix(Data, weighted, swap.order)
    class(mat) = c("conf.mat", "matrix")
    return(mat) 
  }
}

