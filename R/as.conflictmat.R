#' transform an edgelist into a matrix
#' 
#' @param edgelist a 2-column (or 3-column for weighted edgelist) dataframe/matrix of edges. The winner is in the 1st column by default. For weighted edgelist, the third column should be the weight. 
#' @param weighted If the edgelist is a 3-column weighted edgelist, use \code{weighted = TRUE}. 
#' @param swap.order If the winner is in the 2nd column, specify as \code{TRUE}.
#' @return a named matrix with \code{[i,j]}th entry equal to the number of times \code{i} wins over \code{j}. 
#' It is the matrix representation of the edgelist.
#' 
#' @examples
#' rawmatrix <- edgelisttomatrix(sampleEdgelist, swap.order = FALSE)
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
    
    if (anyDuplicated(edgelist[,1:2]) != 0) {
      warning(
        "dyads in the weighted edgelist are not unique; the sum of frequencies is taken for duplicated rows."
        )
      edgelist <- sumDuplicate(edgelist)
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


#' convert an edgelist or a win-loss matrix to conf.mat class 
#' 
#' @param Data either an edgelist of 2 column dataframe with the winner in the 1st column by default; or a win-loss matrix. 
#' @param swap.order If the winner is in the 2nd column, specify as \code{TRUE}.
#' @param weighted If the edgelist is a 3-column weighted edgelist, use \code{weighted = TRUE}. 
#' @return a named matrix with \code{[i,j]}th entry equal to the number of times \code{i} wins over \code{j}.
#' 
#' @examples
#' confmatrix <- as.conflictmat(sampleEdgelist, swap.order = FALSE)
#' confmatrix2 <- as.conflictmat(sampleRawMatrix, swap.order = FALSE)
#' confmatrix3 <- as.conflictmat(sampleWeightedEdgelist, weighted = TRUE, swap.order = FALSE)

as.conflictmat = function(Data, weighted = FALSE, swap.order = FALSE){
  if(ncol(Data) == nrow(Data)){
    if (swap.order == TRUE) {
      mat <- t(as.matrix(Data))
    } else{
      mat <- as.matrix(Data)
    }
    class(mat) = c("conf.mat", "matrix")
    return(mat)
  } else {
    
    mat <- edgelisttomatrix(Data, weighted, swap.order)
    class(mat) = c("conf.mat", "matrix")
    return(mat) 
  }
}


#### internal functions


sumDuplicate <- function(weightedEdgelist) {
  uniqueEdgelist <- unique(weightedEdgelist[,1:2])
  for (i in 1:nrow(uniqueEdgelist)){
    uniqueEdgelist[i,3] <- 
      sum(
        weightedEdgelist[
          match.2coldf(weightedEdgelist[,1:2],  uniqueEdgelist[i,]),
          3])
  }
  names(uniqueEdgelist) <- names(weightedEdgelist)
  return(uniqueEdgelist)
}


match.2coldf <- function(dataframe, values) {
  # dataframe should be of two columns
  # values should be a vector of length 2, or a row of dataframe of two columns
  rowIndex <- intersect(which(dataframe[,1] == values[[1]]),
                        which(dataframe[,2] == values[[2]]))
  return(rowIndex)
}