#' transform an edgelist into a matrix
#' 
#' @param edgelist a 2-column dataframe/matrix of edges. The dominant entity is in the 1st column by default 
#' @param swap.order If the dominant entity is in the 2nd column, specify as TRUE.
#' @return a named matrix with [i,j]th entry equal to the number of times i dominates j.
#' 
#' @examples
#' rawmatrix <- edgelisttomatrix(SampleEdgelist, swap.order = FALSE)


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
    stop("No more than 10000 unique subjects.")
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


#' convert an edgelist or a dominance matrix to conf.mat class
#' 
#' @param df either an edgelist of 2 column dataframe with the dominant entity in the 1st column by default; or a dominance matrix. 
#' @param swap.order If the dominant entity is in the 2nd column, specify as TRUE.
#' @return a named matrix with [i,j]th entry equal to the number of times i dominates j.
#' 
#' @examples
#' confmatrix <- as.conflictmat(SampleEdgelist, swap.order = FALSE)
#' confmatrix <- as.conflictmat(SampleRawMatrix, swap.order = FALSE)

as.conflictmat = function(df, swap.order = FALSE){
  if(ncol(df) == nrow(df)){
    mat <- as.matrix(df)
    class(mat) = c("conf.mat", "matrix")
    return(mat)
  } else {
    mat <- edgelisttomatrix(df, swap.order = FALSE)
    class(mat) = c("conf.mat", "matrix")
    return(mat) 
  }
}


# test the codes using large data set