#' Identifies all paths between all pairs of less than or 
#' equal to a certain length
#' 
#' \code{findAllPaths} Identifies all paths length less than or equal 
#' to \code{maxLength} between all pairs of competitors 
#' 
#' @param conf an N-by-N conflict matrix whose \code{(i,j)}th element is the number of times \code{i} defeated \code{j}
#' @param maxLength a positive numeric integer indicating the maximum length of paths to identify
#' @return A list of two elements. 
#'  
#'  \item{direct pathways}{direct pathways found in original matrix}
#'  
#'  \item{indirect pathways}{a list of all paths from length 2 to the given length}
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(sampleEdgelist)
#' # find all paths of legnth 3
#' allp.3 <- findAllPaths(confmatrix, 3)


findAllPaths = function(conf, maxLength = 2){
  if (maxLength < 2) stop("len should be no smaller than 2.")
  allPathsOutput <- allPaths(conf, maxLength)
#  paths = lapply(2:maxLength, FUN = function(l, conf){
#    do.call(rbind, lapply(1:nrow(conf), FUN = IDpaths, conf = conf, l = l))
#  }, conf = conf)
  paths <- allPathsOutput[[2]]
  pathOutput <- paths
  for (i in 1:length(paths)){
    for (j in 1:length(paths[[i]]))
    pathOutput[[i]][j] <- row.names(conf)[paths[[i]][j]]
  }
  pathOutputAll <- list(which(conf > 0, arr.ind = TRUE), pathOutput)
  names(pathOutputAll)[1] <- "direct pathways"
  names(pathOutputAll)[2] <- "indirect pathways"
  return(pathOutputAll)
  # return(list(which(conf > 0, arr.ind = TRUE), paths))
  #! Why is which(conf > 0, arr.ind = TRUE) useful
  #! > which(confmatrix > 0, arr.ind = TRUE)
  #!        row col
  #! 35510   3   4
  #! 36011   7   4
  #! 36406  19   4
  #! 36504  25   4
  #! 39243  41   4
  #! 39687  44   4
  #! 39800  46   4
  
}

# to do: 
#     -1: #!
#     -2: # name each element in the list with "pathways of length n"