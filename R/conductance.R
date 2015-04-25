#' compute conductance dominance probabilities
#' 
#' \code{conductance} compute conductance dominance probabilities
#' 
#' @param conf N-by-N conflict matrix whose (i,j)th element is the number of times i defeated j
#' @param maxLength a positive numeric integer indicating the maximum length of paths to identify
#' @param beta a positive numeric value (more explanation)
#' @return a list of two elements. 
#' imputed.conf. An N-by-N conflict matrix whose (i,j)th element is the 
#'    'effective' number of wins of i over j.
#' p.mat. An N-by-N numeric matrix whose (i,j)th element is the estimated 
#'      conductance dominance probability.
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(SampleEdgelist)
#' # find dominance probability matrix
#' perm2 <- conductance(confmatrix, 2)
#' perm2$imputed.conf
#' perm2$p.hat

conductance = function(conf, maxLength, alpha = NULL, beta = 1){
  N = nrow(conf)
  
  ### percMat will contain direct + indirect information from dominance paths
  percMat = conf
  
  outdegree = rowSums(conf)
  
  # calculate alpha if not exist
  conf.trans <- transitivity(conf)
  if (is.null(alpha)) {
    alpha <- conf.trans$alpha
  }
  # if alpha is larger than 500, use 500.
  alpha <- min(alpha, 500)
  ####===================
  # alpha.temp <- conf.trans$alpha
  #
  ## allows for completely transitive matrices by approximating Inf alpha with a huge alpha of 500  
  
  # alpha.checker <- function(o_o){
  #   if (o_o == Inf) {
  #     return(500)
  #   } else {
  #     return(o_o)
  #   }
  # }
  
  # alpha <- alpha.checker(alpha.temp)
  ####=====
  if(sum(conf[row(conf) != col(conf)] == 0) > 0){
    paths = allPaths(conf, maxLength)
    
    ### Populating the direct + indirect conflict matrix called "percMat"
    for(k in 1:(maxLength - 1)){
      for(r in 1:nrow(paths[[2]][[k]])){
        percMat[paths[[2]][[k]][r,1], paths[[2]][[k]][r,k+1]] = 
          percMat[paths[[2]][[k]][r,1], paths[[2]][[k]][r,k+1]] + 
          ((alpha + beta)/(alpha + 2*beta)/mean(outdegree))^k
        # gc()
      }
    }
  }
  else{
    constant = sum(((alpha + beta)/(alpha + 2 * beta)/mean(outdegree))
                   ^(1:(maxLength - 1)))
    percMat = percMat + constant
    diag(percMat) = 0
  }
  
  
  ### "percMat2" is the estimated dominance probability matrix
  
  percMat2 = matrix(0, N, N)
  for(i in 2:N){
    for(j in 1:(i-1)){
      temp1 = (alpha * percMat[i,j] + beta)/(alpha * percMat[i,j] + 
                                               alpha * percMat[j,i] + 2 * beta)
      temp2 = (alpha * percMat[j,i] + beta)/(alpha * percMat[i,j] + 
                                               alpha * percMat[j,i] + 2 * beta)
      percMat2[i,j] = ifelse(is.nan(temp1), 0.5, temp1)
      percMat2[j,i] = ifelse(is.nan(temp2), 0.5, temp2)
      # if(verbose){print(i)}  # "Error: object 'verbose' not found"
    }
  }
  row.names(percMat2) <- row.names(conf)
  colnames(percMat2) <- colnames(conf)
  return(list(imputed.conf = percMat, p.hat = percMat2))  
}

# to do:
# -- more explanations for alpha. to add - allow user to set alpha
# -- more explanations for beta