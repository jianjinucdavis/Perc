###############################################################################
### Description: Function to compute conductance dominance probabilities.
### Input:
###   conf - N-by-N conflict matrix whose (i,j)th element is the number of 
###          times i defeated j
###   maxLength - positive numeric integer indicating the maximum length
###                of paths to identify
###   alpha - positive numeric value
###   beta - positive numeric value
### Output: Returns a list with two elements.
###   $imputed.conf - an N-by-N conflict matrix whose (i,j)th element is the
###                   'effective' number of wins of i over j.
###   $p.mat - an N-by-N numeric matrix whose (i,j)th element is the estimated
###            conductance dominance probability.
###############################################################################


conductance = function(conf, maxLength, alpha = 6, beta = 1){
  N = nrow(conf)
  
  ### percMat will contain direct + indirect information from dominance paths
  percMat = conf
  
  outdegree = rowSums(conf)
  
  if(sum(conf[row(conf) != col(conf)] == 0) > 0){
    paths = allPaths(conf, maxLength)
    
    ### Populating the direct + indirect conflict matrix called "percMat"
    for(k in 1:(maxLength - 1)){
      for(r in 1:nrow(paths[[2]][[k]])){
        percMat[paths[[2]][[k]][r,1], paths[[2]][[k]][r,k+1]] = 
          percMat[paths[[2]][[k]][r,1], paths[[2]][[k]][r,k+1]] + 
          ((alpha + beta)/(alpha + 2*beta)/mean(outdegree))^k
        gc()
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
      if(verbose){print(i)}
    }
  }
  
  return(list(imputed.conf = percMat, p.hat = percMat2))  
}