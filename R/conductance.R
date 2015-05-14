#' compute conductance dominance probabilities
#' 
#' \code{conductance} compute conductance dominance probabilities
#' 
#' @param conf N-by-N conflict matrix whose (i,j)th element is the number of times i defeated j
#' @param maxLength a positive numeric integer indicating the maximum length of paths to identify
#' @param alpha positive numeric value (more explanation)
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

#' dominance probability matrix value converter
#' 
#' \code{valueConverter} convert values in the dominance probability matrix into 0.5 - 1.0
#' 
#' @param matrix the second output from \code{conductance}
#' @return a matrix of win-loss probability ranging from 0.5 - 1.0.
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(SampleEdgelist)
#' # find dominance probability matrix
#' perm2 <- conductance(confmatrix, 2)
#' perm2$imputed.conf
#' perm2$p.hat
#' convertedValue <- valueConverter(perm2$p.hat)

valueConverter <- function(matrix){
  matrixAbove0.5 <- abs(0.5 - matrix) + 0.5
  return(matrixAbove0.5)
}

#' dyadic long format converter
#' 
#' \code{dyadicLongConverter} convert dominance probability matrix into long format for each dyad
#' 
#' @param matrix the second output from \code{conductance}
#' @return a dataframe of dyadic level win-loss probability.
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(SampleEdgelist)
#' # find dominance probability matrix
#' perm2 <- conductance(confmatrix, 2)
#' perm2$imputed.conf
#' perm2$p.hat
#' dl <- dyadicLongConverter(perm2$p.hat)


dyadicLongConverter <- function(matrix){
  matrix[lower.tri(matrix, diag = TRUE)] <- NA
  dp.df <- as.data.frame(matrix)
  dp.df2 <- dp.df
  dp.df2$rowID <- rownames(dp.df)
  dp.long <- reshape2::melt(dp.df2, 
                            id.vars = "rowID", 
                            variable.name = "ID2",
                            value.name = "ID1 Win Probability")
  names(dp.long)[1] <- "ID1"
  dpComplete <- dp.long[complete.cases(dp.long), ]
  dpComplete[,"ID2 Win Probability"] <- 1 - dpComplete[,3]
  return(dpComplete)
}


#' individual-level probability converter
#' 
#' \code{individualWinProb} convert dominance probability matrix into long format for each dyad
#' 
#' @param matrix the second output from \code{conductance}
#' @return a dataframe. Averaging probability of win-loss relationship with all other individuals.
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(SampleEdgelist)
#' # find dominance probability matrix
#' perm2 <- conductance(confmatrix, 2)
#' perm2$imputed.conf
#' perm2$p.hat
#' individualLevelOutput <- individualWinProb(perm2$p.hat)

individualWinProb <- function(matrix){
  matrixAbove0.5 <- valueConverter(matrix)
  
  Mean <- apply(data.frame(valueConverter(matrixAbove0.5)), 2, mean)
  SD <- apply(data.frame(valueConverter(matrixAbove0.5)), 2, sd)
  
  attributes(Mean) <- NULL
  attributes(SD) <- NULL
  
  individualProb <- data.frame(ID = rownames(matrix), Mean = Mean, SD = SD)
  return(individualProb)
}


# to do:
# -- transform output: transformed matrix value: 0.5 - 1.0
# -- individual level output
# -- dyadic level output
# -- more explanations for alpha. to add - allow user to set alpha
# -- more explanations for beta