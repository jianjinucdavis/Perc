#' compute win-loss probabilities
#' 
#' \code{conductance} compute win-loss probabilities for all possible pairs
#'  based upon the combined information from directed wins/losses and 
#'  indirect win/loss pathways from the network.
#' 
#' @param conf N-by-N conflict matrix whose \code{(i,j)}th element is the number of times i defeated j. It is the output from \code{as.conflictmat}
#' @param maxLength a positive numeric integer indicating the maximum length of paths to identify
#' @param alpha a positive integer that 
#' reflects the influence of an observed win/loss interaction 
#' on an underlying win-loss probability. 
#' It is used in the calculation of the posterior distribution 
#' for the win-loss probability of \code{i} over \code{j}: \eqn{Beta(\alpha c_{i,j} +\beta, c_{i,j}+\beta)}{Beta*(\alpha * c_ij + \beta, c_ij + \beta)}. 
#' In the absence of expertise to accurately estimate alpha, 
#' it is estimated from the data.
#' @param beta a positive numeric value that, like alpha, 
#' reflects the influence of an observed win/loss interaction 
#' on an underlying win-loss probability. 
#' Both α and β are chosen such that \eqn{((\alpha + \beta)/(\alpha + 2\beta))}{((α + β)/(α + 2β))^2} is 
#' equal to the order-1 transitivity of the observed network. 
#' Therefore, β is commonly set to 1.
#' @return a list of two elements. 
#' 
#'  \item{imputed.conf}{An N-by-N conflict matrix whose \code{(i,j)}th element is the 
#'    'effective' number of wins of \code{i} over \code{j}.}
#'    
#'  \item{p.mat}{An N-by-N numeric matrix whose \code{(i,j)}th element is the estimated 
#'      win-loss probability.}
#'      
#' @details This function performs two major steps. 
#' First, repeated random walks through the empirical network 
#' identify all possible directed win-loss pathways 
#' between each pair of nodes in the network. 
#' Second, the information from both direct wins/losses and 
#' pathways of win/loss interactions are combined into an estimate of 
#' the underlying probability of \code{i} over \code{j}, for all \code{ij} pairs.
#' 
#' @references Fushing H, McAssey M, Beisner BA, McCowan B. 2011. 
#' Ranking network of a captive rhesus macaque society: a sophisticated corporative kingdom. 
#' PLoS ONE 6(3):e17817.
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(sampleEdgelist)
#' # find win-loss probability matrix
#' perm2 <- conductance(confmatrix, 2)
#' perm2$imputed.conf
#' perm2$p.hat

conductance = function(conf, maxLength, alpha = NULL, beta = 1){
  N = nrow(conf)
  
  ### percMat will contain direct + indirect information from win-loss paths
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
  
  
  ### "percMat2" is the estimated win-loss probability matrix
  
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

#' win-loss probability matrix value converter
#' 
#' \code{valueConverter} converts or transforms all values (which range from 0.0 to 1.0)
#'  in the win-loss probability matrix into 0.5 - 1.0
#' 
#' @param matrix the win-loss matrix which is the second output from \code{conductance}. 
#' @return a matrix of win-loss probability ranging from 0.5 - 1.0.
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(sampleEdgelist)
#' # find win-loss probability matrix
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
#' \code{dyadicLongConverter} convert win-loss probability matrix into long format for each dyad
#' 
#' @param matrix the win-loss matrix which is the second output from \code{conductance}. 
#' @return a dataframe of dyadic level win-loss probability.
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(sampleEdgelist)
#' # find win-loss probability matrix
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
#' \code{individualWinProb} convert win-loss probability matrix into long format for each dyad
#' 
#' @param matrix the win-loss matrix which is the second output from \code{conductance}. 
#' @return a dataframe. Averaging probability of win-loss relationship with all other individuals.
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(sampleEdgelist)
#' # find win-loss probability matrix
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
# -- more explanations for alpha. to add - allow user to set alpha
# -- more explanations for beta