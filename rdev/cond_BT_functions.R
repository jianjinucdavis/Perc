library(MASS)
#library(BradleyTerry2)
library(gtools)


IDpaths = function(conf, i, len){
  #############################################################################
  ### Description: function that identifies all unique dominance paths of 
  ###              order (len - 1) beginning at subject i
  ### Input:
  ###   conf - N-by-N conflict matrix whose (i,j)th element is the number of 
  ###          times i defeated j
  ###   i - numeric integer, ID of the subject at the beginning of each 
  ###       dominance path
  ###   len - numeric postitive integer, the length of the dominance paths to 
  ###         be identified (len = order + 1)
  ### Output:
  ###
  #############################################################################
  
  if(sum(conf[i,] > 0) == 0){
    return(matrix(0, 0, len+1))
  }
  levels = list()
  levels[[1]] = which(conf[i,] > 0)
  for(j in 2:len){
    levels[[j]] = lapply(unlist(levels[[j-1]]), function(k) which(conf[k,] > 0))
  }
  ret = matrix(0, length(unlist(levels[[len]])), len+1)
  ret[,1] = i
  ret[,len+1] = unlist(levels[[len]])
  if(len == 2){
    ret[,2] = rep(unlist(levels[[1]]), sapply(levels[[2]], length))
  }
  for(j in len:2){
    currLengths = sapply(levels[[j]], length)
    if(j < len){
      effLengths = numeric(length(currLengths))
      ctr = 1
      for(d in 1:length(effLengths)){
        if(currLengths[d] != 0){
          effLengths[d] = sum(prevLengths[ctr:(ctr + currLengths[d] - 1)])
        }
        else{
          effLengths[d] = 0
        }
        ctr = ctr + currLengths[d]
      }
    }
    else{
      effLengths = currLengths
    }
    if(length(currLengths) == 0){ return(matrix(0, 0, len+1))}
    ret[,j] = rep(unlist(levels[[j-1]]), effLengths)
    prevLengths = effLengths
  }
  isUnique = apply(ret, MARGIN = 1, function(b) {
    length(unique(b)) == len + 1})
  ret[isUnique,]
}



###############################################################################
### Description: Identifies all paths length less than or equal to max.length
###              between all pairs of competitors.  Used in conductance.
### Input:
###   conf - N-by-N conflict matrix whose (i,j)th element is the number of 
###          times i defeated j
###   maxLength - positive numeric integer indicating the maximum length
###                of paths to identify
### Output: list whose elements are all paths of a given length
###############################################################################

allPaths = function(conf, maxLength){
  paths = lapply(2:maxLength, FUN = function(l, conf){
    do.call(rbind, lapply(1:nrow(conf), FUN = IDpaths, conf = conf, l = l))
  }, conf = conf)  
  return(list(which(conf > 0, arr.ind = TRUE), paths))
}



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




###############################################################################
### Description: function converts BT dominance indices into dominance a 
###              matrix of dominance probabilities
### Input: 
###   d - numeric vector of dominance probabilities estimated by the BT model.
###   sumToOne- logical, if TRUE indices are for the "sum to one" 
###               parameterization of the BT model
### Output: N-by-N numeric matrix of dominance probabilities estimated by the
###         BT model
###############################################################################

convertToProb = function(d, sumToOne = FALSE){
  n = length(d)
  m1 = matrix(rep(d, times = n), nrow = n)
  m2 = matrix(rep(d, times = n), nrow = n, byrow = TRUE)
  if(sumToOne){
    P = m1 / (m1 + m2)
  }
  else{
    P = exp(m1) / (exp(m1) + exp(m2))
  }
  diag(P) = 0
  return(P)
}

###############################################################################
###Description: Computes the log-likelihood for the BT model.
###Input:
###  conf.mat - N-by-N conflict matrix whose (i,j)th element is the number of 
###             times i defeated j
###  d - numeric vector of length N consisting of dominance indices
###  sumToOne - logical, TRUE indicate 'sum-to-one' parameterization of BT
###Output: numeric value representing the log-likelihood for given at d
###############################################################################
BTLogLik = function(conf.mat, d, sumToOne = FALSE){
  n = nrow(conf.mat)
  m1 = matrix(rep(d, times = n), nrow = n)
  m2 = matrix(rep(d, times = n), nrow = n, byrow = TRUE)
  log.lik = sum(conf.mat * (m1 - log(exp(m1) + exp(m2))))
  return(log.lik)
}




###############################################################################
###Description: Sample a value of the "systemic test" test statistic for data
###             generated under the BT model.
###Input:
###  prob.mle - N-by-N numeric matrix who entry [i,j] is the BT estimate for
###             the probability that i is dominant over j in a game.
###  num.comps - N-by-N numeric matrix whose entry, [i,j], is the number
###              of observed games between agent i and agent j.
###  baseline - interger between 1 and N inclusive.  ID of baseline agent with
###             dominance index set to 0.
###  maxLength - integer, maximum length of paths used in conductance.
###Output: numeric, value of test statistic for data sampled under BT
###############################################################################
sampleDist = function(prob.mle, num.comps, baseline, maxLength){
  n = nrow(prob.mle)  
  conf = sampleBTConf(n, prob.mle, varcov, num.comps)
  conf.bt = BTMM(conf, baseline = baseline)
  conf.ord = order(conf.bt, decreasing = TRUE)
  conf.cond = conductance(conf, maxLength)
  d = bt.cond.dist(conf.cond$p.hat, convertToProb(conf.bt), conf.ord)
  return(d)
}



###############################################################################
###Description: Sample a conflict matrix based on probabilities from a BT 
###             model.
###Input:
###  n - number of agents (N)
###  p.mat - N-by-N numeric matrix who entry [i,j] is the BT estimate for
###             the probability that i is dominant over j in a game.
###  num.comps - N-by-N numeric matrix whose entry, [i,j], is the number
###              of observed games between agent i and agent j.
###Output: N-by-N conflict matrix simulated under the BT model
###############################################################################
sampleBTConf = function(n, p.mat, num.comps){
  conf = matrix(0, n, n)
  conf[upper.tri(conf)] = rbinom(.5 * n * (n - 1), 
                                 num.comps[upper.tri(num.comps)],
                                 p.mat[upper.tri(p.mat)])
  conf[lower.tri(conf)] = t(num.comps)[lower.tri(num.comps)] - 
    t(conf)[lower.tri(conf)]
  if((sum(rowSums(conf) == 0) + sum(colSums(conf) == 0)) > 0){
    sampleBTConf(n, p.mat, num.comps)
  }
  return(conf)
}




###############################################################################
###Description: Computes the MLE for the BT model using an MM algorithm
###Input:
###  conf.mat - N-by-N conflict matrix whose (i,j)th element is the number of 
###             times i defeated j
###  initial - initial values of dominance indices for the MM algorithm, if 
###            not supplied, the 0 vector will be the inital value.
###  baseline - index for agent to represent baseline dominance index set to
###             0.  If NA, the "sum-to-one" parameterization will be used.
###  stop.dif - numeric value for difference in log likelihood value between
###             iterations.  Used as the convergence criterion for the 
###             algorithm.
###Output: vector of length N consiting of the MLE values of the dominance
###        indices.
###############################################################################
BTMM = function(conf.mat, initial = NA, baseline = NA, stop.dif = .001){
  m = nrow(conf.mat)
  if(length(initial) == 1){
    initial = rep(1/m, times = m)
  }
  d = initial
  W = rowSums(conf.mat)
  N = matrix(0, nrow = m, ncol = m)
  lik.old = BTLogLik(conf.mat, d, TRUE)
  dif = 1
  its = 0
  for(i in 1:(m-1)){
    for(j in (i+1):m){
      N[i,j] = conf.mat[i,j] + conf.mat[j,i]
      N[j,i] = N[i,j]
    }
  }
  while(dif > stop.dif){
    for(i in 1:m){
      C = sapply((1:m)[-i], FUN = function(j, i, N){N[i,j] / (d[i] + d[j])},
                 i = i, N = N)
      d[i] = W[i] * (1 / sum(C))
      d = d/sum(d)
    }
    d[d == 0] = 10^(-43)
    d[d == 1] = 1 - 1^(-43)
    lik.new = BTLogLik(conf.mat, d, TRUE)
    dif = lik.new - lik.old
    lik.old = lik.new
    its = its + 1
  }
  if(!is.na(baseline)){
    d = log(d) - log(d[baseline])
  }
  attr(d, "iterations") = its
  return(d)  
}


###############################################################################
###Description: Computes the distance between two matrices.  Here, the distance
###             is defined as the sum of the square root of the absolute value
###             of the components of each matrix multiplied by the square root
###             of the absolute value of the difference in the row and column
###             number.  The idea is if the matrices are ordered by the ranks
###             of the individuals a bigger differences between high and low
###             ranked agents should have a larger effect.
###Input:
###  m1 - N-by-N numeric matrix of dominance probabilities.
###  m2 - N-by_N numeric matrix of dominance probabilities.
###  ord - integer vector containing integers 1 through N.  Denotes the way
###        the rows and columns of m1 and m2 should be ordered.  This might
###        be the outpout of order(d) where d is dominance indices from the
###        BT model.
###Output: numeric value of the distance between the matrices.
###############################################################################
bt.cond.dist = function(m1, m2, ord){
  mat1 = m1[ord,ord]
  mat2 = m2[ord,ord]
  weights = row(mat1) - col(mat1)
  cost = sum(sqrt(abs(mat1 - mat2)) * sqrt(abs(weights)))
  return(cost)
}


###############################################################################
###Description: Systemic test for the assumptions of the Bradley-Terry model,
###             transitivity and monotonic dominance. That is, if A > B and
###             B > C then A > C and Pr(A beats C) > Pr(B beats C).
###Input: 
###  conf.mat - N-by-N conflict matrix whose (i,j)th element is the number of 
###             times i defeated j
###  baseline - integer between 1 and N inclusive identifying the agent with
###             dominance index equal to zero.
###  maxLength - maximum path length used in conductance
###  reps - number of conflict matrices simulated to estimate the sampling
###         distribution under the BT model.
###Ouput: List of 3 items,
###  $stat - value of the test statistic.
###  $dist - estimated sampling distribution of the test statistics under the
###          BT model.
###  $p.val - p-value of the test.
###############################################################################
condTest = function(conf.mat, baseline, maxLength = 3, reps = 1000){
  n = nrow(conf.mat)
  mle.d = BTMM(conf.mat, baseline = baseline)
  mle.probs = convertToProb(mle.d)
  mle.ord = order(mle.d, decreasing = TRUE)
  cond = conductance(conf.mat, maxLength)
  test.stat = bt.cond.dist(cond$p.hat, mle.probs, mle.ord)
  num.comps = matrix(0, nrow = n, ncol = n)
  num.comps[upper.tri(num.comps)] = conf.mat[upper.tri(conf.mat)] + 
    t(conf.mat)[upper.tri(conf.mat)]
  num.comps[lower.tri(num.comps)] = t(num.comps)[lower.tri(num.comps)] 
  test.dist = replicate(reps, sampleDist(mle.probs, num.comps, baseline, 
                                         maxLength, distance))  
  return(list(stat = test.stat, dist = test.dist, 
              p.val = sum((test.dist>test.stat) / reps)))
}



