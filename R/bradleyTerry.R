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
###Output: 
###  domInds - vector of length N consiting of the MLE values of the dominance
###        indices.
###
###  probMat - N-by-N numeric matrix of dominance probabilities estimated by the
###         BT model
###############################################################################
bradleyTerry = function(conf.mat, initial = NA, baseline = NA, 
                        stop.dif = .001){
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
  logLik = lik.new
  attr(logLik, "iterations") = its
  return(list(domInds = d, probMat = convertToProb(d, is.na(baseline)), 
              logLik = logLik))  
}


# to do:
#     -- documentation on outputs: logLik ?

