
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
bt.test = function(conf.mat, baseline = 1, maxLength = 3, reps = 5){  # temp revision, just to run fastly
  n = nrow(conf.mat)
  mle.d = bradleyTerry(conf.mat, baseline = baseline)
  mle.probs = convertToProb(mle.d[[1]]) # use only domInds (vector of length N consiting of the MLE values of the dominance indices.)
  mle.ord = order(mle.d[[1]], decreasing = TRUE)
  cond = conductance(conf.mat, maxLength)
  test.stat = bt.cond.dist(cond$p.hat, mle.probs, mle.ord)
  num.comps = matrix(0, nrow = n, ncol = n)
  num.comps[upper.tri(num.comps)] = conf.mat[upper.tri(conf.mat)] + 
    t(conf.mat)[upper.tri(conf.mat)]
  num.comps[lower.tri(num.comps)] = t(num.comps)[lower.tri(num.comps)] 
  test.dist = replicate(reps, sampleDist(mle.probs, num.comps, baseline, 
                                         maxLength))  # distance not used
  return(list(stat = test.stat, dist = test.dist, 
              p.val = sum((test.dist>test.stat) / reps)))
}


# to do:
#     -- documentation
