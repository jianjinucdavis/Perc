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