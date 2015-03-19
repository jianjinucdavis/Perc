
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
