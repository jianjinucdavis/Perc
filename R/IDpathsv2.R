
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

# to do: to test from the very beginning
# compare it with IDpaths.R