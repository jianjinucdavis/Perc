#' Find the certainty of dominance interactions
#' 
#' @param data a data frame. Edgelist. With the first column named "Initiator"; the second column named "Recipient". 
#' @param path.length an integer between 2 to 4, representing the length of indirect pathways used in finding dominance interactions.
#' @return a dataframe representing dominance certainty matrix.
#' @examples
#' PercOutput <- PercMatrix(SampleEdgelist, 2)

##==== to do list ====
# Input: 
#    Conflict matrix or edge list (automatically detect type), 
#     list of subjects with either numeric or character strings (optional), alpha value (optional; can have the function calculate it automatically)

# Output:
# * Dominance probability matrix with subject names as row/column names ordered by simulated annealing
# * Costs of different optimal rank orders found + plot of simulated annealing costs
# * Top X simulated annealing solutions found
# * Subject IDs with rank in an Nx2 matrix
# * Heat map of dominance probability matrix
# * Transitivity, alpha, and counts of transitive/intransitive triangles
# * Counts of dominance paths of each length
# * Linearity test statistic + p-value/decision

##==== Correction Notation ====
#** Important correction needed!



PercMatrix <- function(data, path.length = 2) {
  
  ## PercolationMatrix Function takes the input as an edgelist from 
  ##     a directed weighted network. 
  ##   output: a dataframe. Gives the certainty of dominance interactions.
  
  ## 
  
  ## path.legn: the length of indirect pathways used. an integer between 2 to 4.
  
  ## preload IDpaths.R and transitivity.alpha.R
  stopifnot(path.length %in% c(2:4))
  
  data = data[data$Initiator != 0 & data$Recipient != 0,]  ## 
  
  ### Change "Initiator" and "Recipient" to the names of the column headers
  ### This will give you the sorted list of unique subject ID numbers
  subjects = sort(unique(c(data$Initiator, data$Recipient)))
  
  N = length(subjects)
  
  ### Converting the raw data into a conflict matrix called "conf".
  conf = matrix(0, N, N)
  for(j in 1:nrow(data)){
    subject1 = which(subjects == data$Initiator[j])
    subject2 = which(subjects == data$Recipient[j])
    conf[subject1, subject2] = conf[subject1, subject2] + 1
  }
  
  # call IDpaths.R manually into environment
  
  ### You can change this part to identify paths of shorter or longer length.
  ### It currently goes up to length 4. Longer paths can take a long time.
  
  if (path.length == 2){
    allPaths = list(
      which(conf > 0, arr.ind = TRUE),
      do.call(rbind, sapply(1:N, function(k){IDpaths(conf, k, 2)}))
    )
  } else if (path.length == 3) {
    allPaths = list(
      which(conf > 0, arr.ind = TRUE),
      do.call(rbind, sapply(1:N, function(k){IDpaths(conf, k, 2)})),
      do.call(rbind, sapply(1:N, function(k){IDpaths(conf, k, 3)}))
    )
  } else {
    allPaths = list(
      which(conf > 0, arr.ind = TRUE),
      do.call(rbind, sapply(1:N, function(k){IDpaths(conf, k, 2)})),
      do.call(rbind, sapply(1:N, function(k){IDpaths(conf, k, 3)})),
      do.call(rbind, sapply(1:N, function(k){IDpaths(conf, k, 4)}))
    )
  }
  
  
  # all.Paths = list(
  #    which(conf > 0, arr.ind = TRUE),
  #    do.call(rbind, sapply(1:N, function(k){IDpaths(conf, k, 2)})),
  #    do.call(rbind, sapply(1:N, function(k){IDpaths(conf, k, 3)})),
  #    do.call(rbind, sapply(1:N, function(k){IDpaths(conf, k, 4)}))
  #  )
  
  # allPaths = all.Paths[1:path.length] # determine how many path lengths are used.
  
  ### We just need to refer to this number for later.
  maxLength = length(allPaths)
  
  ### percMat will contain direct + indirect information from dominance paths
  percMat = conf
  
  outdegree = rowSums(conf)
  
  ### These can be changed depending on the transitivity calculation.
  ### beta is usually chosen to be 1.
  ### alpha values of 3-8 are the lowest Kevin has dealt with, meaning very little information
  ### is gained from the indirect pathways. For high values in the monkey networks, we tend to choose
  ### a slightly more conservative number than what is calculated.
  
  # 
  
  alpha = transitivity.alpha(conf)
  beta = 1
  
  ### Populating the direct + indirect conflict matrix called "percMat"
  for(k in 2:maxLength){
    for(r in 1:nrow(allPaths[[k]])){
      percMat[allPaths[[k]][r,1], allPaths[[k]][r,k+1]] = percMat[allPaths[[k]][r,1], allPaths[[k]][r,k+1]] + ((alpha + beta)/(alpha + 2*beta)/mean(outdegree))^(k-1)
    }
  }
  
  ### "percMat2" is the estimated dominance probability matrix
  
  percMat2 = matrix(0, N, N)
  for(i in 2:N){
    for(j in 1:(i-1)){
      temp1 = (alpha * percMat[i,j] + beta)/(alpha * percMat[i,j] + alpha * percMat[j,i] + 2*beta)
      temp2 = (alpha * percMat[j,i] + beta)/(alpha * percMat[i,j] + alpha * percMat[j,i] + 2*beta)
      #temp1 = (percMat[i,j])/(percMat[i,j] + percMat[j,i])
      #temp2 = (percMat[j,i])/(percMat[i,j] + percMat[j,i])
      percMat2[i,j] = ifelse(is.nan(temp1), 0.5, temp1)
      percMat2[j,i] = ifelse(is.nan(temp2), 0.5, temp2)
    }
  }
  
  ### Organize Output
  IDList.index <- data.frame(IDindex = 1:length(subjects), ID = subjects)
  
  ### Percolation Matrix
  percMat2.df <- as.data.frame(percMat2)
  
  row.names(percMat2.df) <- subjects
  names(percMat2.df) <- subjects
  return(percMat2.df)
}