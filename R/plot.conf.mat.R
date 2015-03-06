#' generate heat map for a conflict matrix or dominance probability matrix
#' 
#' \code{plot.conf.mat} generate heat map for a conflict matrix or a dominance probability matrix
#' 
#' @param conf.mat an N-by-N matrix. Either a conflict matrix or a dominance probability matrix (the second element from \code{conductance} output)
#' @param ordering a reordering of the rows/columns, specified by a permutation of 1:N
#' @param labels if TRUE, displaying the agent names as specified in the rownames() of conf.mat() on the heatmap
#' @return A heatmap
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(SampleEdgelist)
#' # find dominance probability matrix
#' perm2 <- conductance(confmatrix, 2)
#' # plotting
#' plot.conf.mat(perm2$p.hat)

plot.conf.mat = function(conf.mat, ordering = NA, labels = FALSE){
  
  if(length(rownames(conf.mat)) == 0){
    labels = FALSE
  }
  
  if(length(ordering) == 1){
    ordering = 1:ncol(conf.mat)
  }
  
  conf.mat.ord = conf.mat[ordering, ordering]
  ramp = colorRamp(c("white","blue", "orange", "red"))
  colors = rgb(ramp(seq(0, 1, length = 1000)), max = 255)
  
  N = nrow(conf.mat)
  
  tickdist = ifelse(N > 70, 20, ifelse(N > 30, 10, 5))
  
  if(labels == FALSE){
    low = N - floor(N/tickdist)*tickdist + 1
    x.values = rev(seq(tickdist,N,tickdist))
    y.values = seq(low, N, tickdist)
    lbls = rev(seq(tickdist, N, tickdist))
  }
  else{
    lbls = rownames(conf.mat)
    x.values = 1:N
    y.values = rev(1:N)
  }
  
  lattice::levelplot(t(conf.mat.ord)[,ncol(conf.mat.ord):1], col.regions = colors, 
            xlab = "Loser", ylab = "Winner",
            scales=list(
              x=list(labels=lbls, at = x.values, rot = ifelse(labels == TRUE, 90, 0)),
              y=list(labels=lbls, at = y.values)
            )
  )
}

# to do: test on a large dataset.
