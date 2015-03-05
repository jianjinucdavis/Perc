library("lattice")
### plot.conf.mat() takes in three arguments.
# a conflict matrix (or any square matrix) conf.mat()
# a reordering of the rows/columns, specified by a permutation of 1:N
# whether the agent names are specified in the rownames() of conf.mat()
#    and the user would like them displayed on the heatmap



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
  
  levelplot(t(conf.mat.ord)[,ncol(conf.mat.ord):1], col.regions = colors, 
            xlab = "Loser", ylab = "Winner",
            scales=list(
              x=list(labels=lbls, at = x.values, rot = ifelse(labels == TRUE, 90, 0)),
              y=list(labels=lbls, at = y.values)
            )
  )
}

# to do: test on a large dataset.
