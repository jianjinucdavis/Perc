#' Find the rank order
#' 
#' \code{sim.rank.order} find the rank order for the win-loss relationship
#' 
#' @param data a data frame or a matrix. the win-loss probability matrix 
#' which is the second output from \code{conductance}
#' @param num number of SimAnnealing (default is set at 10)
#' @param kmax an integer between 2 to 1000
#' @param alpha a positive numeric value (somehow related to the unit of each interaction)
#' @return a dataframe representing simulated rank order.
#' 
#' @details <more information on simAnneal>
#' 
#' @references <add citations here!>
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(sampleEdgelist)
#' # find dominance probability matrix
#' perm2 <- conductance(confmatrix, maxLength = 2)
#' s.rank <- sim.rank.order(perm2$p.hat, num = 5, kmax = 5)
#' head(s.rank)

sim.rank.order <- function(data, num = 10, alpha = NULL, kmax = 5){  # if null, take transitivity; if not null take specify
  # input df, a dataframe, the output from percolation function.
  
  # run the SimAnneal.R manually. 
  percMat2 <- as.matrix(data) 
  ### Run the simulated annealing many times, since it sometimes gets 
  ### stuck in local minima.
  
  # automatically replicate simAnneal for times user specified.
  sim.ann.list <- replicate(num, SimAnneal(percMat2, kmax), simplify = FALSE)
  
  # sim.ann1 = SimAnneal(percMat2, kmax)
  # sim.ann2 = SimAnneal(percMat2, kmax)
  # sim.ann3 = SimAnneal(percMat2, kmax)
  # sim.ann4 = SimAnneal(percMat2, kmax)
  # sim.ann5 = SimAnneal(percMat2, kmax)
  # sim.ann6 = SimAnneal(percMat2, kmax)
  # sim.ann7 = SimAnneal(percMat2, kmax)
  # sim.ann8 = SimAnneal(percMat2, kmax)
  # sim.ann9 = SimAnneal(percMat2, kmax)
  # sim.ann10 = SimAnneal(percMat2, kmax)
  
  ## select the sim.ann with smallest output
  # sim.ann.list <- list(sim.ann1, sim.ann2, sim.ann3, sim.ann4, sim.ann5, 
  #                     sim.ann6, sim.ann7, sim.ann8, sim.ann9, sim.ann10)
  
  # sim.ann.all <- c(sim.ann1[[2]], sim.ann2[[2]], sim.ann3[[2]], sim.ann4[[2]],
  #                  sim.ann5[[2]], sim.ann6[[2]], sim.ann7[[2]], sim.ann8[[2]],
  #                 sim.ann9[[2]], sim.ann10[[2]])
  
  sim.ann.all <- unlist(do.call(rbind, lapply(sim.ann.list, function(x)x[2])))
  Index.sim.ann <- which(sim.ann.all == min(sim.ann.all))[1]
  
  ranking = sim.ann.list[[Index.sim.ann]]$Ordb
  
  RankingOrder <- data.frame(SubjectRanking = 1:length(ranking), 
                             IDindex = ranking)
  IDList.index <- data.frame(IDindex = 1:length(colnames(data)), 
                             ID = sort(colnames(data)))
  
  RankingID <- merge(RankingOrder, IDList.index, by = "IDindex")
  
  Rank <- RankingID[,c("SubjectRanking", "ID")]
  Rank.ordered <- Rank[order(Rank$SubjectRanking), ]
  return(Rank.ordered)
}