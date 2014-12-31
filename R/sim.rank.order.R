#' Find the rank order
#' 
#' @param data a data frame. the output from PercMatrix
#' @param kmax an integer between 2 to 1000
#' @return a dataframe representing simulated rank order.
#' @examples
#' rank <- sim.rank.order(PercOutput, 5)
#' head(rank)

sim.rank.order <- function(data, kmax = 5){
  # input df, a dataframe, the output from percolation function.
  
  # run the SimAnneal.R manually. 
  percMat2 <- as.matrix(data) 
  ### Run the simulated annealing many times, since it sometimes gets 
  ### stuck in local minima.
  sim.ann1 = SimAnneal(percMat2, kmax)
  sim.ann2 = SimAnneal(percMat2, kmax)
  sim.ann3 = SimAnneal(percMat2, kmax)
  sim.ann4 = SimAnneal(percMat2, kmax)
  sim.ann5 = SimAnneal(percMat2, kmax)
  sim.ann6 = SimAnneal(percMat2, kmax)
  sim.ann7 = SimAnneal(percMat2, kmax)
  sim.ann8 = SimAnneal(percMat2, kmax)
  sim.ann9 = SimAnneal(percMat2, kmax)
  sim.ann10 = SimAnneal(percMat2, kmax)
  
  ## select the sim.ann with smallest output
  sim.ann.list <- list(sim.ann1, sim.ann2, sim.ann3, sim.ann4, sim.ann5, 
                       sim.ann6, sim.ann7, sim.ann8, sim.ann9, sim.ann10)
  
  sim.ann.all <- c(sim.ann1[[2]], sim.ann2[[2]], sim.ann3[[2]], sim.ann4[[2]],
                   sim.ann5[[2]], sim.ann6[[2]], sim.ann7[[2]], sim.ann8[[2]],
                   sim.ann9[[2]], sim.ann10[[2]])
  
  sim.ann.min <- min(sim.ann.all)
  
  Index.sim.ann <- which(sim.ann.all == sim.ann.min)
  
  ranking = sim.ann.list[[Index.sim.ann]]$Ordb
  
  RankingOrder <- data.frame(SubjectRanking = 1:length(ranking), 
                             IDindex = ranking)
  IDList.index <- data.frame(IDindex = 1:length(names(data)), 
                             ID = sort(as.numeric(names(data))))
  
  RankingID <- merge(RankingOrder, IDList.index, by = "IDindex")
  
  Rank <- RankingID[,c("SubjectRanking", "ID")]
  Rank.ordered <- Rank[order(Rank$SubjectRanking), ]
  return(Rank.ordered)
}