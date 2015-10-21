#' Find rank order using simulated annealing
#' 
#' \code{simRankOrder} find the rank order for the win-loss relationship
#' 
#' @param data a matrix. the win-loss probability matrix 
#' which is the second element of the output from \code{conductance}
#' @param num number of SimAnnealing (default is set at 10)
#' @param kmax an integer between 2 to 1000, indicating the number of simulations in each SimAnnealing.
#' @param alpha a positive integer that 
#' reflects the influence of an observed win/loss interaction 
#' on an underlying win-loss probability. 
#' It is used in the calculation of the posterior distribution 
#' for the win-loss probability of \code{i} over \code{j}: \eqn{Beta(\alpha c_{i,j} +\beta, c_{i,j}+\beta)}{Beta*(\alpha * c_ij + \beta, c_ij + \beta)}. 
#' In the absence of expertise to accurately estimate alpha, 
#' it is estimated from the data.
#' @return a list of two dataframes. 
#'    \item{BestSimulatedRankOrder}{a dataframe representing the best simulated rank order.}
#'    \item{Costs}{the cost of each simulated annealing run}
#'    \item{AllSimulatedRankOrder}{a dataframe representing all simulated rank orders.}
#' 
#' 
#' @references Fushing, H., McAssey, M. P., Beisner, B., & McCowan, B. (2011). Ranking network of a captive rhesus macaque society: a sophisticated corporative kingdom. PLoS One, 6(3), e17817-e17817.
#' 
#' @seealso \code{\link{conductance}} \code{\link{transitivity}}
#' 
#' @examples
#' # convert an edgelist to conflict matrix
#' confmatrix <- as.conflictmat(sampleEdgelist)
#' # find dominance probability matrix
#' perm2 <- conductance(confmatrix, maxLength = 2)
#' \dontrun{
#' # Note: It takes a while to run the simRankOrder example.
#' s.rank <- simRankOrder(perm2$p.hat, num = 10, kmax = 1000)
#' s.rank$BestSimulatedRankOrder
#' s.rank$Costs
#' s.rank$AllSimulatedRankOrder
#' }
#' \dontshow{
#' s.rank <- simRankOrder(perm2$p.hat, num = 2, kmax = 5)
#' s.rank$BestSimulatedRankOrder
#' s.rank$Costs
#' s.rank$AllSimulatedRankOrder
#' }
#' @export

simRankOrder <- function(data, num = 10, alpha = NULL, kmax = 1000){  # if null, take transitivity; if not null take specify
  # the output from percolation function.
  if (!(is.matrix(data))) {
    stop("The second element 'p.hat' from the output of 'conductance' should be used.")
  }
  
  if (any(data < 0)){
    stop("Values smaller than 0 detected. Please check your data. The second element 'p.hat' from the output of 'conductance' should be used.")
  }
  
  if (any(data > 1)){
    stop("Values greater than 1 detected. Please check your data. The second element 'p.hat' from the output of 'conductance' should be used.")
  }
  
  # run the SimAnneal.R manually. 
  percMat2 <- data 
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
  # all costs
  sim.ann.all <- unlist(do.call(rbind, lapply(sim.ann.list, function(x)x[2])))
  # find the lowest
  Index.sim.ann <- which(sim.ann.all == min(sim.ann.all))[1]
  
  # export costs for each simAnnealRun
  CostOutput <- data.frame(simAnnealRun = c(1:num), Cost = sim.ann.all)
  
  ranking = sim.ann.list[[Index.sim.ann]]$Ordb
  
  # use find All Rank Order.R
  bestRankOrder <- rankDF(ranking = ranking, data = data, output = "best")
  # RankingOrder <- data.frame(SubjectRanking = 1:length(ranking), 
  #                             IDindex = ranking)
  # IDList.index <- data.frame(IDindex = 1:length(colnames(data)), 
  #                             ID = sort(colnames(data)), stringsAsFactors = FALSE)
  # RankingID <- merge(RankingOrder, IDList.index, by = "IDindex")
  
  # Rank <- RankingID[,c("SubjectRanking", "ID")]
  # Rank.ordered <- Rank[order(Rank$SubjectRanking), ]
  
  # find all rank order (Ordb)
  allRankOrder <- lapply(sim.ann.list, function(x)x[[3]])
  allRankList <- lapply(allRankOrder, function(x)rankDF(ranking = x, data = data, output = "all"))
  allRankonlyList <- lapply(allRankList, function(x)x[,1])
  RanksDF <- data.frame(do.call(cbind, allRankonlyList))
  names(RanksDF) <- paste0("SimRun", c(1:num))
  allRankingDF <- data.frame(ID = allRankList[[1]]$ID, RanksDF)

  # return(Rank.ordered)
  return(
    list(BestSimulatedRankOrder = bestRankOrder, 
         Costs = CostOutput, 
         AllSimulatedRankOrder = allRankingDF)
    )
}