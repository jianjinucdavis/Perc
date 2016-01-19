# to do: document all these functions.
getSimOutput <- function(simAnnealList, num){
  costs_all <- unlist(do.call(rbind, lapply(simAnnealList, function(x)x$Cb)))
  best_cost_index <- which.min(costs_all)
  allRankOrder <- lapply(simAnnealList, function(x) x$Ordb)
  names(allRankOrder) <- paste0("SimAnneal", 1:num)
  allRankOrder_df <- data.frame(allRankOrder)
  bestRankOrderIndex <- allRankOrder[[best_cost_index]]
  return(list(
    costs_all = costs_all,
    bestRankOrder = bestRankOrderIndex,
    allRankOrder = allRankOrder_df
  ))
}

getAllCosts <- function(costs_all, num){
  # export costs for each simAnnealRun
  CostOutput <- data.frame(simAnnealRun = c(1:num), Cost = costs_all)
  return(CostOutput)
}

getBestRankOrder <- function(ID_index, bestRankOrder) {
  bestRankOrder <- data.frame(ID = ID_index[bestRankOrder, "ID"],
                              ranking = 1:nrow(ID_index))
  return(bestRankOrder)
}

getAllRankOrder <- function(ID_index, allRankOrder){
  
  allRankOrder_df <- data.frame(apply(allRankOrder, 
                                      2, 
                                      function(x) ID_index[x, "ID"]),
                                stringsAsFactors = FALSE)
  return(allRankOrder_df)
}
