
rankDF <- function(ranking = ranking, data = data, output) {
  RankingOrder <- data.frame(SubjectRanking = 1:length(ranking), 
                             IDindex = ranking)
  IDList.index <- data.frame(IDindex = 1:length(colnames(data)), 
                             ID = sort(colnames(data)), stringsAsFactors = FALSE)
  RankingID <- merge(RankingOrder, IDList.index, by = "IDindex")
  
  Rank <- RankingID[,c("SubjectRanking", "ID")]
  
  if (output == "best") {
    Rank.ordered <- Rank[order(Rank$SubjectRanking), ]
  } else if (output == "all") {
    Rank.ordered <- Rank[order(Rank$ID), ]
  } else {
    stop("use either 'best' or 'all' for output argument.")
  }
  
  return(Rank.ordered)
}





