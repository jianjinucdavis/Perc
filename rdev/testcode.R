
# Play with Perc package
library(Perc)

#### Try with sample data automatically loaded with the package####

?findIDpaths # find pathways of a certain length for an individual
confmatrix <- as.conflictmat(SampleEdgelist)
path38891 <- findIDpaths(confmatrix, ID = 38891, len = 3)
head(path38891)


# Identifies all paths length less than or equal to maxLength 
#       between all pairs of competitors
?findAllPaths 
# convert an edgelist to conflict matrix
confmatrix <- as.conflictmat(SampleEdgelist)
# find all paths of legnth 3
allp.3 <- findAllPaths(confmatrix, maxLength = 3)
str(allp.3)
head(allp.3[[1]])
head(allp.3[[2]])

#calculate transitivity measurements for a conflict matrix
?transitivity
# convert an edgelist to conflict matrix
confmatrix <- as.conflictmat(SampleEdgelist)
# transitivity calculation
conftrans <- transitivity(confmatrix)
conftrans$transitive      # number of transitive triangles
conftrans$intransitive    # number of intransitive triangles
conftrans$transitivity    
conftrans$alpha



# convert an edgelist to conflict matrix
confmatrix <- as.conflictmat(SampleEdgelist)
# find dominance probability matrix
perm2 <- conductance(confmatrix, 2)
perm2$imputed.conf
domProb.matrix <- perm2[[2]]

## convert domProb matrix into long format =====
# load reshape2 package
library(reshape2)

# convert matrix to a dataframe
dp.df <- as.data.frame(domProb.matrix)

# save the rowname into a variable "rowID" for the domProb dataframe
dp.df2 <- dp.df
dp.df2$rowID <- rownames(dp.df)
str(dp.df2)

# use melt function in reshape2 to convert the dataframe into long format
?melt
dp.long <- melt(dp.df2, 
                id.vars = "rowID", 
                variable.name = "colID", 
                value.name = "DomProb")
str(dp.long)

# check it
head(dp.long)
head(dp.df, n = 10)
## ===============================================
# plot
?plot.conf.mat
plot.conf.mat(perm2$p.hat)

# rank
s.rank <- sim.rank.order(perm2$p.hat, num = 5, kmax = 5)
head(s.rank)


#### try with external data ####
getwd()
# save the "EdgeListFollowupBiweek10.3days.csv" into your working folder
week10.3days <- read.csv("./EdgeListFollowupBiweek10.3days.csv")

confmatrix <- as.conflictmat(week10.3days)
path38891 <- findIDpaths(confmatrix, ID = 38891, len = 2)
head(path38891)


# Identifies all paths length less than or equal to maxLength 
#       between all pairs of competitors
# convert an edgelist to conflict matrix
confmatrix <- as.conflictmat(week10.3days)
# find all paths of legnth 3
allp.3 <- findAllPaths(confmatrix, 3)
str(allp.3)
head(allp.3[[1]])
head(allp.3[[2]])

#calculate transitivity measurements for a conflict matrix
# convert an edgelist to conflict matrix
confmatrix <- as.conflictmat(week10.3days)
# transitivity calculation
conftrans <- transitivity(confmatrix)
conftrans$transitive      # number of transitive triangles
conftrans$intransitive    # number of intransitive triangles
conftrans$transitivity    
conftrans$alpha


# convert an edgelist to conflict matrix
confmatrix <- as.conflictmat(week10.3days)
# find dominance probability matrix
perm2 <- conductance(confmatrix, 2)
perm2$imputed.conf
perm2$p.hat

# plot
plot.conf.mat(perm2$p.hat)

s.rank <- sim.rank.order(perm2$p.hat, num = 5, kmax = 5)
head(s.rank)

#### try running a real dataset. ####
# save the "EdgeListPreRemoval.csv" into your working folder
preremoval <- read.csv("./EdgeListPreRemoval.csv")
str(preremoval)
confmatrix <- as.conflictmat(preremoval)
path38891 <- findIDpaths(confmatrix, ID = 38891, len = 2)
head(path38891)


# Identifies all paths length less than or equal to maxLength 
#       between all pairs of competitors
# convert an edgelist to conflict matrix
confmatrix <- as.conflictmat(preremoval)
# find all paths of legnth 3
allp.3 <- findAllPaths(confmatrix, 3)
str(allp.3)
head(allp.3[[1]])
head(allp.3[[2]])


#calculate transitivity measurements for a conflict matrix
# convert an edgelist to conflict matrix
confmatrix <- as.conflictmat(preremoval)
# transitivity calculation
conftrans <- transitivity(confmatrix)
conftrans$transitive      # number of transitive triangles
conftrans$intransitive    # number of intransitive triangles
conftrans$transitivity    
conftrans$alpha



# convert an edgelist to conflict matrix
confmatrix <- as.conflictmat(preremoval)
# find dominance probability matrix
perm2 <- conductance(confmatrix, 2)
perm2$imputed.conf
perm2$p.hat

# plot
plot.conf.mat(perm2$p.hat)

# rank
s.rank <- sim.rank.order(perm2$p.hat, num = 5, kmax = 5)
head(s.rank)

#### Running on multiple large datasets.
dyadicedgelist <- read.csv("./DyadicFightsEdgeList.csv")
str(dyadicedgelist)
el.pre <- dyadicedgelist[dyadicedgelist$Period == "PreRemoval", 2:3]
el.post <- dyadicedgelist[dyadicedgelist$Period == "PostRemoval", 2:3]
el.followup <- dyadicedgelist[dyadicedgelist$Period == "Followup", 2:3]

str(el.pre)
str(el.post)
str(el.followup)

el.list <- list(el.pre, el.post, el.followup)

# convert each edgelist to conf.matrix
confmatrix.list <- list()
for (i in 1:length(el.list)){
  confmatrix.list[[i]] <- as.conflictmat(el.list[[i]])
}

head(confmatrix.list[[1]])
head(confmatrix.list[[2]])
head(confmatrix.list[[3]])
# find all pathways for each conf.matrix

allp.list <- list()
for (i in 1:length(confmatrix.list)){
  allp.list[[i]] <- findAllPaths(confmatrix.list[[i]], maxLength = 3)
  cat("conf matrix =", i, "\n")
}

str(allp.list)

# for confmatrix 1:
head(allp.list[[1]][[1]])
head(allp.list[[1]][[2]])

# for confmatrix 2:
head(allp.list[[2]][[1]])
head(allp.list[[2]][[2]])

# for confmatrix 3:
head(allp.list[[3]][[1]])
head(allp.list[[3]][[2]])

# transitivity
trans.list <- list()
for (i in 1:length(confmatrix.list)){
  trans.list[[i]] <- transitivity(confmatrix.list[[i]])
  cat("conf matrix =", i, "\n")
}

str(trans.list)

perm2.list <- list() 
for (i in 1:length(confmatrix.list)) {
  perm2.list[[i]] <- conductance(confmatrix.list[[i]], 2)
  cat("conf matrix =", i, "\n")
}

str(perm2.list)

head(perm2.list[[1]]$p.hat) # confmatrix 1
head(perm2.list[[2]]$p.hat) # confmatrix 2
head(perm2.list[[3]]$p.hat) # confmatrix 3

a <- plot.conf.mat(perm2.list[[1]]$p.hat)
a
b <- plot.conf.mat(perm2.list[[2]]$p.hat)
b
plot.conf.mat(perm2.list[[3]]$p.hat)

# select p.hat for each conf matrix output
library(rlist)
perm2.phat.list <- unlist(list.select(perm2.list, p.hat), recursive = 0)
str(perm2.phat.list)

# plotting
plot.list <- list()
for (i in 1:length(perm2.phat.list)){
  plot.list[[i]]  <- plot.conf.mat(perm2.phat.list[[i]])
  cat("plot = ", i, "\n")
}
plot.list[[1]]
plot.list[[2]]
plot.list[[3]]

# Ranking
s.rank.list <- list()
for (i in 1:length(perm2.phat.list)){
  s.rank.list[[i]] <- sim.rank.order(perm2.phat.list[[i]], num = 10, kmax = 100)
  write.csv(paste0(cat("./simrank", i), ".csv"))
  cat("data runned = ", i, "\n")
}

str(s.rank.list)
s.rank.list[[1]]
s.rank.list[[2]]
s.rank.list[[3]]

