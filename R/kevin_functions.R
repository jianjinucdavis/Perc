#edgelist = matrix(order(rnorm(50)), ncol = 2)



### 
# as.conflictmat() takes in two arguments:
#   edgelist: A (K x 2) matrix of edges, with the dominant entity in the 1st column
#   swap.order: If the dominant entity is in the 2nd column, specify as TRUE.
# Outputs a matrix with [i,j]th entry equal to the number of times i dominates j.
# Note: edgelist values should be between 1 and N, where N is the total number of entities.
# Also works to convert a matrix to conf.mat class



as.conflictmat = function(edgelist, swap.order = FALSE){
  if(ncol(edgelist) == nrow(edgelist)){
    class(mat) = c("conf.mat", "matrix")
    return(mat)
  }

  if(ncol(edgelist) != 2){
    stop("Input a matrix with two columns.")
  }
  if(swap.order == TRUE){
    edgelist = edgelist[,2:1]
  }
  N = max(edgelist)
  if(N > 10000){
    stop("Convert edge IDs to integers starting at 1.")
  }
  mat = matrix(0, N, N)
  for(i in 1:nrow(edgelist)){
    mat[edgelist[i,1], edgelist[i,2]] = mat[edgelist[i,1], edgelist[i,2]] + 1
  }
  class(mat) = c("conf.mat", "matrix")
  return(mat)
}




### countPaths() takes in the output of the IDpaths function
#   and returns a list of the same length of matrices.
#   These matrices contain the number of paths from i to j
#   for each order of dominance path.
###

countPaths = function(allPaths){
  nOrders = length(allPaths)
  pathList = list()
  N = max(sapply(allPaths, max))
  
  for(K in 1:nOrders){
    pathList[[K]] = as.conflictmat(allPaths[[K]][,c(1,(K+1))])
  }
  pathList
}




### transitivity() takes in a conflict matrix "conf" and outputs a list of 4 items
#  1: The number of transitive triangles.
#  2: The number of intransitive triangles.
#  3: The order-1 transitivity.
#  4: The value of alpha corresponding to this value of transitivity.
###

transitivity = function(conf){

N = nrow(conf)


### These lines set up the transitivity calculation.
### We are making a matrix of all the possible sets of three subjects.
### We won't really need to refer to this matrix.
### The important part of the calculation is at the end.

numrows = 0
ctr = 0
for(i in 1:(N-2)){
  ctr = ctr + i
  numrows = numrows + ctr
}

firstrow = numeric(0)
for(i in 1:(N-2)){
  temp = rep(i, (N-1-i)*(N-i)/2)
  firstrow = c(firstrow, temp)
}

secondrow = numeric(0)
for(i in 1:(N-2)){ # first row number
for(j in (i+1):(N-1)){
  temp = rep(j, N-j)
  secondrow = c(secondrow, temp)
}
}

thirdrow = numeric(0)
for(i in 3:N){
for(j in i:N){
  thirdrow = c(thirdrow, seq(j, N, 1))
}
}
  

triples = matrix(0, numrows, 3)
triples[,1] = firstrow
triples[,2] = secondrow
triples[,3] = thirdrow


## Here's where the actual transitivity calculation begins.

transitive = 0
intransitive = 0
tList = matrix(0, 0, 4)
iList = matrix(0, 0, 4)
for(i in 1:nrow(triples)){
  tA = triples[i,1]
  tB = triples[i,2]
  tC = triples[i,3]
  AB = conf[tA, tB] - conf[tB, tA]
  AC = conf[tA, tC] - conf[tC, tA]
  BC = conf[tB, tC] - conf[tC, tB]
  ### See if the triangle is transitive...
  if((AC > 0 & BC > 0 & AB != 0) | (AB < 0 & AC < 0 & BC != 0) | (AB > 0 & BC < 0 & AC != 0)){
    transitive = transitive + 1
    tList = rbind(tList, c(triples[i,], i))
  }
  ### See if the triangle is intransitive...
  if((AB > 0 & BC > 0 & AC < 0) | (AB < 0 & AC > 0 & BC < 0)){
    intransitive = intransitive + 1
    iList = rbind(iList, c(triples[i,], i))
  }
}


### Estimated transitivity is the proportion of transitive triangles.
T1 = transitive / (transitive + intransitive)

### T1 is the order-1 transitivity.

### From the paper, we estimate alpha as follows.
alpha = (2 * sqrt(T1) - 1) / (1 - sqrt(T1))

return(list(transitive = transitive, intransitive = intransitive, transitivity = T1, alpha = alpha))
}


    


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

# 70 and below has tickmarks every 10
N = 128
plot.conf.mat(conf2[1:N, 1:N], labels = FALSE)

conf2 = conf
rownames(conf2) = Teams$School
colnames(conf2) = Teams$School


