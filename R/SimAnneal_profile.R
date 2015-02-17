
SimAnneal <- function(PMat, kmax=1000) {

#PMat = ret

## Part I

  N <- nrow(PMat)
  ColSum <- sapply(1:N, function(n) sum(PMat[,n]))
  Order <- order(ColSum)
  C <- Cost(Reorder(PMat, Order))
  C1 <- C
  Cb <- C
  Ordb <- Order
  profCn = numeric(kmax * 10)
  profC = numeric(kmax * 10)
  profCn[1] = C
  profC[1] = C

## Part II
  k <- 0
  l <- 1
  while(k < kmax) {
    NewOrder <- FindNeighbor(PMat,Order)
    Cn <- Cost(Reorder(PMat, NewOrder))
    if(Cn < C) {
      C <- Cn
      Order <- NewOrder
      if(Cn < Cb) {
        Cb <- Cn
        Ordb <- NewOrder
      }
    } else
    if( AP(C, Cn, Temp(k/kmax)) > runif(1) ) {
      C <- Cn
      Order <- NewOrder
    }
    l <- l+1
    profCn[l] = Cn
    profC[l] = C
    if((l %% 10) == 0) k <- k+1
  }
  return(list(C1=C1, Cb=Cb, Ordb=Ordb, profC = profC, profCn = profCn))
}

Cost <- function(Mat) {
  N <- nrow(Mat)
  Cost <- 0
  for(i in 2:N) for(j in 1:(i-1)) Cost <- Cost + max(0, -log(2*(1-Mat[i,j]))) * 
     exp((N+1-j)*(i-j)*2/N^2)
  return(Cost)
}

# This is way faster than the old Reorder function!
Reorder = function(Mat, Ord){
  Mat[Ord, Ord]
}
 

FindNeighbor <- function(Mat, Ord) {
  #Mat = ret
  #Ord = Order
  CMat <- Reorder(Mat, Ord)
  N <- nrow(Mat)
  Vals <- vector("numeric")
  for(i in 1:(N-1))  Vals <- c(Vals, CMat[(i+1):N,i])
  Q <- quantile(Vals, 0.8)
  I <- vector("numeric")
  J <- vector("numeric")
  Pr <- vector("numeric")
  for(i in 2:N) for(j in 1:(i-1)) if(CMat[i,j] >= Q) {
    I <- c(I, i)
    J <- c(J, j)
    Pr <- c(Pr, CMat[i,j])
  }
  Pr <- Pr/sum(Pr)
  S <- sample(x=1:length(Pr), size=1, prob=Pr)
  Seq <- 1:N
  Seq[c(I[S],J[S])] <- Seq[c(J[S],I[S])]
  Ord <- Ord[Seq]
  return(Ord)
}


  

AP <- function(e1, e2, T) {
  if(e2 < e1) return(1)
  return( exp((e1 - e2)/T/0.1) )
}

Temp <- function(num) {
  return( 1 - exp((num-1)*5) )
}
