"grad.givenIndex" <-
function(index,aGnome,cProbs){
  # called inside grad.gnome
  X <- aGnome$X
  Y <- aGnome$Y
  S <- ncol(Y)
  j <- attr(aGnome$B,"J")[index] + 1  # Species index. Add one to by-pass y0 colomn
  k <- attr(aGnome$B,"K")[index]      # Parameter index
#  sum(Y[,k]*X[,j]) - sum(Y*X[,rep(j,S)]*cProbs[,rep(k,S)]) # Bugged
  sum(Y[,j]*X[,k] - rowSums(Y)*X[,k]*cProbs[,j])
}

