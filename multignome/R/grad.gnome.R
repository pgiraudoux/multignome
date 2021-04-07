"grad.gnome" <-
function(paraV,aGnome) {
  # called from gnome
  # Derive the gradients vector delta(LogL)
  # Create parameter matrix fixing unused parameters at zero
  X <- aGnome$X
  Y <- aGnome$Y
  B <- aGnome$B
  paraM <- rep(0, prod(dim(B)))
  paraM[is.element(attr(B,"pV"),attr(B,"redpV"))] <- paraV
  paraM <- matrix(paraM,nrow=nrow(B))
  cEta <- cbind(0, X %*% paraM)
  cProbs <- t(apply(cEta,1,etaV2prob)) # First element relates to null
  sapply(1:length(paraV), grad.givenIndex, aGnome=aGnome, cProbs=cProbs)
}

