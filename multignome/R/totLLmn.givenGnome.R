"totLLmn.givenGnome" <-
function(paraV,aGnome){
  # aGnome: a list object for fitting multinomial models with seperate regression equations for each species of Gnome captured.
  # Y: a multinomial response matrix. The first colomn should repressent the null response - ie. no captured Gnomes.
  # X: explanatory variable matrix.
  # B: names of the species specific parameters. B indicates which colomns of X are to be used to calculate the capture probabillities of each Gnome species.
  X <- aGnome$X
  Y <- aGnome$Y
  B <- aGnome$B
  # Create parameter matrix fixing unused parameters at zero
  paraM <- rep(0,  prod(dim(B)))
  paraM[is.element(attr(B,"pV"),attr(B,"redpV"))] <- paraV
  paraM <- matrix(paraM,nrow=nrow(B))
  # Derived probabilities and log likelihood
  cEta <- cbind(0, X %*% paraM)
  (cProbs <- t(apply(cEta,1,etaV2prob)))
  totLLmn(cProbs, Y)
}

