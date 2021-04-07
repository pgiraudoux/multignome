"simMnData" <-
function(nSp,nO,nEx,size){
  # nSp, number of species
  # nO, number of observations
  # nEx, number of ~ Norm(0,1) explanatory variables, must be at least two due to the behaviour of sapply for nEx<2
  # intercept included by default
  dataMat <- cbind(rep(1,nO),t(sapply(rep(nEx,nO),rnorm)))         # random standard normal data
  nEx <- 1 + nEx                                           # add one to include intercept
  paraM <- matrix(runif(nEx*nSp,-5,5),nrow=nEx,ncol=nSp) # random parameter vector in [-10,10]
  eta <- dataMat %*% paraM                                # linear predictors for each species
  eta <- cbind(0,eta)                                      # include linear predictor for null-response (eg.empty traps)
  probs <- t(apply(eta,1, etaV2prob))                      # convert linear predicotr to probabilities
  colnames(probs) <- c("empty", paste("s",1:nSp, sep=""))
  output <- t(apply(probs,1,simMultinom,n=1,size=size))
  attributes(output)$dataMat <- dataMat
  attributes(output)$pars <- paraM
  output
}

