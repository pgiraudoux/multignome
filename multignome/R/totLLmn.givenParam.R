"totLLmn.givenParam" <-
function(paraM, mnData, expData){
  # expData matrix of explanatory variables, dim a*b, includes intercept, ie. the model matrix
  # paraM a matrix of parameters, dim b*c, b the number of model parameters, c-1 the number of species
  # mnData matrix of observations, dim a*c, a the number of observations (eg. trapnights)
  if(floor(length(as.numeric(paraM))/ncol(expData)) != (length(as.numeric(paraM))/ncol(expData))) stop("parameters and data dimensions non-compatible")
  paraM <- matrix(paraM, nrow=ncol(expData))
  cEta <- cbind(0, expData %*% paraM)
  cProbs <- t(apply(cEta,1,etaV2prob))
  totLLmn(cProbs,mnData)
}

