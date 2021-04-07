"parNames" <-
function(Xcmbd,termsList){
  # Called from inside combineFrames
  nSpecies <- length(termsList)
  tfFrame <- matrix(NA,nrow=ncol(Xcmbd),ncol=nSpecies) # true/false frame
  pNames <- tfFrame                                    # parameter frame
  pNames <- matrix(rep(letters[1:nrow(pNames)],ncol(pNames)),nrow=nrow(pNames))
  nM <- matrix(rep(1:ncol(pNames),nrow(pNames)),ncol=ncol(pNames), byrow=TRUE)
  pNames <- matrix(paste(pNames,nM,sep=""),nrow=nrow(pNames))
  
  Xnames <- colnames(Xcmbd)
  tfList <- list() # TRUE / FALSE list
  for (i in 1:nSpecies) {
    tfList[[i]] <- is.element(colnames(Xcmbd), termsList[[i]])
    tfFrame[,i] <- tfList[[i]]
  }
  pNames[!tfFrame] <- NA
  pV <- as.vector(pNames) # parameter vector
  redpV <- pV[!is.na(pV)] # reduced parameter vector, coefficients for optimisation
  attr(pNames,"pV") <- pV
  attr(pNames,"redpV") <- redpV
  # Index for Species
  attr(pNames,"J") <- as.vector(matrix(rep(1:ncol(pNames),nrow(pNames)),nrow=nrow(pNames),byrow=TRUE)[!is.na(pNames)])
  # Index for Parameters
  attr(pNames,"K") <- as.vector(matrix(rep(1:nrow(pNames),ncol(pNames)),nrow=nrow(pNames))[!is.na(pNames)])
  pNames
}

