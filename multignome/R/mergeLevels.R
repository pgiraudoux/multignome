mergeLevels <- function(aFormula,data,mergeFactor,AICtable=FALSE)
{
  ## Purpose:
  ## Produces an n x n model comparison matrix in which the pairs
  ## of factor levels are constrained to share the same regression coefficient.
  ## ----------------------------------------------------------------------
  ## Arguments: a model formula, a dataframe, a character vector
  ## ----------------------------------------------------------------------
  ## Author: Amelie & Dave, Date: 12 Dec 2006

  # require(nnet)
  
  ## Run some checks on the formula
  af2 <- as.character(aFormula[2])
  af3 <- as.character(aFormula[3])
  if (all.equal(mergeFactor, substr(af3,1,nchar(mergeFactor)))!=TRUE) {
    stop("The factor whos levels are to be merged must appear first in the formula")
  }
  aFormula <- formula(paste(af2,"~ -1 + ",af3))
  baseModel <- multinom(aFormula,data=data)
  XYlist <- extractXY(aFormula,data=data)
  mergeCharV <- XYlist$xNames[grep(mergeFactor, XYlist$xNames)]
  mergeCharV <- sub(mergeFactor, "", mergeCharV)
  XYlist$xNames <- sub(mergeFactor, "", XYlist$xNames)
  colnames(XYlist$X) <- XYlist$xNames
  Xnames <- XYlist$xNames
  XYmatrix <- cbind(XYlist$X, XYlist$Y)
  XYdataf <- as.data.frame(XYmatrix)
  nClasses <- length(mergeCharV)
  output <- matrix(0,nClasses, nClasses)
  output2 <- diag(baseModel$AIC,nClasses, nClasses)
  for (i in 1:(nClasses-1)) {
    for (j in (i+1):nClasses) {
    RHS <- paste("I(",mergeCharV[i],"+",mergeCharV[j],")")
      otherX <- Xnames[!is.element(Xnames,mergeCharV[c(i,j)])]
      for(k in 1:length(otherX)){
        RHS <- paste(RHS,otherX[k],sep="+")
      }
      newFormula <- aFormula
      newFormula[3] <- as.call(parse(text=RHS))
      newModel <- multinom(newFormula,XYdataf)
      newAIC <- newModel$AIC
      newDeltaAIC <- newModel$AIC - baseModel$AIC
      newAIC <- newModel$AIC
      output[i,j] <- newDeltaAIC
      output[j,i] <- newDeltaAIC
      output2[i,j] <- newAIC
      output2[j,i] <- newAIC
    }
  }
  colnames(output) <- rownames(output) <- colnames(output2) <- rownames(output2) <- mergeCharV
  if (AICtable==TRUE) Output <- list(deltaAIC=output,AIC=output2) else Output <- output
  Output
}

