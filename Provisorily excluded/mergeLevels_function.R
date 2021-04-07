## Adapted and modified by Amelie from the original mergeLevels function.

mergeLevels_2 <- function(aFormula,data,mergeFactor,AICtable=FALSE)
{
  ## Purpose: Produces an n x n model comparison matrix in which the pairs of factor levels are forced to share the same regression coefficient. 
  ## ----------------------------------------------------------------------
  ## Arguments: a model formula, a dataframe, a character vector
  ## ----------------------------------------------------------------------
  ## Author: Amelie & Dave, Date: 23 mar 2006, 19:35 
  baseModel <- multinom(aFormula,data=data,trace=FALSE)
  K <- length(baseModel$coefnames)
  n <- nrow(data)
  baseModel_AICc <- baseModel$AIC + (2*K*(K+1))/(n-K-1)
  XYlist <- extractXY(aFormula,data=data)
  mergeCharV <- XYlist$xNames[grep(mergeFactor, XYlist$xNames)]  
  mergeCharV <- sub(mergeFactor, "", mergeCharV)
  
  ## DON'T FORGET THE INTERCEPT
  ## EITHER ADD IT HERE OR INCLUDE IT BY DEFAULT LATTER
  
  XYlist$xNames <- sub(mergeFactor, "", XYlist$xNames)
  colnames(XYlist$X) <- XYlist$xNames
  
  Xnames <- XYlist$xNames
  ## This removes "(intercept)" as it is included by default by multinom
  if (TRUE==all.equal(Xnames[1],"(Intercept)")) Xnames <- Xnames[-1] ##  else stop("Function not defined for fitting models without an intercept.")
  
  XYmatrix <- cbind(XYlist$X, XYlist$Y)
  XYdataf <- as.data.frame(XYmatrix)
  ## To Do: check colnames
  ## colnames(XYdataf) <- sub(mergeFactor,"",colnames(XYdataf)) ## Handled above now
  nClasses <- length(mergeCharV)
  
  ## WHY WERE THESE N+1 X N+1 AND NOT N X N ????????
  ## the answer lies in how nClasses is defined;
  ## the level included as base-line (intercept) is not included in nClasses
  output <- matrix(0,nClasses+1, nClasses+1)
                                        #output2 <- diag(baseModel$AIC,nClasses+1, nClasses+1) 
  output2 <- diag(baseModel_AICc,nClasses+1, nClasses+1) 
  
  
  ## Create a new formula & model to apply for each merge:
  ## When i==j there is no need to consider that merge
  ## but we use that opportunity to analyse the merge (intercept,level_j)
  ## so the result is stored in
  ## the ith row of first col
  ## and ith col of first row
  for (i in 1:nClasses) {
    for (j in i:nClasses) {
      
      ## If nClasses=1 (2 classes only)
      if(nClasses==1){RHS <- 1}
      
      else{
        
        
        startAt <-  min((1:nClasses)[!is.element(1:nClasses,c(i,j))])
        ## The following might be false when mergeCharV is length 2
        if (is.finite(startAt)) {
          RHS <- mergeCharV[startAt] 
          for (k in (startAt+1):length(mergeCharV)) {
            if (k!=j & k!=i & k<=length(mergeCharV)) {
              RHS <- paste(RHS,"+",mergeCharV[k])
            }
          }
        }
        else RHS <- "" ## has replaced "" per 1 (24/06/10)
        if (i!=j) RHS <- paste(RHS,"+","I(",mergeCharV[i],"+",mergeCharV[j],")")
        
        otherX <- Xnames[!is.element(Xnames,mergeCharV)]
        if (length(otherX)==0) {RHS <- RHS}
        else {
          
          for(k in 1:length(otherX)){
            RHS <- paste(RHS,otherX[k],sep="+")
          }
        }
      }
      
      newFormula <- aFormula
      newFormula[3] <- as.call(parse(text=RHS))
      newModel <- multinom(newFormula,XYdataf,trace=FALSE)
      k <- length(newModel$coefnames)
                                        #newAIC <- newModel$AIC     
      newAICc <- (newModel$AIC) + (2*k*(k+1))/(n-k-1)
                                        #newDeltaAIC <- newModel$AIC - baseModel$AIC
      newDeltaAICc <- newAICc - baseModel_AICc 
                                        #newAIC <- newModel$AIC
      
      if (i==j) {
        ## results for merges with intercept
        output[1,i+1] <- newDeltaAICc
        output[i+1,1] <- newDeltaAICc
        output2[1,i+1] <- newAICc
        output2[i+1,1] <- newAICc
      } else {
        ## results for merges between non-intercept levels
        output[i+1,j+1] <- newDeltaAICc
        output[j+1,i+1] <- newDeltaAICc
        output2[i+1,j+1] <- newAICc
        output2[j+1,i+1] <- newAICc
      }        
    }   
  }
  
  
  ## Obtain the order in which factor levels were merged for col and row naming
  factorLevels <- eval(parse(text=paste("data$",mergeFactor,sep="")))
  factorLevels <- factorLevels[!duplicated(factorLevels)]
  
  colnames(output) <-c(as.character(factorLevels[!is.element(factorLevels,mergeCharV)]),mergeCharV)
  rownames(output) <-colnames(output)
  
  colnames(output2) <- colnames(output)  
  rownames(output2) <- colnames(output) 
  
  if (AICtable == TRUE) attr(output,"AICtable") <- output2
  index <- output==min(output)
  
  if (sum(index)!=ncol(output)) { 
    attr(output,"mostLikelyMerge") <- names(colSums(index)[colSums(index)==1])
  } else {
    attr(output,"mostLikelyMerge") <- "No merge"
  }
  print(paste("minimum AIC",min(output)))
  output
}
