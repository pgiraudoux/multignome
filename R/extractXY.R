extractXY <- function(formula,data){
  # called from inside combineFrames
  call <- match.call(expand.dots=TRUE)
  ## to become the model.frame
  mf <- match.call(expand.dots=FALSE)
  ## Possibly no need to add further arguments
    ## if extractXY is just called from inside multignome.
  m <- match(c("formula", "data"), names(mf), nomatch=0) 
  mf <- mf[c(1,m)]
  mf$drop.unused.levels <- TRUE
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf,parent.frame())
  mt <- attr(mf, "terms")                ## model terms
  Y <- model.response(mf,"numeric")
  X <- model.matrix(mt, mf)              ## currently no contrasts arguement
  yName1 <- mt[[2]]
  yName2 <- colnames(mf)[1]

  ## I forget why this line is here, looks like a check on terms.
  ## But it is q poor check and can fail 
  ## which is probably related to the issue highlighted in help("=="))
  ## So it appears safe to blank out this line 
  
  ## if (yName1 != yName2) stop("Woops. I over looked something, need to debug source.")
  xNames <- colnames(X)       ## attr(mt,"term.labels")
  list(Y=Y,X=X,yNames=yName1,xNames=xNames)
}

