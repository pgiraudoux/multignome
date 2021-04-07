"combineFrames" <-
function(model,data){
  # A real work horse. this creates the Gnome objects, collections of data and omitted parameter infomration.
  if(class(model)!="list") stop("model must be of class list")
  myList <- lapply(model, extractXY,data=data)
  if(dim(myList[[1]]$X)[2]!=0) stop("First formula in list 'model' must be of form y0~0.")
  nEta <- length(myList)
  Xcmbd <- {}
  Ycmbd <- {}
  Xterms <- list()
  for (i in 1:nEta) {
    newXcols <- !is.element(colnames(myList[[i]]$X), colnames(Xcmbd))
    newX <- as.matrix(myList[[i]]$X[,newXcols])
    colnames(newX) <-  myList[[i]]$xNames[newXcols]
    Xcmbd <- cbind(Xcmbd, newX)  # combined X matrix   
    newY <- matrix(myList[[i]]$Y,ncol=1)        
    colnames(newY) <- as.character(myList[[i]]$yNames)
    Ycmbd <- cbind(Ycmbd, newY)                      # combined Y matrix
    Xterms[[i]] <- myList[[i]]$xNames
  }
  Xterms <- Xterms[-1]
  pNames <- parNames(Xcmbd,Xterms)               # the parameter matrix
  aList <- list(X=Xcmbd, Y=Ycmbd, B=pNames)
  attr(aList,"class") <- "gnomeComponents"
  aList
}

