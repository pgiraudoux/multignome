"totLLmn" <-
function(probM,mnData){
  if (ncol(probM)!=ncol(mnData)) stop("ncol of probability and data matrices must same")
  if (nrow(probM)!=nrow(mnData)) stop("nrow of probability and data matrices must same")
  Mat <- cbind(probM,mnData)
  sum(apply(Mat,1,LLmn))
}

