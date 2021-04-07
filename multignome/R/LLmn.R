"LLmn" <-
function(probDataV) {
  # assumes the declared vector is a row from cbind(probM,mnData) with corresponding columns
  n <- length(probDataV)/2
  dmultinom(probDataV[n+1:n],prob=probDataV[1:n],log=TRUE)
}

