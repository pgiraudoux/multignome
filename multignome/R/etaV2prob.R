"etaV2prob" <-
function(etaV){
  if (etaV[1]!=0) stop("eta = 0 required for null-response at first position in vector")
  probs <- exp(etaV)/sum(exp(etaV))
  sina <- sum(is.na(probs))
  if (sina!=0) probs[is.na(probs)] <- 1/sina
  probs
}

