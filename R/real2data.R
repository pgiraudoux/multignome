"real2data" <-
function(p1,p2,idp1,idp2){
  # This function converts non-mutually exclussive probabilities of unknown "ground truth" to
  # mutually exclussive probabilities associated with recorded data on the transect forms.
  # Current implimentation is designed for two species only. eg. O.curzoniae and O.cansus
  #
  # p1 probability of pressence of species 1
  # idp1 proportion of holes of species 1 with visible faeces
  # Transforms p(S|x) to p(D|x), ie. non-M.E. probs of pressence per species to M.E. probs for recorded data responses
  pN1 <- 1 - p1
  pN2 <- 1 - p2
  lam1 <- -log(pN1) # lambda, expected # of holes (identifiable and unidentifiable) of species 1
  lam2 <- -log(pN2) # 
  pI1 <- 1 - exp(-idp1*lam1)
  pU1 <- 1 - exp(-(1-idp1)*lam1)
  pI2 <- 1 - exp(-idp2*lam2)
  pU2 <- 1 - exp(-(1-idp2)*lam2)

  pN <- pN1 * pN2
  pU <- (pN1 + pU1) * (pN2 + pU2) - pN
  p1 <- (pI1 - pI1*pU1) * (pN2 + pU2)
  p2 <- (pN1 + pU1) * (pI2 - pI2*pU2)
  pB <- (pI1 - pI1*pU1) * (pI2 - pI2*pU2)

  c(pN,pU,p1,p2,pB)
}

