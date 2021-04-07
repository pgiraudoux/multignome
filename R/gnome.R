"gnome" <-
function(aGnome){
  # for optimisation of a Gnome object created by combineFrames
  # a Gnome object is a collection of data and omitted parameters information for fitting multinomial models at the species level.
  X <- aGnome$X
  Y <- aGnome$Y
  B <- aGnome$B
  if (missing(X)) stop("Bad Bad Gnome")
  if (missing(Y)) stop("Bad Bad Gnome")
  if (missing(B)) stop("Bad Bad Gnome")
  if (sum(names(attributes(B))==c("dim","pV","redpV","J","K"))!=5) stop("attributes mismatch")
  # Set initial values
  # Other strategies can be added later.
  # e.g. approximation with binomial glm
  iniP <- rep(0,length(attr(B,"redpV")))
  # Optimise
  outputGnome <- optim(iniP, totLLmn.givenGnome, gr=grad.gnome,
                       method="BFGS",aGnome=aGnome,
                       control=list(fnscale = -1,reltol=1e-15))
  #  print(outputGnome$convergence)
  attr(outputGnome,"class") <- "outputGnome"
  outputGnome$AIC <- -2*outputGnome$value + 2*length(outputGnome$par)
  outputGnome$pars
  paraM <- rep(0,  prod(dim(B)))
  paraM[is.element(attr(B,"pV"),attr(B,"redpV"))] <- outputGnome$par
  paraM <- matrix(paraM,nrow=nrow(B))
  outputGnome$params <- paraM
  outputGnome
}

