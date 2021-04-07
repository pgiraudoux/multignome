\name{multignome}
\alias{multignome}
\alias{combineFrames}
\alias{gnome}

\title{The Gnome}
\description{
  Functions for maximum likelihood fitting of multinomial models in
which each response level i.e. species, can possess a uniquely defined
regression expression.
}
\usage{
multignome(model, data)
combineFrames(model, data)
gnome(aGnome)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{A list object containing formulas. The first formula must
  be of the form y0~0, other formulas are as for othe glm models.}
  \item{data}{Data frame containing colomns called from model.}
  \item{aGnome}{Object returned by combineFrames.}
}
\details{
The user calls multignome. This function then calls combineFrames which
prepares the data for optimisation ie.create the Gnome
object. Optimisation is then perfomred by passing theGnome object to the
function gnome. The output shows the optimised parameters, the
maximised log likelihood and the AIC.
}
%\value{
%}
%\references{ ~put references to the literature/web site here ~ }
\author{David Pleydell}

\references{

\href{http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2702787/}{Vaniscotte A., Pleydell D., Raoul F., Quere J.P., Coeurdassier M., Delattre P., Li T., Qian W., Takahashi K., Weidmann J.C., Qiu J., Giraudoux P. 2009 Modelling and spatial discrimination of small mammal assemblages: an example from western Sichuan (China). Ecological Modelling, 220: 1218-1231.}

Raoul F, Pleydell D, Quere JP, Vaniscotte A, Rieffel R, Takahashi K, Bernard N, Wang J, Dobigny T, Galbreath KE, Giraudoux P 2008 Small-mammal assemblage response to deforestation and afforestation in central China. Mammalia 72 : 320-332
}


\note{
  Anyone want to write a print method?
}
\seealso{multinom, multinomRob}

\examples{
# A function to derive unscaled AIC from fitted values of multinom
sumRowDs <- function(dataProb) {
  n <- length(dataProb) /2   # half vector length
  dmultinom(dataProb[1:n],prob=dataProb[n+1:n],log=TRUE)
}
 
library(nnet)
# Simulate Data
nSpecies <- 2
nTrapNights <- 50
nExplain <- 2
nTrapsPerLine <- 25
simData <- simMnData(nSpecies,nTrapNights,nExplain,nTrapsPerLine)
sim <- as.data.frame(cbind(simData,attr(simData,"dataMat")))
colnames(sim) <- c(paste("y",0:nSpecies,sep=""), paste("x",0:nExplain,sep=""))

# Fit model using multinom in nnet library
ripMN <- multinom(cbind(y0,y1,y2)~x1+x2, sim)
# Fir model using gnome
modelList <- list(y0~0,y1~x1+x2,y2~x1+x2)
myMG <- multignome(modelList,data=sim)

# Ripley's  coefficients
t(coefficients(ripMN))
# Our coefficients 
myMG$params
# True values of coefficients
attr(simData,"pars")

# Ripley's log likelihood
sum(apply(cbind(sim[,1+(0:nSpecies)],ripMN$fitted),1,"sumRowDs"))
# Our log likelihood
myMG$value
}
\keyword{regression}% at least one, from doc/KEYWORDS

