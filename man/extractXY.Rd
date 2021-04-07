\name{extractXY}
\alias{extractXY}
\alias{parNames}
\alias{grad.givenIndex}
\alias{grad.gnome}

\title{Internal Functions For Fitting Multinomial Models}
\description{
  Internal Functions for constructing model frame, obtainning indexes
  for parameters and providing gradients for optimisation. In normal use
  the user does not need to call these functions.
}
\usage{
extractXY(formula, data)
parNames(Xcmbd, termsList)
grad.givenIndex(index, aGnome, cProbs)
grad.gnome(paraV, aGnome)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A standard model formula as specified for other fors of
  GLM.}
  \item{data}{A data frame containing data for the regression equation.}
  \item{Xcmbd}{A composite X matrix built one species at a time. eg. All
  explanatory variables for the second species are cbind to those of the
first, etc etc.}
  \item{termsList}{I forget, but not important for the user. }
  \item{index}{An index.forget it.}
  \item{aGnome}{Gnomes are described in ?combineFrames}
  \item{cProbs}{The matrix of current probabilities defined by the model
  parameters.}
  \item{paraV}{Vector of parameters }
}
\details{
  See multignome, gnome and combineFrames.
}
%\value{
%  \item{comp1 }{Description of 'comp1'}
%}
%\references{ ~put references to the literature/web site here ~ }
\author{David Pleydell}
\note{
Brian Ripley's multinom still outperforms optimisation at the second or
third decimal place. perhaps there is a tolerance setting somewhere to
tighten up.
}
\seealso{multinom, multinomRob}
\examples{
# See multiGnome
}
\keyword{regression}

