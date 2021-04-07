\name{totLLmn}
\alias{totLLmn}
\alias{totLLmn.givenGnome}
\alias{totLLmn.givenParam}
\alias{LLmn}

\title{Calculate Log-Likelihood of Multinomial Model}
\description{Calculate Log-Likelihood of Multinomial Model}
\usage{
totLLmn(probM, mnData)
totLLmn.givenParam(paraM, mnData, expData)
totLLmn.givenGnome(paraV, aGnome)
LLmn(probDataV)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{probM}{A matrix of fitted probabilities derived from multinomial
  model. Will have same dimension as matrix Y of observations.}
  \item{mnData}{Observations Y, the null response is assumed to be the
    first column.}
  \item{probDataV}{For internal use only. A composite vector of fitted
    probabilities and related observations Y for some row i of probM and
  mnData.}
  \item{paraM}{A matrix of model parameters.}
  \item{expData}{Explanatroy variables X, the model frame.}
  \item{paraV}{A vector derived directly from paraM.}
  \item{aGnome}{A Gnome object is a list consisting of matrices Y, X and
  parameter matrix B. The attributes of B detail which parameters are to
be fixed at zero throughout optimisation.}

}
\details{These funcitons are called internally and are of little
  interest to ecologists and hunters of Gnomes.
}
%\value{
% ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
%\references{ ~put references to the literature/web site here ~ }
\author{David Pleydell}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{multinom, multinomRob}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function(probDataV) {
%  # assumes the declared vector is a row from cbind(probM,mnData) with corresponding columns
%  n <- length(probDataV)/2
%  dmultinom(probDataV[n+1:n],prob=probDataV[1:n],log=T)
%  }
%}
\keyword{regression}% at least one, from doc/KEYWORDS

