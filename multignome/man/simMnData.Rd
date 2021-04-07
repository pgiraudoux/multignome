\name{simMnData}
\alias{simMnData}
\alias{simMultinom}

\title{Simulate multinomial data.}
\description{
Simulates multinomial data.
}
\usage{
simMnData(nSp, nO, nEx, size)
simMultinom(probV, n, size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nSp}{Number of species.}
  \item{nO}{Number of observations. eg. number of trapnights.}
  \item{nEx}{Number of explanatory variables X in addition to an
    intercept which is included by default. Currently no optin to remove
  that intercep.}
  \item{size}{How many traps per trapline.}
  \item{probV}{A vector of probabilities to be coerced to a matrix for simulation.}
  \item{n}{How many observations. Similar to nO.}
}
\details{
Use simMnData to obtain a list with responses, explanatory variables and parameters.
}
%\value{
%  ~Describe the value returned
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
\seealso{rmultinom}
%\examples{}
\keyword{distribution}
