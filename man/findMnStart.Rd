\name{findMnStart}
\alias{findMnStart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{search for non-naive starting values}
\description{
Fits binomial GLMs to each response to obtain starting values which are
not so wild as say everything zero. Currently not included in multignome
because specifying gr in optim appears to be sufficient for good performance.
}
\usage{
findMnStart(mnData, expData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mnData}{Y repsonse matrix.}
  \item{expData}{X model data. Explanatory variables.}
}
\details{
The first colomn of Y is assumed to be the null response. eg. empty
traps or no holes.
}
%\value{
%  ~Describe the value returned
%  If it is a LIST, use
%  \item{comp1 }{Description of 'comp1'}
%  \item{comp2 }{Description of 'comp2'}
%  ...
%}
%\references{ ~put references to the literature/web site here ~ }
\author{David pleydell}
%\note{ ~~further notes~~ %
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# No Examples
}
\keyword{regression}

