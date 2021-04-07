\name{etaV2prob}
\alias{etaV2prob}
\alias{real2data}

\title{Link Functions}
\description{
  etaV2prob provides the classic log-linear link function.

  real2data is the link function to use when working with Ochotona
  trnasect data.
}
\usage{
etaV2prob(etaV)
real2data(p1, p2, idp1, idp2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{etaV}{A vector of linear predictors for a given trapnight.}
  \item{p1}{Probability that species 1 is pressent with holes.}
  \item{p2}{Same for species two.}
  \item{idp1}{The ID proportion, species 1. The proportion of species 1 holes with
identifying features ie.shits or sightings.}
  \item{idp2}{The ID proportion, species 2. } 
}
\details{
Using etaV2prob and eauivelent model formulae for each response gives
estimates equivelent to Brian Ripley's multinom function in nnet
library.

For Ochotona the relation between reality on the ground and data
recorded on transect forms is handled by real2data. For each species
the probabillity of no-holes gives an expected number of holes. A
proportion of this expectation gives the probability of identifiable
holes and the probability of non-identifiable holes. A total of 16
combinations of N, U, I, and the intersection of UI provide the
probabilities of the five mutully exclusive responses one the transect
form Nothing, Hole, Sp1, Sp2, Both.
}
\value{
Returns vectors of probabilities which are used with the observed data
to obtain the log-likelihood. Parameters giving these probabilities are
then tuned such that the likelihood is maximised. The ID probabilities
are unknown and so included in that optimisation.
}
%\references{ ~put references to the literature/web site here ~ }
\author{David pleydell}
\note{
How to tune optim to get that second decimal place advantage in multinom?
}
\seealso{multinom, multinomRob}
\examples{
# coming soon
}
\keyword{regression}

