\name{runMerges}
\alias{runMerges}


\title{Merge classes of explanatory factor of a multinomial model}
\description{Groups iteratively all classes pairwise of the explanatory factor of a multinomial model. Grouping that provides improvement of multinomial model AIC is kept and define one super class of the factor. In the context of Species Distribution Modelling, the final super-classes correspond to species/habitat assemblages.}
\usage{
runMerges(formula, data, fit)
}

\arguments{
  \item{formula}{Multinomial model formula}
  \item{data}{Data frame of species responses (presence/absence) and explanatory environmental variables including the merged factor.}
  \item{fit}{Multinomial model fit including the explanatory factor required to run the classes merging procedure}
  
}
\details{While running, the function provides the difference in AIC obtained between the model with a given grouping of 2 classes of habitat and the full model as well as the corresponding name of the merge done if this difference was larger than 2. When the grouping did not improve the AIC, 'no more merge possible' is returned for the corresponding grouping.}
  

\value{
A list including:
\item{Nmerges}{the number of merges}
\item{datanew}{the new data frame}
\item{new_model}{the new multinomial model fit}
\item{last_merge}{the last merge done}
\item{Table}{a table of the number of traplines per original and new habitat classes}
\item{list_merges}{a list of all merge outputs (cf mergeLevels function)}
\item{list_AIC}{list of all multinomial model fits}

}


\references{Vaniscotte A, Pleydell DR, Quere JP, Wang Q, Li T, Bernard N, Coeurdassier M, Delattre P, Takahashi K, Weidmann JC, Giraudoux P. 2009. Modelling and spatial discrimination of small mammal assemblages: an example from western Sichuan (China). Ecological Modelling, 220 (9-10), 1218-1231.}

\examples{
library(nnet)
data(metalSm)
formula <- cbind(Apsy,Mygl,Crle,Crru,Miag,Miar,Mimi,Mumu,Misu,Soar,Somi,empty) ~ habitat 
fit <- multinom(cbind(Apsy,Mygl,Crle,Crru,Miag,Miar,Mimi,Mumu,Misu,Soar,Somi,empty)~habitat,data=metalSm)
try <- runMerges(formula,data=metalSm,fit=fit)
}
