\name{metalSm}
\docType{data}
\alias{metalSm}
\title{Metaleurope trapping Data}
\description{
  This data set was collected in the Metaleurope area in Northern France (Noyelles-Godault, Nord - Pas-de-Calais)}
\usage{metalSm}
\format{
  A data frame with 288 rows. Each row represents one trapline, (trapline: line of 10 3m spaced traps), set for 3 nights. Traps were set in woody habitats in 30 squares (edge: 500 m) chosen according to pollution level in soil in 4 different landscape types.  

The columns of metalSm:
  \itemize{
\item{ID_line}{Trap line ID}
\item{nb_traps}{Number of traps for the line}
\item{landscape} {Landscape type (cf. below)} 
\item{habitat}{Habitat name}
\item{Apsy}{Apodemus sylvaticus}
\item{Mygl}{Myodes glareolus}
\item{Crle}{Crocidura leucodon}
\item{Crru}{Crocidura russula}
\item{Miag}{Microtus agrestis}
\item{Miar}{Microtus arvalis}
\item{Mimi}{Micromys minutus}
\item{Mumu}{Mus musculus}
\item{Misu}{Microtus subterraneus}
\item{Soar}{Sorex araneus}
\item{Somi}{Sorex minutus}
\item{empty}{Number of empty traps}
    }
}

\source{ANR/ADEM STARTT programme. Coordinator: Renaud Scheifler, renaud.scheifler@univ-fcomte.fr }


\details{
  Colomns are as follows.
  Key for landscape abreviations.
    \describe{
      \item{A}{Arable lands}      
      \item{U}{Urban areas}    
      \item{W}{Woodlands}      
      \item{S}{Shrublands}       
    }
}

\references{C. Fritsch, A. Vaniscotte, F. Raoul, M. Coeurdassier, P. Giraudoux, A. de Vaufleury and R. Scheifler. Metallic pollution affects small mammal assemblages: evidence from a large smelter impacted area. In prep.}

\keyword{datasets}
