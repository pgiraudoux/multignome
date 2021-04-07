\name{serxu}
\docType{data}
\alias{serxu}
\title{Serxu Trapping Data}
\description{
  This data set was collected on the Tibetan plateau in 2001 and 2002 in
  Serxu County, Sichuan, PRC.
}
\usage{serxu}
\format{
  A data frame with 186 rows. Each row repressents one trap night, that
  is 25 traps left in a given location for one night. This is repeated
  three times at each location. The colomns of serxu.
  \itemize{
    \item{empty}{Number of empty traps.}
    \item{Mili}{Number of traps with Microtus limnophilus.}
    \item{Miir}{Number of traps with Microtus irene.}
    \item{Mile}{Number of traps with Microtus leurcus.}
    \item{Crka}{Number of traps with Cricetulus kansus.}
    \item{night}{Factor: 1,2 or 3.}
    \item{habitat}{Factor: A code for habitat class. See details below.}
    \item{ID line}{An ID code usufl for identifying repetitions at each location.}
    }
}
\source{
  Raoul F. et al. Distribution of small mammals in a pastoral
  landscape of the Tibetan plateaus (Western Sichuan, China) and
  relationship with grazing practices. Mammalia. (Submitted).
}

\details{
  Colomns are as follows.
  Key for habitat abreviations.
    \describe{
      \item{bs}{Bare ground slope}      
      \item{bv}{Bare ground valley}    
      \item{cg}{Carex grassland}      
      \item{dg}{Drier grassland}       
      \item{fg}{Fenced grassland}      
      \item{mt}{microtopography}       
      \item{mg}{Mixed grassland}        
      \item{pt}{Potentilla mountain top}
      \item{pv}{Potentilla valley}      
      \item{rs}{Rocky slopes}          
      \item{sx}{Salix bushes}           
     \item{sg}{Stellera grassland}    
      \item{sb}{Stream bank}
      \item{ug}{Upper grassland}
    }
  To do: add spatial coordinates.
}

\references{
  Raoul F. et al. Distribution of small mammals in a pastoral
  landscape of the Tibetan plateaus (Western Sichuan, China) and
  relationship with grazing practices. Mammalia. (Submitted).
}

\keyword{datasets}
