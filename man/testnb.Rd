\name{testnb}
\alias{testnb}

\title{Test a neighbours list for symmetry}
\description{
Checks a neighbours list for symmetry/transitivity (if i is a neighbour of j,
then j is a neighbour of i). This holds for distance and contiguity based
neighbours, but not for k-nearest neighbours.
}
\usage{
testnb(nb)
}

\arguments{
  \item{nb}{an object of class \code{nb} with a list of integer vectors containing neighbour region number ids.}
}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{read.gal}}}

\examples{
data(columbus)
testnb(col.gal.nb)
k4 <- knn2nb(knearneigh(coords, k=4), row.names=rownames(columbus))
testnb(k4)
}
\keyword{spatial}
