\name{plot.nb}
\alias{plot.nb}
\title{Plot a neighbours list}
\description{
A function to plot a neighbours list given point coordinates to represent the region in two dimensions.
}
\usage{
plot.nb(nb, coords, col="black", add=FALSE)
}

\arguments{
  \item{nb}{an object of class \code{nb}}
  \item{coords}{matrix of region point coordinates}
  \item{col}{plotting colour}
  \item{add}{(logical) add to existing plot}
}
}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{summary.nb}}}

\examples{
data(columbus)
plot(col.gal.nb, coords)
title(main="GAL order 1 links with first nearest neighbours in red")
col.knn <- knearneigh(coords, k=1)
plot(knn2nb(col.knn), coords, add=T, col="red")
}
\keyword{spatial}
