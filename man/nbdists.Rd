% Copyright 2001 by Roger S. Bivand
\name{nbdists}
\alias{nbdists}
\title{Spatial link distance measures}
\description{
  Given a list of spatial neighbour links (a neighbours list of object type
  \code{nb}), the function returns the Euclidean distances along the links
  in a list of the same form as the neighbours list.
}
\usage{
nbdists(nb, coords)
}
\arguments{
  \item{nb}{an object of class \code{nb}}
  \item{coords}{matrix of point coordinates}
}
\value{
A list with class \code{nbdist}
}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{summary.nb}}, \code{\link{nb2listw}}}

\examples{
data(columbus)
dlist <- nbdists(col.gal.nb, coords)
dlist <- lapply(dlist, function(x) 1/x)
stem(unlist(dlist))
}
\keyword{spatial}
