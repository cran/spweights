\name{summary.nb}
\alias{summary.nb}
\title{Summary function for neighbours lists}
\description{
The function prints summary measures for links in a neighbours list. If a
matrix of coordinates is given as well, summary descriptive measures for
the link lengths are also printed.
}
\usage{
summary.nb(nb, coords=NULL)
}

\arguments{
  \item{nb}{an object of class \code{nb}}
  \item{coords}{matrix of region point coordinates}
}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{plot.nb}}}

\examples{
data(columbus)
summary(col.gal.nb, coords)
}
\keyword{spatial}
