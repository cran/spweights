\name{edit.neighbours}
\alias{edit.neighbours}
\title{Interactive editing of neighbours lists}
\description{
The function provides simple interactive editing of neighbours lists to
allow unneeded links to be deleted. It uses \code{identify} to pick the
endpoints of the link to be deleted, and asks for confirmation before
committing. If the result is not assigned to a new object, the editing
will be lost - as in \code{edit}.
}
\usage{
edit.neighbours(nb, coords, polys=NULL, bbs=NULL)
}

\arguments{
  \item{nb}{an object of class \code{nb}}
  \item{coords}{matrix of region point coordinates}
  \item{polys}{if polygon boundaries supplied, will be used as background}
  \item{bbs}{bounding boxes for polygons}
}
\value{
The function returns an object of class \code{nb} with the edited list of integer vectors containing neighbour region number ids, with an added attribute tallying the deleted links.
}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{summary.nb}}, \code{\link{plot.nb}}}

\keyword{spatial}
