\name{graphneigh}
\alias{gabrielneigh}
\alias{relativeneigh}
\alias{plot.Gabriel}
\alias{plot.relative}
\alias{gabriel2nb}
\alias{relative2nb}
\title{Graph based spatial weights}
\description{
Functions return a graph object containing a list with the vertex
coordinates and the to and from indices defining the edges. Helper
functions \code{gabriel2nb} and \code{relative2nb} convert graph
objects into neighbours lists. The plot functions plot the graph objects.
}
\usage{
gabrielneigh(x)
relativeneigh(x)
gabriel2nb(gab, row.names=NULL)
relative2nb(rel, row.names=NULL)
plot.Gabriel(gab, show.points=FALSE, add=FALSE, linecol=par(col), ...)
plot.relative(rel, show.points=FALSE, add=FALSE, linecol=par(col),...)
}

\arguments{
  \item{x}{matrix of region point coordinates}
  \item{gab}{a Gabriel graph object}
  \item{rel}{a relative graph object}
  \item{row.names}{character vector of region ids to be added to the neighbours list as attribute \code{region.id}, default \code{seq(1, nrow(x))}}
  \item{show.points}{(logical) add points to plot}
  \item{add}{(logical) add to existing plot}
  \item{linecol}{edge plotting colour}
  \item{...}{further graphical parameters as in \code{par(..)}}
}
\details{
The two algorithms calculate the 
}
\value{
A list of class \code{Gabriel} or class \code{relative}
  \item{np}{number of input points}
  \item{from}{array of origin ids}
  \item{to}{array of destination ids}
  \item{nedges}{number of edges in graph}
  \item{x}{input x coordinates}
  \item{y}{input y coordinates}
The helper functions return an \code{nb} object with a list of integer
 vectors containing neighbour region number ids.
}

\references{Matula, D. W. and Sokal R. R. 1980, Properties of Gabriel
  graphs relevant to geographic variation research and the clustering of
  points in the plane, Geographic Analysis, 12(3), pp. 205-222.
  Toussaint, G. T. 1980, The relative neighborhood graph of a finite
  planar set, Pattern Recognition, 12(4), pp. 261-268.}

\author{Nicholas Lewin-Koh \email{kohnicho@comp.nus.edu.sg}}

\seealso{\code{\link{knearneigh}}, \code{\link{dnearneigh}},
\code{\link{knn2nb}}}

\examples{
data(columbus)
col.gab <- gabrielneigh(coords)
col.rel <- relativeneigh(coords)
plotpolys(polys, bbs, border="grey")
plot(col.gab, add=TRUE, linecol=2)
plot(col.rel, add=TRUE, linecol=4)
title(main="")
plotpolys(polys, bbs, border="grey")
plot(gabriel2nb(col.gab), coords, add=TRUE, col="red", points=F)
plot(relative2nb(col.rel), coords, add=TRUE, col="blue", points=F)
}
\keyword{spatial}
