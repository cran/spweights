# Copyright 2000-1 by Roger S. Bivand. 
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#

dnearneigh <- function(x, d1, d2, row.names=NULL) {
    if (!is.matrix(x)) stop("Data not in matrix form")
    if (any(is.na(x))) stop("Data include NAs")
    np <- nrow(x)
    if (!is.null(row.names)) if(length(row.names) != np)
        stop("row.names wrong length")
    if (is.null(row.names)) row.names <- as.character(1:np)
    dimension <- ncol(x)
    md <- 0
    for (i in 1:dimension) md <- sum(md, (diff(range(x[,i]))^2))
    if (d1 < 0) d1 <- 0.0
    if (d2 > sqrt(md)) d2 <- sqrt(md)
    z <- .Call("dnearneigh", as.double(d1), as.double(d2), as.integer(np),
        as.integer(dimension), as.matrix(x))
    attr(z[[1]], "region.id") <- row.names
    attr(z[[1]], "call") <- match.call()
    attr(z[[1]], "dnn") <- c(d1, d2)
    z[[1]] <- sym.attr.nb(z[[1]])
    invisible(z[[1]])
}
