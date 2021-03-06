# Copyright 2001 by Roger S. Bivand. 
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

knearneigh <- function(x, k=1)
{
    if (!is.numeric(x)) stop("Data non-numeric")
    if (!is.matrix(x)) stop("Data not in matrix form")
    if (any(is.na(x))) stop("Data include NAs")
    np <- nrow(x)
    dimension <- ncol(x)
    if (k >= np) stop("Fewer data points than k")
    xx <- x[,1]
    if (dimension > 1) for (i in 2:dimension) xx <- c(xx, x[,i])
    nn <- integer(np*k)
    dnn <- double(np*k)
    z <- .C("knearneigh", k=as.integer(k), np=as.integer(np),
        dimension=as.integer(dimension),
        xx=as.double(xx), nn=as.integer(nn), dnn=as.double(dnn))
    res <- list(nn=matrix(z$nn, np, k, byrow=T), np=np, k=k,
    	dimension=dimension, x=x)
    class(res) <- "knn"
    attr(res, "call") <- match.call()
    invisible(res)
}
