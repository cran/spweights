nblag <- function(neighbours, maxlag, sptimes=1)
{
	if (!is.null(attributes(neighbours)$self.included) &&
		(as.logical(attributes(neighbours)$self.included)))
		stop("No lags for neighbours lists including self")
	n <- length(neighbours)
	card <- .Call("card", neighbours)
	lags <- .Call("nblagC", as.integer(n), as.integer(maxlag), card,
		neighbours, as.integer(sptimes));
	for (i in 1:maxlag) {
		class(lags[[i]]) <- "nb"
		attr(lags[[i]], "region.id") <- attr(neighbours, "region.id")
	}
	attr(lags, "call") <- match.call()
	invisible(lags)
}

