nblag <- function(neighbours, maxlag)
{
	if (!is.null(attributes(neighbours)$self.included) &&
		(as.logical(attributes(neighbours)$self.included)))
		stop("No lags for neighbours lists including self")
	n <- length(neighbours)
	lags <- vector(mode="list", length=maxlag)
	lags[[1]] <- neighbours
	for (thislag in 2:maxlag)
		lags[[thislag]] <- vector(mode="list", length=n)
	for (i in 1:n) {
		already <- i
		new <- neighbours[[i]]
		for (thislag in 2:maxlag) {
			already <- c(already, new)
			active <- new
			new <- NULL
			for (j in active)
				new <- c(new, neighbours[[j]])
			new <- sort(unique(new))
			lags[[thislag]][[i]] <- new[-which(new %in% already)]
		}
	}
	for (i in 1:maxlag) {
		class(lags[[i]]) <- "nb"
		attr(lags[[i]], "region.id") <- attr(neighbours, "region.id")
	}
	attr(lags, "call") <- match.call()
	invisible(lags)
}
