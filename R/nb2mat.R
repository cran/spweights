nb2mat <- function(neighbours, glist=NULL, style="W")
{
	if(class(neighbours) != "nb") stop("Not a neighbours list")
	n <- length(neighbours)
	cardnb <- card(neighbours)
	if (is.null(glist)) {
		glist <- vector(mode="list", length=n)
		for (i in 1:n) glist[[i]] <- rep(1, length=cardnb[i])
	}
	if (length(glist) != n) stop("glist wrong length")
	if (cardnb != unlist(lapply(glist, length)))
		stop("neighbours and glist do not conform")
	if (any(is.na(unlist(glist))))
		stop ("NAs in general weights list")
	res <- matrix(0, nrow=n, ncol=n)
	if (style == "W") {
		d <- unlist(lapply(glist, sum))
		for (i in 1:n)
		    res[i, neighbours[[i]]] <- (1/d[i]) * glist[[i]]
	}
	if (style == "B")
		for (i in 1:n)
		   res[i, neighbours[[i]]] <- rep(1, cardnb[i])
	if (style == "C") {
		D <- sum(unlist(glist))
		if (is.na(D) || !(D > 0))
			stop(paste("Failure in sum of weights:", D))
		for (i in 1:n)
		    res[i, neighbours[[i]]] <- (n/D) * glist[[i]]
	}
	if (style == "S") {
		glist <- lapply(glist, function(x) x^2)
		q <- sqrt(unlist(lapply(glist, sum)))
		for (i in 1:n) glist[[i]] <- (1/q[i]) * glist[[i]]
		Q <- sum(unlist(glist))
		if (is.na(Q) || !(Q > 0))
		    stop(paste("Failure in sum of intermediate weights:", Q))
		for (i in 1:n)
		    res[i, neighbours[[i]]] <- (n/Q) * glist[[i]]
	}
	attr(res, "call") <- match.call()
	invisible(res)
}

invIrM <- function(neighbours, rho, glist=NULL, style="W") {
	if(class(neighbours) != "nb") stop("Not a neighbours list")
	n <- length(neighbours)
	V <- nb2mat(neighbours, glist, style)
	feasible <- 1/(range(eigen(V, only.values=TRUE)))
	if (rho <= feasible[1] || rho >= feasible[2])
		stop(paste("Rho outside feasible range:", feasible))
	mat <- diag(n) - rho * V
	res <- solve(mat)
	attr(res, "call") <- match.call()
	invisible(res)
}
