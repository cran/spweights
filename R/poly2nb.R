# Copyright 2001 by Roger Bivand
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


poly2nb <- function(pl, bb, brute.search=FALSE, row.names=NULL) {
	if (all(class(pl) != "polylist")) stop("Not a polygon list")
	if ("multiparts" %in% class(pl)) multiparts <- TRUE
	else  multiparts <- FALSE
	n <- length(pl)
	regid <- attr(pl, "region.id")
	if (is.null(regid)) {
		if(is.null(row.names)) regid <- as.character(1:n)
		else {
			if(length(row.names) != n)
				stop("row.names wrong length")
			else regid <- row.names
		}
	}
	if (nrow(bb) != n)
		stop("Number of polygons not equal to number of bounding boxes")

	between <- function(x, low, up) {return(x >= low && x <= up)}

	pipbb <- function(pt, bb) {
	return(between(pt[1], bb[1], bb[3]) && between(pt[2], bb[2], bb[4]))}

	ptonpoly <- function(pt, poly) {
	return(pt[1] %in% poly[,1] && pt[2] %in% poly[,2])}

	cand <- vector(mode="list", length=n)
	if (brute.search) {
	    allregs <- 1:n
	    for (i in 1:n) cand[[i]] <- allregs
	} else {
	    for (i in 1:n) {
		bbi <- bb[i,]
		x <- rep(bbi[1], 5)
		x <- c(x, seq(bbi[1], bbi[3], length=5))
		x <- c(x, rep(bbi[3], 5))
		x <- c(x, seq(bbi[3], bbi[1], length=5))
		y <- seq(bbi[2], bbi[4], length=5)
		y <- c(y, rep(bbi[4], 5))
		y <- c(y, seq(bbi[4], bbi[2], length=5))
		y <- c(y, rep(bbi[2], 5))
		ptsi <- cbind(x, y)
		jhits <- NULL
		for (j in (i+1):n) {
			if (j > n) break
			bbj <- bb[j,]	
			for (k in 1:nrow(ptsi)) {
				res <- pipbb(ptsi[k,], bbj)
				if (res) {
					jhits <- c(jhits, j)
					break
				}
			}
		}
		if (length(jhits) > 0) {
			cand[[i]] <- sort(unique(c(cand[[i]], jhits)))
			for (j in 1:length(jhits))
				cand[[jhits[j]]] <-
				    sort(unique(c(cand[[jhits[j]]], i)))
		}
	    }
	    class(cand) <- "nb"
	    cands <- nblag(cand, 2)
	    cand1 <- union.nb(cands[[1]], cands[[2]])
	}
	ans <- vector(mode="list", length=n)
	if (multiparts) {
		nparts <- integer(n)
		for (i in 1:n) nparts[i] <- length(pl[[i]])
	}
	for (i in 1:n) {
		leni <- length(cand[[i]])
		if (leni > 0) {
			candi <- cand1[[i]]
			jhits <- NULL
			if (multiparts) {
				nrpl <- integer(nparts[i])
				for (j in 1:nparts[i])
					nrpl[j] <- nrow(pl[[i]][[j]])
			} else nrpl <- nrow(pl[[i]])
			for (j in 1:length(candi)) {
				ji <- candi[j]
				if (multiparts) {
				    for (j1 in 1:nparts[i]) {
					for (k in 1:nrpl[j1]) {
					    for (j2 in 1:nparts[ji]) {
					        res <- ptonpoly(pl[[i]]
						    [[j1]][k,], pl[[ji]][[j2]])
					        if (res) {
						    jhits <- c(jhits, ji)
						    break
					        }
					    }
					}
				    }
				} else {
				    for (k in 1:nrpl) {
					res <- ptonpoly(pl[[i]][k,], pl[[ji]])
					if (res) {
						jhits <- c(jhits, ji)
						break
					}
				    }
				}
			}
			if (length(jhits) > 0) {
				ans[[i]] <- sort(unique(c(ans[[i]], jhits)))
				for (j in 1:length(jhits))
					ans[[jhits[j]]] <-
					    sort(unique(c(ans[[jhits[j]]], i)))
			}
		} else error(paste("No candidate neighbours for", i,
		 "\ntry brute.search=TRUE"))
	}
	class(ans) <- "nb"
	attr(ans, "region.id") <- regid
	attr(ans, "call") <- match.call()
	attr(ans, "queen") <- TRUE
	invisible(ans)
}

plotpolys <- function(pl, bb, col=NA, border=par("fg"), add=FALSE) {
	if (all(class(pl) != "polylist")) stop("Not a polygon list")
	if (!add) {
		xlim <- c(min(bb[,1]), max(bb[,3]))
		ylim <- c(min(bb[,2]), max(bb[,4]))
		plot(x=bb[,1], y=bb[,4], xlim=xlim, ylim=ylim, type="n",
		asp=1, xlab="", ylab="")
	}
	if (length(col) != length(pl)) {
		col <- rep(col, length(pl), length(pl))
	}
	for (j in 1:length(pl)) {
		if ("multiparts" %in% class(pl)) {
			for (k in 1:length(pl[[j]]))
				polygon(pl[[j]][[k]], col=col[j], border=border)
		} else {
			polygon(pl[[j]], col=col[j], border=border)
		}
	}
}
	

