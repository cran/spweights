write.nb.gal <- function(nb, file) {
	if(class(nb) != "nb") stop("not an nb object")
	n <- length(nb)
	con <- file(file, open="w")
	open(con, open="w")
	writeLines(paste(n), con)
	for (i in 1:n) {
		writeLines(paste(i, length(nb[[i]]), collapse=" "), con)
		writeLines(paste(nb[[i]], collapse=" "), con)
	}
	close(con)
}

testnb <- function(nb)
{
	if(class(nb) != "nb")stop("Not neighbours list")
	n <- length(nb)
	fstop <- 0
	for (i in 1:n) {
		icard <- length(nb[[i]])
		for (j in 1:icard) {
			flag <- 0
			k <- nb[[i]][j]
			flag <- length(which(i %in% nb[[k]]))
			if (flag != 1) {
				fstop <- fstop + 1
				cat("Non matching contiguities:", i,
					"and", k, "\n")
			}
		}
	}
	if(fstop != 0) cat("Non-symmetric neighbours list\n")
}

include.self <- function(nb) {
	if (!is.null(attributes(nb)$self.included) &&
		(as.logical(attributes(nb)$self.included)))
		stop("Self already included")
	n <- length(nb)
	for (i in 1:n) nb[[i]] <- sort(c(i, nb[[i]]))
	attr(nb, "self.included") <- TRUE
	invisible(nb)
}

gabriel2nb <- function(gab, row.names=NULL) {
	if (class(gab) != "Gabriel") stop("not an object from gabrielneigh")
	res <- vector(mode="list", length=gab$np)
	for (i in 1:gab$nedges) {
		res[[gab$from[i]]] <- c(res[[gab$from[i]]],
			gab$to[i])
		res[[gab$to[i]]] <- c(res[[gab$to[i]]],
			gab$from[i])
	}
	for (i in 1:gab$np) res[[i]] <- sort(unique(res[[i]]))
    	if (is.null(row.names)) row.names <- as.character(1:gab$np)
 	attr(res, "region.id") <- row.names
	attr(res, "Gabriel") <- attr(gab, "call")
	class(res) <- "nb"
	invisible(res)
}

relative2nb <- function(rel, row.names=NULL) {
	if (class(rel) != "relative") stop("not an object from relativeneigh")
	res <- vector(mode="list", length=rel$np)
	for (i in 1:rel$nedges) {
		res[[rel$from[i]]] <- c(res[[rel$from[i]]],
			rel$to[i])
		res[[rel$to[i]]] <- c(res[[rel$to[i]]],
			rel$from[i])
	}
	for (i in 1:rel$np) res[[i]] <- sort(unique(res[[i]]))
    	if (is.null(row.names)) row.names <- as.character(1:rel$np)
 	attr(res, "region.id") <- row.names
	attr(res, "relative") <- attr(rel, "call")
	class(res) <- "nb"
	invisible(res)
}
