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
