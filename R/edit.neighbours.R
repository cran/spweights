edit.neighbours <- function(nb, coords, polys=NULL, bbs=NULL) {
	if (class(nb) != "nb") stop("Not a neighbours list")
	x <- coords[,1]
	y <- coords[,2]
	n <- length(nb)
	row.names <- attr(nb, "region.id")
	if (is.null(row.names)) row.names <- as.character(1:n)
	xlim <- range(x)
	ylim <- range(y)
	plot.new()
        plot.window(xlim = xlim, ylim = ylim, "", asp=1)
	if (!is.null(polys) && !is.null(bbs))
		plotpolys(polys,bbs, border="grey", add=T)
	for (i in 1:n) {
        	inb <- nb[[i]]
        	for (j in inb)
			lines(c(x[i], x[j]), c(y[i], y[j]),
				col="black")
	}
	points(x, y)
	finished <- "n"
	deletions <- NULL
	while (finished == "n") {
		cat("Identifying contiguity for deletion ...\n")
		cand <- identify(x, y, n=2)
		bringToTop(-1)
		if ((cand[2] %in% nb[[cand[1]]]) && (cand[1] %in% nb[[cand[2]]])) {
			lines(x[cand], y[cand], col="red")
			delete <- readline("Delete this line (y/n) ")
			if (delete != "y") delete <- "n"
			else {
				deletions <- c(deletions, paste(cand,
					collapse="-"))
				nb[[cand[1]]] <- nb[[cand[1]]][nb[[cand[1]]] != cand[2]]
				if(length(nb[[cand[1]]]) == 0) {
					nb[[cand[1]]] <- 0
					cat(cand[1], "is now an island\n")
				}
				nb[[cand[2]]] <- nb[[cand[2]]][nb[[cand[2]]] != cand[1]]
				if(length(nb[[cand[2]]]) == 0) {
					nb[[cand[2]]] <- 0
					cat(cand[2], "is now an island\n")
				}
 			}
			cat("deleted contiguity between point", cand[1],
				"and", cand[2], "\n")
			plot.new()
	        	plot.window(xlim = xlim, ylim = ylim, "", asp=1)
			if (!is.null(polys) && !is.null(bbs))
				plotpolys(polys,bbs, border="grey", add=T)
			for (i in 1:n) {
	        		inb <- nb[[i]]
	        		for (j in inb)
					lines(c(x[i], x[j]), c(y[i], y[j]),
						col="black")
			}
			points(x, y)
		} else {
			cat("No contiguity between chosen points\n")
		}
		finished <- readline("Finished yet? (y/n) ")
		if (finished != "y") finished <- "n"
	}
	attributes(nb) <- list(deleted=deletions)
	attr(nb, "region.id") <- row.names
	class(nb) <- "nb"
	invisible(nb)
}
