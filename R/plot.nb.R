plot.nb <- function(nb, coords, col="black", add=FALSE) {
	x <- coords[,1]
	y <- coords[,2]
	n <- length(nb)
	xlim <- range(x)
	ylim <- range(y)
	if (!add) {
		plot.new()
        	plot.window(xlim = xlim, ylim = ylim, "", asp=1)
	}
	for (i in 1:n) {
        	inb <- nb[[i]]
        	for (j in inb)
			lines(c(x[i], x[j]), c(y[i], y[j]),
				col=col)
	}
	points(x, y)
}
