knn2nb <- function(knn, row.names=NULL) {
	if (class(knn) != "knn") stop("Not a knn object")
	res <- vector(mode="list", length=knn$np)
    	if (!is.null(row.names)) if(length(row.names) != knn$np)
        	stop("row.names wrong length")
    	if (is.null(row.names)) row.names <- as.character(1:knn$np)
	for (i in 1:knn$np) res[[i]] <- sort(knn$nn[i,])
 	attr(res, "region.id") <- row.names
 	attr(res, "knn-call") <- attr(knn, "call")
 	attr(res, "knn-k") <- knn$k
	class(res) <- "nb"
	invisible(res)
}
