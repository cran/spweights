card <- function(nb) {
    if (class(nb) != "nb") stop("not a neighbours list")
    z <- .Call("card", nb)
    invisible(z)
}
