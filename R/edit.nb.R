# Copyright 2001 by Roger Bivand and Nicholas Lewin-Koh
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

#edit.nb <- function(name, coords, polys=NULL, bbs=NULL, ...) {
#	nb <- name
#	if (class(nb) != "nb") stop("Not a neighbours list")
#	x <- coords[,1]
#	y <- coords[,2]
#	n <- length(nb)
#	row.names <- attr(nb, "region.id")
#	if (is.null(row.names)) row.names <- as.character(1:n)
#	xlim <- range(x)
#	ylim <- range(y)
#	plot.new()
#        plot.window(xlim = xlim, ylim = ylim, "", asp=1)
#	if (!is.null(polys) && !is.null(bbs))
#		plotpolys(polys,bbs, border="grey", add=T)
#	for (i in 1:n) {
#        	inb <- nb[[i]]
#        	for (j in inb)
#			lines(c(x[i], x[j]), c(y[i], y[j]),
#				col="black")
#	}
#	points(x, y)
#	finished <- "n"
#	deletions <- NULL
#	additions <- NULL
#	while (finished == "n") {
#		cat("Identifying contiguity for deletion ...\n")
#		cand <- identify(x, y, n=2)
#		lines(x[cand], y[cand], col="red")
#
#		if (.Platform$OS.type == "windows") bringToTop(-1)
#		if ((cand[2] %in% nb[[cand[1]]]) && (cand[1] %in% nb[[cand[2]]])) {
#						delete <- readline("Delete this line (y/n) ")
#			if (delete != "y") delete <- "n"
#			else {
#				deletions <- c(deletions, paste(cand,
#					collapse="-"))
#				nb[[cand[1]]] <- nb[[cand[1]]][nb[[cand[1]]] != cand[2]]
#				if(length(nb[[cand[1]]]) == 0) {
#					nb[[cand[1]]] <- 0
#					cat(cand[1], "is now an island\n")
#				}
#				nb[[cand[2]]] <- nb[[cand[2]]][nb[[cand[2]]] != cand[1]]
#				if(length(nb[[cand[2]]]) == 0) {
#					nb[[cand[2]]] <- 0
#					cat(cand[2], "is now an island\n")
#				}
#				cat("deleted contiguity between point",
#					cand[1], "and", cand[2], "\n")
#
# 			}
#			plot.new()
#	        	plot.window(xlim = xlim, ylim = ylim, "", asp=1)
#			if (!is.null(polys) && !is.null(bbs))
#				plotpolys(polys,bbs, border="grey", add=T)
#			for (i in 1:n) {
#	        		inb <- nb[[i]]
#	        		for (j in inb)
#					lines(c(x[i], x[j]), c(y[i], y[j]),
#						col="black")
#			}
#			points(x, y)
#		} else {
#			if (length(cand == 2)) {
#				cat("No contiguity between chosen points\n")
#				addcont <- readline("Add contiguity? (y/n) ")
#				if (addcont != "y") addcont <- "n"
#				if (addcont == "y") {
#					nb[[cand[1]]] <-
#					sort(unique(c(nb[[cand[1]]], cand[2])))
#					nb[[cand[2]]] <-
#					sort(unique(c(nb[[cand[2]]], cand[1])))
#					cat("added contiguity between point",
#					cand[1], "and", cand[2], "\n")
#					additions <- c(additions, paste(cand,
#					collapse="-"))
#
#				}
#				plot.new()
#	        		plot.window(xlim = xlim, ylim = ylim, "", asp=1)
#				if (!is.null(polys) && !is.null(bbs))
#				plotpolys(polys,bbs, border="grey", add=T)
#				for (i in 1:n) {
#	        			inb <- nb[[i]]
#	        			for (j in inb)
#						lines(c(x[i], x[j]),
#							c(y[i], y[j]),
#							col="black")
#				}
#				points(x, y)
#			}
#		}
#		finished <- readline("Finished yet? (y/n) ")
#		if (finished != "y") finished <- "n"
#	}
#	attributes(nb) <- list(deleted=deletions)
#	attr(nb, "added") <- additions
#	attr(nb, "region.id") <- row.names
#	class(nb) <- "nb"
#	nb <- sym.attr.nb(nb)
#	invisible(nb)
#}

edit.nb <- function(name, coords, polys=NULL, bbs=NULL, ...) {
  nb <- name
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
    #arrows(x[i],y[i],x[nb[[i]]],y[nb[[i]]],lenght=.08, angle=.15)
###
    segments(x[i],y[i],x[nb[[i]]],y[nb[[i]]])
###
    #inb <- nb[[i]]
    #for (j in inb)
    #lines(c(x[i], x[j]), c(y[i], y[j]), col="black")
  }
  points(x, y)
  finished <- "n"
  deletions <- NULL
  additions <- NULL
###
  edit.segs<-list()
  e.seg.stat<-NULL
  enum<-0
  erase.col<-par()$bg
###
  while (finished == "n") {
    cat("Identifying contiguity for deletion ...\n")
    cand <- identify(x, y, n=2)
    lines(x[cand], y[cand], col="red")

    if (.Platform$OS.type == "windows") bringToTop(-1)
    if ((cand[2] %in% nb[[cand[1]]]) && (cand[1] %in% nb[[cand[2]]])) {
      delete <- readline("Delete this line (y/n) ")
      if (delete != "y") delete <- "n"
      else {
        deletions <- c(deletions, paste(cand, collapse="-"))
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
###
        lines(x[cand], y[cand], col=erase.col)
        lines(x[cand], y[cand], col='brown',lty=4)
        enum<-enum+1
        edit.segs[[enum]]<-cand
        e.seg.stat<-c(e.seg.stat,0)
###
        cat("deleted contiguity between point", cand[1], "and", cand[2], "\n")
      }

      #plot.new()
      #plot.window(xlim = xlim, ylim = ylim, "", asp=1)
      #if (!is.null(polys) && !is.null(bbs))
      #  plotpolys(polys,bbs, border="grey", add=T)
      #for (i in 1:n) {
      #  inb <- nb[[i]]
      #  for (j in inb)
      #    lines(c(x[i], x[j]), c(y[i], y[j]),
      #          col="black")
      #}
      #points(x, y)
    }
      else {
        if (length(cand == 2)) {
          cat("No contiguity between chosen points\n")
          addcont <- readline("Add contiguity? (y/n) ")
          if (addcont != "y") addcont <- "n"
          if (addcont == "y") {
            nb[[cand[1]]] <-
              sort(unique(c(nb[[cand[1]]], cand[2])))
            nb[[cand[2]]] <-
              sort(unique(c(nb[[cand[2]]], cand[1])))
            cat("added contiguity between point",
                cand[1], "and", cand[2], "\n")
            additions <- c(additions, paste(cand, collapse="-"))
###
            enum<-enum+1
            edit.segs[[enum]]<-cand
            e.seg.stat<-c(e.seg.stat,1)
            lines(x[cand], y[cand], col='yellow')
###
          }
#          plot.new()
#          plot.window(xlim = xlim, ylim = ylim, "", asp=1)
#          if (!is.null(polys) && !is.null(bbs))
#            plotpolys(polys,bbs, border="grey", add=T)
#          for (i in 1:n) {
#            inb <- nb[[i]]
#            for (j in inb)
#              lines(c(x[i], x[j]),
#                    c(y[i], y[j]),
#                    col="black")
#				}
#          points(x, y)
        }
      }
#    finished <- readline("Finished yet? (y/n) ")
### 
    finished <- readline("Options: quit[q] refresh[r] continue[c] ")
    if (finished == "r") {
      plot.new()
      plot.window(xlim = xlim, ylim = ylim, "", asp=1)
      if (!is.null(polys) && !is.null(bbs))
        plotpolys(polys,bbs, border="grey", add=T)
      for (i in 1:n) {
        if(nb[[i]][1]!=0 & length(nb[[i]])>0)
          segments(x[i],y[i],x[nb[[i]]],y[nb[[i]]])
      }
      if(enum>1){
        for(i in 1:enum){
          if(e.seg.stat[i]==0){
            lines(x[edit.segs[[i]]], y[edit.segs[[i]]], col=erase.col)
            lines(x[edit.segs[[i]]], y[edit.segs[[i]]], col='brown',lty=4)
          }
          else lines(x[edit.segs[[i]]], y[edit.segs[[i]]], col='yellow')
        }
      }
      points(x, y)
      finished <- readline("Options: quit[q] continue[c]")
    }
    if (finished != "q") finished <- "n"
####
  }
  
  attributes(nb) <- list(deleted=deletions)
  attr(nb, "added") <- additions
  attr(nb, "region.id") <- row.names
  class(nb) <- "nb"
  nb <- sym.attr.nb(nb)
  invisible(nb)
}

