# Copyright 2001 by Nicholas Lewin-Koh
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


union.nb<-function(nb.obj1, nb.obj2){
  if(!inherits(nb.obj1,"nb") | !inherits(nb.obj2,"nb")){
    stop("Both arguments must be of class nb")
  }
  if(any(attr(nb.obj1,"region.id")!= attr(nb.obj2,"region.id"))){
   stop("Both neighbor objects must be \n generated from the same coordinates")
  }
  new.nb<-list()
  for(i in 1:length(nb.obj1))
    new.nb[[i]]<-sort(union(nb.obj1[[i]], nb.obj2[[i]]))
  attr(new.nb,"region.id")<-attr(nb.obj1,"region.id")
  attr(new.nb,"type")<-paste("union(",attr(nb.obj1,"type"),
                             ",",attr(nb.obj2,"type"),")")
  class(new.nb)<-"nb"
  new.nb
 }

intersect.nb<-function(nb.obj1, nb.obj2){
  if(!inherits(nb.obj1,"nb") | !inherits(nb.obj2,"nb")){
    stop("Both arguments must be of class nb")
  }
  if(any(attr(nb.obj1,"region.id")!= attr(nb.obj2,"region.id"))){
   stop("Both neighbor objects must be \n generated from the same coordinates")
  }
  new.nb<-list()
  for(i in 1:length(nb.obj1))
    new.nb[[i]]<-sort(intersect(nb.obj1[[i]], nb.obj2[[i]]))
  attr(new.nb,"region.id")<-attr(nb.obj1,"region.id")
  attr(new.nb,"type")<-paste("intersect(",attr(nb.obj1,"type"),
                             ",",attr(nb.obj2,"type"),")")
  class(new.nb)<-"nb"
  new.nb
}
setdiff.nb<-function(nb.obj1, nb.obj2){
  if(!inherits(nb.obj1,"nb") | !inherits(nb.obj2,"nb")){
    stop("Both arguments must be of class nb")
  }
  if(any(attr(nb.obj1,"region.id")!= attr(nb.obj2,"region.id"))){
   stop("Both neighbor objects must be \n generated from the same coordinates")
  }
  new.nb<-list()
  for(i in 1:length(nb.obj1))
    new.nb[[i]]<-sort(setdiff(nb.obj1[[i]], nb.obj2[[i]]))
  attr(new.nb,"region.id")<-attr(nb.obj1,"region.id")
  attr(new.nb,"type")<-paste("setdiff(",attr(nb.obj1,"type"),
                             ",",attr(nb.obj2,"type"),")")
  class(new.nb)<-"nb"
  new.nb
}
complement.nb<-function(nb.obj){
   if(!inherits(nb.obj,"nb")){
    stop("Argument must be of class nb")
   }
  cmp<-1:length(nb.obj)
  new.nb<-list()
  attributes(new.nb)<-attributes(nb.obj)
  for(i in 1:length(nb.obj))
    new.nb[[i]]<-sort(cmp[-nb.obj[[i]]])
  attr(new.nb,"type")<-paste("complement(",attr(nb.obj,"type"),")")
  class(new.nb)<-"nb"
  new.nb
 }
