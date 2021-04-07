"multignome" <-
function(model,data){
  # a wrapper for the real workhorse functions.
  if(class(model)!="list") stop("model must be of class list")
  theGnome <- combineFrames(model,data)       # Prepares model
  gnome(theGnome)                             # Optimisation
}

