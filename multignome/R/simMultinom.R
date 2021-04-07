"simMultinom" <-
function(probV,n,size){
  # function to pass to apply
  rmultinom(n,size,probV)
}

