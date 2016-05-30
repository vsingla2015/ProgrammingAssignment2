makeCacheMatrix <- function(x = matrix()) {
inv <-NULL
setMatrix<-function(mat) {
m<<-mat 
inv<<-NULL
}
getMatrix<-function() m % get matrix function
setInverse<-function(temp) inv<<-temp % set Inverse function
getInverse<-function() inv  % get inverse function
list(setMatrix=setMatrix, getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
}
cacheSolve <- function(s,…) { % solve function to calculate inverse
inv<-s$getInverse()
if (!is.null(inv)){ % checking for inverse function being null or not
message("getting cached data")
return(inv)
}
data <- s$getMatrix()
inv<-solve(data,…)
s$setInverse(inv)
inv
}
