## makeCacheMatrix takes a square, invertible matrix and stores it along with it's inverse in cache.
## cacheSolve output is the inverse of a square,invertible matrix previously saved in cache. 

## makeCache takes a square, invertible matrix as input. Then it generates the space to save in cache the matrix and it's inverse. 
## The output is a list with the 4 functions that enable you to set an/or retrieve the input matrix and/or it's inverse in cache.

makeCacheMatrix <- function(x = matrix()) {
 y<-NULL
 s<-NULL
  set<-function(y){
    y<<-x
    s<<-NULL
   
  }
  
get<-function() x 
setinverse <- function(solve) s <<- solve
getinverse <- function() s
list(set= set, get= get,
     setinverse = setinverse,
     getinverse = getinverse)

}

}


## cacheSolve input is the makeCache output of a square invertible matrix. It's output is the inverse of that matrix. 
## However, if this inverse has already been calculated, then cacheSolve only gets it from cache, making the process faster, and telling that it did so ("getting cached data").

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data") 
    return(s)}
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setinverse(s)
  s

}
