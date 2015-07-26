## This set of two functions either retrieves or computes a matrix inverse given matrix
## input. If the inverse has already been computed, then the function cacheSolve simply
## retrieves the inverse from memory (the cache). If the the inverse has not been
## computed in this R session, then the function cacheSolve uses R's built-in SOLVE to
## compute the inverse.
## USAGE:
## 
## makeCacheMatrix(x)
##
## Arguments:
##                x -- matrix, e.g. x <- matrix(rnorm(16),4,4)
##                                  m <- makeCacheMatrix(x)
## Output:
##                cached version of the matrix inverse
##
## cacheSolve(m)
##
## Arguments:
##                m -- matrix, e.g. m from cached matrix above
##                                  a <- cacheSolve(m)
## Output:
##                matrix inverse (computed using SOLVE or cached version from memory)

## Constructor for cached version of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Either compute the matrix inverse using SOLVE or retrieve cached version

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
