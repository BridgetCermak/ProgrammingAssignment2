##Pair of functions that cache the inverse of a matrix
##Note: the matrix supplied must always be invertible

## Function 1 : creates a special matrix object that can cache
##its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- Null
        }
      get <- function()x
      setinv <- function(solve) m <<- solve
      getinv <- function()m
      list(set=set,
           get=get,
           setinv = setinv,
           getinv = getinv)
}


## Function 2: computes the inverse of the matrix returned from makeCacheMatrix
##function above.If the inverse has already been calculated 
##(and matrix has not changed), the n cachesolve will retrieve
##the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
                if (!is.null(m)){
                message("getting cached data")
                return(m)
          }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
