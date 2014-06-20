## function makeCacheMatrix :
## 		Makes a matrix special, in that it returns a list functions
## 		to help cache it's inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## function cacheSolve:
##      consumes the special matrix created by makeCacheMatrix function.
##		Looks at the environment that defined the matrix 
##      to see if there is a previously cached version of the inverse matrix
##      else, calculates and sets and returns the new inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(! is.null(i)) {
        message("getting cached inverse of the matrix")
            return(i)
        }
        mat <- x$get()
        i <- solve(mat,...)
        x$setinv(i)
        i

}
