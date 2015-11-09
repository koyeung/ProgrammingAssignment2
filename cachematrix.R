## These functions pair calculate and cache result of matrix inversion


## makeCacheMatrix: 
##     creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Input: 'x' invertible matrix
    
    ## Return list of functions to get/set matrix value and the inverse
    ## get() get the value of the matrix
    ## set() set the value of the matrix
    ## getinverse() get the value of the matrix inverse
    ## setinverse() set the value of the matrix inverse
    
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve:
##     This function computes the inverse of the special "matrix" returned by
##     makeCacheMatrix above. If the inverse has already been calculated 
##     (and the matrix has not changed), then the cachesolve should retrieve 
##     the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Input: 'x' cache matrix created by makeCacheMatrix()
    ##        '...' other arguments passing to solve() function
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
