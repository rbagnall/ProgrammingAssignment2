## cachematrix.R will calculate the inverse of a matrix
## The inverse of the matix will be cached
## The cached data can be recalled when needed again

## makeCacheMatrix function will calculate the inverse of a matrix
## The inverse will be stored (cached) in an environment that can be accessed later

makeCacheMatrix <- function(x = matrix()) { ## x must be an invertable matrix
    
        ## m is the cashed inverse
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        
        # solve function calculates the inverse of a matrix
        setinverse <- function(solve) m <<- solve
        
        # access the inverse with getinverse
        getinverse <- function() m
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## cacheSolve function will retrieve cached inverse of a matrix

cacheSolve <- function(x, ...) {
    
        # if m is not empty...get cached data
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m

}
