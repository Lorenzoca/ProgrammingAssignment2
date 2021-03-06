## The goal of the following functions is to store in cache
## the inverse of a matrix. The cached inverse can be re-used
## without the need of un-necessary computations


## This function takes a matrix and defines 
## a list of additional functions that calculate/store its inverse.

makeCacheMatrix <- function(x = matrix()){
        # initialization
        inv <- NULL

        # set re-defines x as a new matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get uses x as a matrix
        get <- function() x
        
        # puts the inverse in the cache
        setinv <- function(solve) inv <<- solve
        
        # getinv takes the inverse value 
        getinv <- function() inv
        
        # all the functions are returned
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The following function checks if the inverse of x 
## is already present in cache, otherwise computes it
## using the functions defined before.

cacheSolve <- function(x, ...) {
        
        # if the inverse is present in the cache, returns it
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # if not present in cache, uses x as a matrix...
        data <- x$get()
        #...computes its inverse...
        inv <- solve(data, ...)
        #...puts it in the cache...
        x$setinv(inv)
        #...and returns it!
        inv
}
