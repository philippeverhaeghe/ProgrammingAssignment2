## These 2 functions allow to compute an inverted matrix and cache the result
## in order to avoid re-computing if not necessary

## makeCacheMatrix instantiates a matrix and returns a list of functions
## to set, get, set the inverse, get the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {												
                x <<- y															# set new matrix
                m <<- NULL													# empty cache
        }
        get <- function() x 		        # returns the matrix
        setinv <- function(invm) m <<- invm     # cache inverted matrix
        getinv <- function() m 			# returns cached matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve checks if inverted matrix is in the cache and if true returns it 
## else it computes it, stores it in the cache and returns it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()         # query the inverted matrix
        if(!is.null(m)) {       # if there is an inverted matrix in the cache
                message("getting cached data")
                return(m)       # just return the cached inverted matrix
        }
        data <- x$get()         # if there is nothing in the cache
        m <- solve(data, ...)   # compute the inverted matrix
        x$setinv(m)             # save the result in the cache
        m                       # return the result
}
