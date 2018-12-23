## Put comments here that give an overall description of what your
## functions do

## build the matrix
## create a matric 
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL  
        set <- function(y) {
                x <<- y  
                m <<- NULL  
        }
        get <- function() x
        setiv <- function(solve) m <<- solve  
        getiv <- function() m
        list(set = set, get = get,
             setiv = setiv,
             getiv = getiv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getiv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setiv(m)
        m
}
