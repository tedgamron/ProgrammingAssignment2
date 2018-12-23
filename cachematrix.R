##makeCacheMatrix
##makeCacheMatrix creats a matrix 
##The first function, makeVector creates a special "vector", which is really a list containing a function to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set inverse via the  solve() function
## 4 get the inverse 

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


## cacheSolve
## cacheSolve caculates the inverse from makeCacheMatrix.  It first checks to see the inverse is
## stored in the enviroment before running the solve() fuction

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
