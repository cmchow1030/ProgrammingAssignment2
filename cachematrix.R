## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y #set for a new matrix
                i <<- NULL # Reset the inverse when the matrix changes
        }
        get <- function() x #get the matrix
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv) #return a list of the above functions
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("Getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
        ## Return a matrix that is the inverse of 'x'
}


